#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "op.rkt"
                     (submod "op.rkt" for-parse)
                     "transformer.rkt"
                     (submod "transformer.rkt" for-parse)))

(provide rhombus-top
         rhombus-block
         rhombus-expression

         (for-syntax :operator
                     :declaration
                     :definition
                     :expression
                     :binding
                     :binding-form

                     ;; for continuing enforestation of expressions or bindings:
                     :op+expression+tail
                     :op+binding+tail))

(begin-for-syntax
  (define-syntax-class :operator
    (pattern ((~datum op) name)))

  ;; Form at the top of a module:
  (define-syntax-class :declaration
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-declaration-transformer? v)
             #:attr expandeds (apply-declaration-transformer v (cons #'head #'tail))))

  ;; Form in a definition context:
  (define-syntax-class :definition
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-definition-transformer? v)
             #:do [(define-values (defns-and-exprs exprs)
                     (apply-definition-transformer v (cons #'head #'tail)))]
             #:attr expandeds (datum->syntax #f defns-and-exprs)
             #:attr exprs (datum->syntax #f exprs)))

  ;; Form in an expression context:
  (define-syntax-class :expression
    (pattern ((~datum group) . tail) #:attr expanded (enforest #'tail #f)))

  ;; For reentering the enforestation loop within a group, stopping when
  ;; the group end or when an operator with weaker precedence than `op` is found
  (define-splicing-syntax-class :op+expression+tail
    (pattern (op:identifier . tail)
             #:do [(define-values (form new-tail) (enforest-step (syntax-local-value #'op) #'tail))]
             #:attr expanded form
             #:attr new-tail new-tail))

  ;; Form in a binding context:
  (define-syntax-class :binding
    (pattern ((~datum group) . tail)
             #:with (variable-ids matcher-form syntax-ids syntax-form) (enforest-binding #'tail)))
  ;; To re-unpack a `:binding` result:
  (define-syntax-class :binding-form
    (pattern (variable-ids matcher-form syntax-ids syntax-form)))

  ;; Like `:op+expression+tail`, but for bindings
  (define-splicing-syntax-class :op+binding+tail
    (pattern (op:identifier . tail)
             #:do [(define-values (form new-tail) (enforest-binding-step (syntax-local-value #'op) #'tail))]
             #:with (variable-ids matcher syntax-ids syntax-rhs) form
             #:attr new-tail new-tail))

  ;; The `enforest` functions below are based on the one described in
  ;; figure 1 of "Honu: Syntactic Extension for Algebraic Notation
  ;; through Enforestation". Some key differences:
  ;;
  ;; * The stack of pending operators is represented through the
  ;;   continuation, instead of an explicit stack. The `enforest-step`
  ;;   functions provide a way to reenter enforestation given a
  ;;   preceding operator (for precedence), which is useful for
  ;;   operator transformers. (All operators could be implemented as
  ;;   transformers, but we provide support for non-transformer
  ;;   operators as a convenience and as a way to understand the
  ;;   implementation.)
  ;;
  ;; * A prefix or infix operator has an associated combiner procedure
  ;;   to produce a Racket expression form, instead of always making a
  ;;   `bin` or `un` AST node.
  ;;
  ;; * A prefix or infix operator can be bound to a transformer, in
  ;;   which case all parsing for the (second) argument is up to the
  ;;   transformer. A prefix operator as a transformer acts just like
  ;;   a transformer bound to an identifier, while an infix operator
  ;;   as transformer is useful for something like `.` (where the
  ;;   second "argument" is not an expression).
  ;;
  ;; * Function calls, array references, and list construction are not
  ;;   quite built-in. Instead, those positions correspond to the use
  ;;   of implicit operators, such as `#%call`. Many of these
  ;;   operators are special because they're N-ary (e.g., `f(1,2)` has
  ;;   three "arguments" `f`, `1`, and `2`), so they use a special
  ;;   protocol as non-transformer operators, but they can follow the
  ;;   regular protocol as transformers.
  ;;
  ;; * The paper's prefix-operator case seems wrong; the old operator
  ;;   and combiner should be pushed onto the stack, as reflected by a
  ;;   recursive call here.
  ;;
  ;; * Operator precedence is not based on numerical levels or even a
  ;;   transitive order. Instead, each operator can declare an order
  ;;   relative to specific other operators, and an error is reported
  ;;   if two operators must be compared fr precedence and have no
  ;;   declared order. See "op.rkt" for more on precedence.
  ;;
  ;; Terminology compared to the paper: "form" means "tree term", and
  ;; "stx" means "term". A "head" or "tail" is a stx/term.
  ;;
  ;; The "tree term" for binding enforestation is a syntactic list of
  ;; length 4 to hold the results of a binding transformer. Putting
  ;; those the pieces in one syntax object is a little awkward, but it
  ;; makes the `enforest` code work easily for both modes,
  ;; parameterized over whether it's in binding mode.

  ;; implicit prefix operator names:
  (define tuple-name   '#%tuple)       ; parentheses not after an expression
  (define array-name   '#%array)       ; square brackets not after an expression
  (define block-name   '#%block)       ; curly braces (normally mapped to `rhombus-block`)
  (define alts-name    '#%alts)        ; vertical bars
  (define literal-name '#%literal)     ; numbers, strings, etc.

  ;; implicit infix operator names:
  (define call-name      '#%call)      ; parentheses adjacent to preceding expression
  (define ref-name       '#%ref)       ; square brackets adjacent to preceding expression
  (define juxtapose-name '#%juxtapose) ; exprs with no operator between, when not #%call or #%ref

  (define-syntax-rule (where expr helper ...) (begin helper ... expr))

  (define-syntax-rule (define-enforest enforest enforest-step
                        binding?
                        ;; the rest are named as if they're for expressions,
                        ;; but they can be replaced by functions for bindings
                        rhombus-infix-operator?
                        rhombus-infix-operator-transformer?
                        rhombus-prefix-operator?
                        rhombus-prefix-operator-transformer?
                        rhombus-expression-transformer?
                        apply-expression-transformer
                        make-identifier-expression)
    (begin
      (define (enforest stxes [current-op #f])
        ;; either `stxes` starts with a prefix operator or this first step
        ;; will dispatch to a suitable implicit prefix operator
        (define-values (form tail) (enforest-step stxes current-op))
        (let loop ([init-form form] [stxes tail])
          (cond
            [(stx-null? stxes) init-form]
            [else
             ;; either `stxes` starts with an infix operator (which was weaker
             ;; precedence than consumed in the previous step), or this step will
             ;; dispatch to a suitable implicit infix operator, like `#%juxtapose`
             (define-values (form tail) (enforest-step init-form stxes current-op))
             (loop form tail)])))

      (define (raise-unbound-operator op-stx)
        (raise-syntax-error #f
                            (if binding? "unbound binding operator" "unbound operator")
                            op-stx))

      ;; Takes 2 or 3 arguments, depending on whether a preceding expression is available
      (define enforest-step
        (case-lambda
          [(stxes current-op)
           ;; No preceding expression, so dispatch to prefix (possibly implicit)
           ((syntax-parse stxes
              [() (raise-syntax-error #f "empty expression")]
              [(head::operator . tail)
               (define v (syntax-local-value #'head.name (lambda () #f)))
               (cond
                 [(rhombus-prefix-operator? v)
                  (dispatch-prefix-operator v #'tail #'head.name)]
                 [(rhombus-infix-operator? v)
                  (raise-syntax-error #f "infix operator without preceding argument" #'head.name)]
                 [else
                  (raise-unbound-operator #'head.name)])]
              [(head:identifier . tail)
               (define v (syntax-local-value #'head (lambda () #f)))
               (cond
                 [(rhombus-expression-transformer? v)
                  (apply-expression-transformer v stxes)]
                 [else
                  (enforest-step (make-identifier-expression #'head) #'tail current-op)])]
              [(((~and tag (~datum parens)) . inside) . tail)
               (dispatch-prefix-implicit tuple-name #'tag)]
              [(((~and tag (~datum braces)) . inside) . tail)
               (dispatch-prefix-implicit array-name #'tag)]
              [(((~and tag (~datum block)) . inside) . tail)
               (dispatch-prefix-implicit block-name #'tag)]
              [(((~and tag (~datum alts)) . inside) . tail)
               (dispatch-prefix-implicit alts-name #'tag)]
              [(literal . tail)
               (dispatch-prefix-implicit literal-name #'literal)])

            . where .

            (define (dispatch-prefix-operator v tail op-stx)
              (cond
                [(rhombus-prefix-operator-transformer? v)
                 ;; it's up to the transformer to consume whatever it wants after the operator
                 (define-values (form new-tail) (apply-prefix-operator-transformer v stxes #:binding? binding?))
                 (enforest-step form new-tail current-op)]
                [else
                 ;; new operator sets precedence, defer application of operator until a suitable
                 ;; argument is parsed
                 (define-values (form new-tail) (enforest-step tail v))
                 (enforest-step (apply-prefix-operator v form op-stx #:binding? binding?)
                                new-tail
                                current-op)]))

            (define (dispatch-prefix-implicit implicit-name head-stx)
              (define-values (implicit-v op-stx) (lookup-prefix-implicit implicit-name head-stx
                                                                         #:binding? binding?))
              (dispatch-prefix-operator implicit-v stxes op-stx)))]

          [(init-form stxes current-op)
           ;; Has a preceding expression, so dispatch to infix (possibly implicit)
           ((syntax-parse stxes
              [() (values init-form stxes)]
              [(head::operator . tail)
               (define v (syntax-local-value #'head.name (lambda () #f)))
               (cond
                 [(rhombus-infix-operator? v)
                  (dispatch-infix-operator v #'tail #'head.name)]
                 [(rhombus-prefix-operator? v)
                  (dispatch-infix-implicit juxtapose-name #'head)]
                 [else
                  (raise-unbound-operator #'head.name)])]
              [(head:identifier . tail)
               (dispatch-infix-implicit juxtapose-name #'head)]
              [(((~and tag (~datum parens)) . inside) . tail)
               (dispatch-infix-implicit call-name #'tag)]
              [(((~and tag (~datum braces)) . inside) . tail)
               (dispatch-infix-implicit ref-name #'tag)]
              [(((~and tag (~datum block)) . inside) . tail)
               (dispatch-infix-implicit juxtapose-name #'tag)]
              [(((~and tag (~datum alts)) . inside) . tail)
               (dispatch-infix-implicit juxtapose-name #'tag)]
              [(literal . tail)
               (dispatch-infix-implicit juxtapose-name #'literal)])

            . where . 

            (define (dispatch-infix-operator v tail op-stx)
              (define rel-prec (if (not current-op)
                                   'stronger
                                   (relative-precedence v current-op op-stx #:binding? binding?)))
              (cond
                [(eq? rel-prec 'stronger)
                 (cond
                   [(rhombus-infix-operator-transformer? v)
                    ;; it's up to the transformer to consume whatever it wants after the operator
                    (define-values (form new-tail) (apply-infix-operator-transformer v init-form stxes #:binding? binding?))
                    (enforest-step form new-tail current-op)]
                   [else
                    ;; new operator sets precedence, defer application of operator until a suitable
                    ;; right-hand argument is parsed
                    (define-values (form new-tail) (enforest-step tail v))
                    (enforest-step (apply-infix-operator v init-form form op-stx #:binding? binding?)
                                   new-tail
                                   current-op)])]
                [(eq? rel-prec 'weaker)
                 (values init-form stxes)]
                [else
                 (cond
                   [(eq? rel-prec 'same)
                    (raise-syntax-error #f
                                        "non-associative operator needs explicit parenthesization"
                                        op-stx)]
                   [else
                    (raise-syntax-error #f
                                        (format
                                         (string-append 
                                          "combination of ~aoperators without declared relative precedence"
                                          " needs explicit parenthesization\n"
                                          "  other operator: ~a")
                                         (if binding? "binding " "")
                                         (syntax-e (rhombus-operator-name current-op #:binding? binding?)))
                                        op-stx)])]))

            (define (dispatch-infix-implicit implicit-name head-stx)
              (define-values (implicit-v op-stx) (lookup-infix-implicit implicit-name head-stx
                                                                        #:binding? binding?))
              (dispatch-infix-operator implicit-v stxes op-stx)))]))))

  ;; the expression variant:
  (define-enforest enforest enforest-step #f
    rhombus-infix-operator?
    rhombus-infix-operator-transformer?
    rhombus-prefix-operator?
    rhombus-prefix-operator-transformer?
    rhombus-expression-transformer?
    apply-expression-transformer
    make-identifier-expression)

  ;; the binding variant:
  (define-enforest enforest-binding enforest-binding-step #t
    rhombus-infix-binding-operator?
    rhombus-infix-binding-operator-transformer?
    rhombus-prefix-binding-operator?
    rhombus-prefix-binding-operator-transformer?
    rhombus-binding-transformer?
    apply-binding-transformer
    make-identifier-binding)

  ;; helper function for expressions
  (define (make-identifier-expression id)
    id)

  ;; helper function for bindings
  (define (make-identifier-binding id)
    (list (list id) #'(lambda (v) (values #t v)) null #'(values))))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ form . forms)
     #`(begin
         #,(syntax-parse #'form
             [e::declaration #'(begin . e.expandeds)]
             [e::definition #'(begin (begin . e.expandeds) . e.exprs)]
             [e::expression #'(#%expression e.expanded)])
         (rhombus-top . forms))]))

;; For a definition context, interleaves expansion and enforestation:
(define-syntax (rhombus-block stx)
  (syntax-parse stx
    [(_)
     (raise-syntax-error #f "found an empty block" stx)]
    [(_ e::definition . tail)
     (when (and (stx-null? #'tail)
                (stx-null? #'e.exprs))
       (raise-syntax-error #f "block does not end with an expression" stx))
     #`(begin
         (begin . e.expandeds)
         (maybe-begin
          (#%expression (begin . e.exprs))
          #,(syntax/loc stx
              (rhombus-block . tail))))]
    [(_ e::expression . tail)
     #`(maybe-begin
        (#%expression e.expanded)
        #,(syntax/loc stx
            (rhombus-block . tail)))]))

(define-syntax (maybe-begin stx)
  (syntax-parse stx
    [(_ e (_)) #'e]
    [(_ e1 (_ . _)) #'(begin e1 e2)]))

;; For an expression context:
(define-syntax (rhombus-expression stx)
  (syntax-parse stx
    [(_ e::expression) #'e.expanded]))
