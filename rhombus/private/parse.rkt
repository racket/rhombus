#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "op.rkt"
                     "transformer.rkt"))

(provide rhombus-top
         rhombus-block
         rhombus-expression

         (for-syntax :operator
                     :declaration
                     :definition
                     :expression
                     :pattern))

(begin-for-syntax
  (define-syntax-class :operator
    (pattern ((~datum op) name) #:attr opname #'name))

  ;; Things that can appear at the top of a module:
  (define-syntax-class :declaration
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-declaration-transformer? v)
             #:attr expandeds (apply-declaration-transformer v (cons #'head #'tail))))

  ;; Things that can appear in a definition context:
  (define-syntax-class :definition
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-definition-transformer? v)
             #:do [(define-values (defns-and-exprs exprs)
                     (apply-definition-transformer v (cons #'head #'tail)))]
             #:attr expandeds (datum->syntax #f defns-and-exprs)
             #:attr exprs (datum->syntax #f exprs)))

  ;; Things that can appear in an expression context:
  (define-syntax-class :expression
    (pattern ((~datum group) . tail) #:attr expanded (enforest #'tail)))

  ;; Things that can appear in a pattern context:
  (define-syntax-class :pattern
    (pattern ((~datum group) . tail)
             #:do [(define expanded (enforest-pattern #'tail))]
             #:attr bindings (datum->syntax #f (stx-car expanded))
             #:attr filter (stx-car (stx-cdr expanded))))

  ;; The `enforest` functions below are based on the one described in
  ;; figure 1 of "Honu: Syntactic Extension for Algebraic Notation
  ;; through Enforestation". Some key differences:
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
  ;;   and combiner should be pushed onto the stack, as here.
  ;;
  ;; Also, operator precedence is not based on numerical levels or
  ;; even a transitive order. Instead, each operator can declare an
  ;; order relative to specific other operators, and an error is
  ;; reported if two operators must be compared fr precedence and have
  ;; no declared order.
  ;;
  ;; Terminology compared to the paper: "form" means "tree term", and
  ;; "stx" means "term". A "head" or "tail" is a stx/term.
  ;;
  ;; The "tree term" for pattern enforestation is a syntactic list of
  ;; length 2, where the first part is a list of N identifiers bound
  ;; by the pattern and the second part is an expression that acts as
  ;; a function from the argument to a boolean (true implies matching)
  ;; and N values (that are ignored if the first result is false).
  ;; Putting those two pieces in one syntax object just makes the
  ;; `enforest` code work easily for both modes, parameterized over
  ;; whether it's in pattern mode.

  ;; implicit operator names:
  (define juxtipose-name '#%juxtipose) ; exprs with no operator between, when not #%call or #%ref
  (define tuple-name '#%tuple)         ; parentheses not after an expression
  (define call-name '#%call)           ; parentheses adjacent to preceding expression
  (define array-name '#%array)         ; square brackets not after an expression
  (define ref-name '#%ref)             ; square brackets adjacent to preceding expression

  ;; implicit form names:
  (define block-name '#%block)         ; curly braces (normally mapped to `rhombus-block`)

  (define-syntax-rule (define-enforest enforest
                        pattern?
                        ;; the rest are named as if they're for expressions,
                        ;; but they can be replaced by functions for patterns
                        rhombus-infix-operator?
                        rhombus-infix-operator-transformer?
                        rhombus-prefix-operator?
                        rhombus-prefix-operator-transformer?
                        rhombus-expression-transformer?
                        apply-expression-transformer
                        make-identifier-expression
                        make-datum-expression)
    (define (enforest stxes)
      (let loop ([init-form #f]
                 [stxes stxes]
                 [combine (lambda (form) form)] [current-op #f] [stack '()])
        ;; helper to continue with an implicitly combined argument:
        (define (keep form/s tail implicit-combine)
          (loop (implicit-combine init-form form/s)
                tail
                combine current-op stack))
        ;; helper to apply an implicit-combine transformer:
        (define (implicit-transform implicit-combine stx)
          (define-values (form tail) (apply-infix-operator-transformer implicit-combine init-form stxes stx
                                                                       #:pattern? pattern?))
          (loop form tail
                combine current-op stack))
        ;; helper to selected among the above two helpers
        (define (implicit-or-keep to-keep-thunk stx tail implicit-combine)
          (cond
            [(rhombus-infix-operator-transformer? implicit-combine)
             ;; `init-form` must be non-#f here
             (implicit-transform implicit-combine stx)]
            [(rhombus-prefix-operator-transformer? implicit-combine)
             ;; `init-form` must be #f here
             (define-values (form tail) (apply-prefix-operator-transformer implicit-combine stxes stx
                                                                           #:pattern? pattern?))
             (loop form tail
                   combine current-op stack)]
            [else
             (define r (to-keep-thunk))
             (keep r tail implicit-combine)]))
        ;; helper to handle a sequence of expressions:
        (define (enforest-sequence stx)
          (syntax-parse stx 
            [() '()]
            [(((~datum group) . e) . tail)
             (cons (enforest #'e) (enforest-sequence #'tail))]))
        ;; dispatch on the head token
        (syntax-parse stxes
          [()
           ;; input stream ended, so use up the combiner stack
           (cond
             [(not init-form) (raise-syntax-error #f "empty expression")]
             [(null? stack) (combine init-form)]
             [else (loop (combine init-form) stxes (caar stack) (cdar stack) (cdr stack))])]
          [(head::operator . tail)
           (define v (syntax-local-value #'head.opname (lambda () #f)))
           (cond
             [(and (rhombus-infix-operator? v)
                   (or init-form
                       (not (rhombus-prefix-operator? v))))
              (unless init-form
                (raise-syntax-error #f "infix operator without preceding argument" #'head))
              (define rel-prec (if (not current-op)
                                   'higher
                                   (relative-precedence v current-op #'head)))
              (cond
                [(or (not current-op) (eq? rel-prec 'higher))
                 (cond
                   [(rhombus-infix-operator-transformer? v)
                    ;; it's up to the transformer to consume whatever it wants after the operator
                    (define-values (form new-tail) (apply-infix-operator-transformer v init-form #'tail #'head
                                                                                     #:pattern? pattern?))
                    (loop form new-tail combine current-op stack)]
                   [else
                    ;; new operator sets precedence, defer application of operator until a suitable rhs is parsed
                    (loop #f #'tail
                          (lambda (form) (apply-infix-operator v init-form form #'head
                                                               #:pattern? pattern?))
                          v
                          (cons (cons combine current-op) stack))])]
                [(eq? rel-prec 'lower)
                 (unless (pair? stack) (error 'internal "empty enforest stack"))
                 ;; combine for current operator, then try again
                 (loop (combine init-form) stxes
                       (caar stack) (cdar stack) (cdr stack))]
                [else
                 (cond
                   [(eq? rel-prec 'same)
                    (raise-syntax-error #f
                                        "non-associative operator needs explicit parenthesization"
                                        #'head.opname)]
                   [else
                    (raise-syntax-error #f
                                        (format
                                         (string-append 
                                          "combination of ~aoperators without declared relative precedence"
                                          " needs explicit parenthesization\n"
                                          "  other operator: ~a")
                                         (if pattern? "pattern " "")
                                         (syntax-e (rhombus-operator-name current-op #:pattern? pattern?)))
                                        #'head.opname)])])]
             [(rhombus-prefix-operator? v)
              (define implicit-combine (lookup-implicit-combine init-form #f juxtipose-name #'head.opname
                                                                #:pattern? pattern?))
              (cond
                [(rhombus-infix-operator-transformer? implicit-combine)
                 ;; it's up to the transformer to figure out what to do with the operator
                 (implicit-transform implicit-combine #'head.opname)]
                [(rhombus-prefix-operator-transformer? v)
                 ;; it's up to the transformer to consume whatever it wants after the operator
                 (define-values (form new-tail) (apply-prefix-operator-transformer v #'tail #'head
                                                                                   #:pattern? pattern?))
                 (keep form new-tail implicit-combine)]
                [else
                 ;; new operator sets precedence, defer application of operator until a suitable argument is parsed
                 (loop #f #'tail
                       (lambda (form) (implicit-combine init-form (apply-prefix-operator v form #'head
                                                                                         #:pattern? pattern?)))
                       v
                       (cons (cons combine current-op) stack))])]
             [else
              (raise-syntax-error #f
                                  (if pattern? "unbound pattern operator" "unbound operator")
                                  #'head.name)])]
          [(head:identifier . tail)
           (define implicit-combine (lookup-implicit-combine init-form #f juxtipose-name #'head
                                                             #:pattern? pattern?))
           (cond
             [(rhombus-infix-operator-transformer? implicit-combine)
              (implicit-transform implicit-combine #'head)]
             [else
              (define v (syntax-local-value #'head (lambda () #f)))
              (cond
                [(rhombus-expression-transformer? v)
                 (define-values (form tail) (apply-expression-transformer v stxes))
                 (keep form tail implicit-combine)]
                [else
                 (keep (make-identifier-expression #'head) #'tail implicit-combine)])])]
          [(((~and tag (~datum parens)) . inside) . tail)
           (define implicit-combine (lookup-implicit-combine init-form tuple-name call-name #'tag
                                                             #:multi? #t #:pattern? pattern?))
           (implicit-or-keep (lambda () (enforest-sequence #'inside)) #'tag #'tail implicit-combine)]
          [(((~and tag (~datum braces)) . inside) . tail)
           (define implicit-combine (lookup-implicit-combine init-form array-name ref-name #'tag
                                                             #:multi? #t #:pattern? pattern?))
           (implicit-or-keep (lambda () (enforest-sequence #'inside)) #'tag #'tail implicit-combine)]
          [(((~and tag (~datum block)) . inside) . tail)
           (define implicit-combine (lookup-implicit-combine #f init-form juxtipose-name #'tag
                                                             #:pattern? pattern?))
           (define (make-block-form)
             (define stx #'tag)
             (datum->syntax #f (cons (datum->syntax stx block-name stx stx) #'inside) stx stx))
           (implicit-or-keep make-block-form #'tag #'tail implicit-combine)]
          [(((~and tag (~datum alts)) . inside) . tail)
           (raise-syntax-error #f
                               "misplaced alternative"
                               (stx-car stxes)
                               #'tag)]
          [(datum . tail)
           (define implicit-combine (lookup-implicit-combine init-form #f juxtipose-name #'datum
                                                             #:pattern? pattern?))
           (implicit-or-keep (lambda () (make-datum-expression #'datum)) #'datum #'tail implicit-combine)]))))

  ;; the expression variant:
  (define-enforest enforest #f
    rhombus-infix-operator?
    rhombus-infix-operator-transformer?
    rhombus-prefix-operator?
    rhombus-prefix-operator-transformer?
    rhombus-expression-transformer?
    apply-expression-transformer
    make-identifier-expression
    make-datum-expression)

  ;; the pattern variant:
  (define-enforest enforest-pattern #t
    rhombus-infix-pattern-operator?
    rhombus-infix-pattern-operator-transformer?
    rhombus-prefix-pattern-operator?
    rhombus-prefix-pattern-operator-transformer?
    rhombus-pattern-transformer?
    apply-pattern-transformer
    make-identifier-pattern
    make-datum-pattern)

  ;; helper functions for expressions
  (define (make-identifier-expression id)
    id)
  (define (make-datum-expression datum)
    (datum->syntax datum (cons #'#%datum datum) datum datum))

  ;; helper functions for patterns
  (define (make-identifier-pattern id)
    (list (list id) #'(lambda (v) (values #t v))))
  (define (make-datum-pattern datum)
    (list null #'(lambda (v) (values (equal? v datum))))))

;; For a module top level, interleaves expansion and enforestation:
(define-syntax (rhombus-top stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ form . forms)
     #`(begin
         #,(syntax-parse #'form
             [e::declaration #'(begin . e.expandeds)]
             [e::definition #'(begin (begin . e.expandeds) . e.exprs)]
             [e::expression #'e.expanded])
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
          (begin . e.exprs)
          #,(syntax/loc stx
              (rhombus-block . tail))))]
    [(_ e::expression . tail)
     #`(maybe-begin
        e.expanded
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
