#lang racket/base
(require syntax/parse
         syntax/stx
         "op.rkt"
         (submod "op.rkt" for-parse)
         "transformer.rkt"
         (submod "transformer.rkt" for-parse)
         "syntax-local.rkt"
         "operator-parse.rkt")

(provide define-enforest)

;; The `enforest` function generated below is based on the one
;; described in figure 1 of "Honu: Syntactic Extension for Algebraic
;; Notation through Enforestation". Some key differences:
;;
;; * The stack of pending operators is represented through the
;;   continuation, instead of an explicit stack. The `enforest-step`
;;   function and `:...-op+form+tail` syntax classes provide a way to
;;   reenter enforestation given a preceding operator (for
;;   precedence), which is useful for operator transformers. All
;;   operators could be implemented as transformers, but we provide
;;   support for non-transformer operators as a convenience and as a
;;   way to understand the implementation.
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
;;   second "argument" is not an expression). Note that an infix
;;   transformer that doesn't consume further input is effectively
;;   a postfix operator.
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
                      :form :prefix-op+form+tail :infix-op+form+tail
                      form-kind-str operator-kind-str
                      in-space
                      prefix-operator-ref infix-operator-ref
                      check-result
                      make-identifier-form)
  (begin
    (define-syntax-class :form
      (pattern ((~datum group) . tail) #:attr expanded (transform-out (enforest (transform-in #'tail) #f))))

    ;; For reentering the enforestation loop within a group, stopping when
    ;; the group ends or when an operator with weaker precedence than `op`
    ;; is found
    (define-splicing-syntax-class :prefix-op+form+tail
      (pattern (op-name:identifier . tail)
               #:do [(define op (prefix-operator-ref (syntax-local-value* (in-space #'op-name)
                                                                          prefix-operator-ref)))
                     (define-values (form new-tail) (enforest-step op (transform-in #'tail)))]
               #:attr expanded (transform-out form)
               #:attr new-tail (transform-out new-tail)))
    (define-splicing-syntax-class :infix-op+form+tail
      (pattern (op-name:identifier . tail)
               #:do [(define op (infix-operator-ref (syntax-local-value* (in-space #'op-name)
                                                                         infix-operator-ref)))
                     (define-values (form new-tail) (enforest-step op (transform-in #'tail)))]
               #:attr expanded (transform-out form)
               #:attr new-tail (transform-out new-tail)))

    (define enforest-step (make-enforest-step form-kind-str operator-kind-str
                                              in-space
                                              prefix-operator-ref infix-operator-ref
                                              check-result
                                              make-identifier-form))
    (define enforest (make-enforest enforest-step))))

(define (make-enforest enforest-step)
  (lambda (stxes [current-op #f])
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
         (loop form tail)]))))

(define (make-enforest-step form-kind-str operator-kind-str
                            in-space
                            prefix-operator-ref infix-operator-ref
                            check-result
                            make-identifier-form)
  (define (raise-unbound-operator op-stx)
    (raise-syntax-error #f
                        (string-append "unbound " operator-kind-str)
                        op-stx))
  
  ;; Takes 2 or 3 arguments, depending on whether a preceding expression is available
  (define enforest-step
    (case-lambda
      [(stxes current-op)
       ;; No preceding expression, so dispatch to prefix (possibly implicit)
       ((syntax-parse stxes
          [() (raise-syntax-error #f (format "empty ~a" form-kind-str))]
          [(head::operator . tail)
           (define head-id (in-space #'head.name))
           (define v (syntax-local-value* head-id
                                          (lambda (v) (or (prefix-operator-ref v)
                                                          (infix-operator-ref v)))))
           (cond
             [(prefix-operator-ref v)
              => (lambda (op)
                   (dispatch-prefix-operator v op #'tail head-id))]
             [(infix-operator-ref v)
              (raise-syntax-error #f "infix operator without preceding argument" #'head.name)]
             [(identifier? #'head)
              (enforest-step (make-identifier-form #'head) #'tail current-op)]
             [else
              (raise-unbound-operator #'head.name)])]
          [(((~datum parsed) inside) . tail)
           (enforest-step #'inside #'tail current-op)]
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

        (define (dispatch-prefix-operator v op tail op-stx)
          (cond
            [(operator-transformer? op)
             ;; it's up to the transformer to consume whatever it wants after the operator
             (define-values (form new-tail) (apply-prefix-transformer-operator op op-stx stxes check-result))
             (enforest-step form new-tail current-op)]
            [else
             ;; new operator sets precedence, defer application of operator until a suitable
             ;; argument is parsed
             (define-values (form new-tail) (enforest-step tail v))
             (enforest-step (apply-prefix-direct-operator op form op-stx check-result)
                            new-tail
                            current-op)]))

        (define (dispatch-prefix-implicit implicit-name head-stx)
          (define-values (v op op-stx) (lookup-prefix-implicit implicit-name head-stx in-space
                                                               prefix-operator-ref
                                                               operator-kind-str form-kind-str))
          (dispatch-prefix-operator v op stxes op-stx)))]

      [(init-form stxes current-op)
       ;; Has a preceding expression, so dispatch to infix (possibly implicit)
       ((syntax-parse stxes
          [() (values init-form stxes)]
          [(head::operator . tail)
           (define head-id (in-space #'head.name))
           (define v (syntax-local-value* head-id
                                          (lambda (v) (or (infix-operator-ref v)
                                                          (prefix-operator-ref v)))))
           (cond
             [(infix-operator-ref v)
              => (lambda (op)
                   (dispatch-infix-operator v op #'tail head-id))]
             [(prefix-operator-ref v)
              (dispatch-infix-implicit juxtapose-name #'head)]
             [(identifier? #'head)
              (dispatch-infix-implicit juxtapose-name #'head)]
             [else
              (raise-unbound-operator #'head.name)])]
          [(((~datum parsed) . _) . _)
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

        (define (dispatch-infix-operator v op tail op-stx)
          (define rel-prec (if (not current-op)
                               'stronger
                               (relative-precedence op current-op op-stx)))
          (cond
            [(eq? rel-prec 'stronger)
             (cond
               [(operator-transformer? op)
                ;; it's up to the transformer to consume whatever it wants after the operator
                (define-values (form new-tail) (apply-infix-transformer-operator op op-stx init-form stxes check-result))
                (enforest-step form new-tail current-op)]
               [else
                ;; new operator sets precedence, defer application of operator until a suitable
                ;; right-hand argument is parsed
                (define-values (form new-tail) (enforest-step tail v))
                (enforest-step (apply-infix-direct-operator op init-form form op-stx check-result)
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
                                      "combination of ~as without declared relative precedence"
                                      " needs explicit parenthesization\n"
                                      "  other operator: ~a")
                                     operator-kind-str
                                     (syntax-e (operator-name current-op)))
                                    op-stx)])]))

        (define (dispatch-infix-implicit implicit-name head-stx)
          (define-values (v op op-stx) (lookup-infix-implicit implicit-name head-stx in-space
                                                              infix-operator-ref
                                                              operator-kind-str form-kind-str))
          (dispatch-infix-operator v op stxes op-stx)))]))

  enforest-step)

