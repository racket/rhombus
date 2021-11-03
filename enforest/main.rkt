#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse
         syntax/stx
         "operator.rkt"
         (submod "operator.rkt" for-parse)
         "private/transform.rkt"
         "syntax-local.rkt"
         "name-parse.rkt"
         "name-root.rkt"
         (submod "name-root.rkt" for-parse)
         "private/name-path-op.rkt"
         "private/check.rkt"
         "implicit.rkt")

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
;; * When the inital term is an identifier followed by a name-path
;;   operator and the `lookup` analog produces a name root for the
;;   identifier, the name-root transformer is applied. Its result, a
;;   new term and tail, are used for the new state, without changing
;;   the current operator (if any).
;;
;; * A prefix or infix operator has an associated combiner procedure
;;   to produce a Racket expression form, instead of always making a
;;   `bin` or `un` AST node.
;;
;; * A prefix or infix operator can be bound to a macro transformer,
;;   in which case all parsing for the (second) argument is up to the
;;   transformer. Note that an macro infix transformer that doesn't
;;   consume further input is effectively a postfix operator.
;;
;; * Function calls, array references, and list construction are not
;;   quite built-in. Instead, those positions correspond to the use
;;   of implicit operators, such as `#%call`. The enforestation
;;   function is parameterized over the mapping from head terms to
;;   implicit operators, but there must be at least a `#%juxtapose`
;;   implicit for use when, for example, an identifier follows a
;;   term without an infix operator in between.
;;
;; * The paper's prefix-operator case seems wrong; the old operator
;;   and combiner should be pushed onto the stack, as reflected by a
;;   recursive call here.
;;
;; * Operator precedence is not based on numerical levels or even a
;;   transitive order. Instead, each operator can declare an order
;;   relative to specific other operators, and an error is reported if
;;   two operators must be compared fr precedence and have no declared
;;   order. See "operator.rkt" for more on precedence.
;;
;;  * Already-parsed forms that are encoded with `parsed` (which are
;;   "term"s in the figure's terminology) are immediately converted to
;;   parsed form (i.e., "tree term"s in the figure) by removing the
;;   `parsed` wrapper.
;;
;; Terminology compared to the paper: "form" means "tree term", and
;; "stx" means "term". A "head" or "tail" is a stx/term.

(define-syntax-rule (where expr helper ...) (begin helper ... expr))

(define-syntax (define-enforest stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:enforest enforest)
                         #:defaults ([enforest #'enforest]))
              (~optional (~seq #:enforest-step enforest-step)
                         #:defaults ([enforest-step #'enforest-step]))
              (~optional (~seq #:relative-precedence relative-precedence))
              (~optional (~seq #:syntax-class form)
                         #:defaults ([form #':form]))
              (~optional (~seq #:prefix-more-syntax-class prefix-op+form+tail)
                         #:defaults ([prefix-op+form+tail #':prefix-op+form+tail]))
              (~optional (~seq #:infix-more-syntax-class infix-op+form+tail)
                         #:defaults ([infix-op+form+tail #':infix-op+form+tail]))
              (~optional (~seq #:desc form-kind-str)
                         #:defaults ([form-kind-str #'"form"]))
              (~optional (~seq #:operator-desc operator-kind-str)
                         #:defaults ([operator-kind-str #'"operator"]))
              (~optional (~seq #:in-space in-space)
                         #:defaults ([in-space #'values]))
              (~optional (~seq #:name-path-op name-path-op)
                         #:defaults ([name-path-op #'name-path-op]))
              (~optional (~seq #:prefix-operator-ref prefix-operator-ref)
                         #:defaults ([prefix-operator-ref #'prefix-operator-ref]))
              (~optional (~seq #:infix-operator-ref infix-operator-ref)
                         #:defaults ([infix-operator-ref #'infix-operator-ref]))
              (~optional (~seq #:check-result check-result)
                         #:defaults ([check-result #'check-is-syntax]))
              (~optional (~seq #:make-identifier-form make-identifier-form)
                         #:defaults ([make-identifier-form #'values]))
              (~optional (~seq #:make-operator-form make-operator-form)
                         #:defaults ([make-operator-form #'#f]))
              (~optional (~seq #:select-prefix-implicit -select-prefix-implicit)
                         #:defaults ([-select-prefix-implicit #'select-prefix-implicit]))
              (~optional (~seq #:select-infix-implicit -select-infix-implicit)
                         #:defaults ([-select-infix-implicit #'select-infix-implicit]))
              (~optional (~seq #:juxtapose-implicit-name -juxtapose-implicit-name)
                         #:defaults ([-juxtapose-implicit-name #'juxtapose-implicit-name])))
        ...)
     #'(begin
         (define-syntax-class form
           (pattern ((~datum group) . tail) #:attr parsed (transform-out (enforest (transform-in #'tail)))))

         ;; For reentering the enforestation loop within a group, stopping when
         ;; the group ends or when an operator with weaker precedence than `op`
         ;; is found
         (define-syntax-class prefix-op+form+tail
           (pattern (op-name::name . in-tail)
                    #:do [(define op (prefix-operator-ref (syntax-local-value* (in-space #'op-name.name)
                                                                               prefix-operator-ref)))
                          (define-values (form new-tail) (enforest-step (transform-in #'in-tail) op #'op-name.name #t))]
                    #:attr parsed (transform-out form)
                    #:attr tail (transform-out new-tail)))
         (define-syntax-class infix-op+form+tail
           (pattern (op-name::name . in-tail)
                    #:do [(define op (infix-operator-ref (syntax-local-value* (in-space #'op-name.name)
                                                                              infix-operator-ref)))
                          (define-values (form new-tail) (enforest-step (transform-in #'in-tail) op #'op-name.name #t))]
                    #:attr parsed (transform-out form)
                    #:attr tail (transform-out new-tail)))

         (define enforest-step (make-enforest-step form-kind-str operator-kind-str
                                                   in-space
                                                   name-path-op prefix-operator-ref infix-operator-ref
                                                   check-result
                                                   make-identifier-form
                                                   make-operator-form
                                                   -select-prefix-implicit -select-infix-implicit -juxtapose-implicit-name))
         (define enforest (make-enforest enforest-step))

         (~? (define relative-precedence (make-relative-precedence
                                          'relative-precedence
                                          operator-kind-str
                                          in-space
                                          name-path-op prefix-operator-ref infix-operator-ref))))]))

(define (make-enforest enforest-step)
  (lambda (stxes)
    ;; either `stxes` starts with a prefix operator or this first step
    ;; will dispatch to a suitable implicit prefix operator
    (define-values (form tail) (enforest-step stxes #f #f #f))
    (let loop ([init-form form] [stxes tail])
      (cond
        [(stx-null? stxes) init-form]
        [else
         ;; either `stxes` starts with an infix operator (which was weaker
         ;; precedence than consumed in the previous step), or this step will
         ;; dispatch to a suitable implicit infix operator, like `#%juxtapose`
         (define-values (form tail) (enforest-step init-form stxes #f #f #f))
         (loop form tail)]))))

(define (make-enforest-step form-kind-str operator-kind-str
                            in-space
                            name-path-op prefix-operator-ref infix-operator-ref
                            check-result
                            make-identifier-form
                            make-operator-form
                            select-prefix-implicit select-infix-implicit juxtapose-implicit-name)
  (define (raise-unbound-operator op-stx)
    (raise-syntax-error #f
                        (string-append "unbound " operator-kind-str)
                        op-stx))
  
  ;; Takes 3 or 4 arguments, depending on whether a preceding expression is available
  (define enforest-step
    (case-lambda
      [(stxes current-op current-op-stx stop-on-unbound?)
       ;; No preceding expression, so dispatch to prefix (possibly implicit)
       ((syntax-parse stxes
          [() (raise-syntax-error #f (format "missing ~a" form-kind-str) stxes)]
          [(head::name . tail)
           (define head-id (in-space #'head.name))
           (define name-path? (starts-like-name-path? #'head #'tail))
           (define v (syntax-local-value* head-id
                                          (lambda (v) (or (and name-path?
                                                               (name-root-ref v))
                                                          (prefix-operator-ref v)
                                                          (infix-operator-ref v)))))
           (cond
             [(name-root? v)
              (define-values (head tail) (apply-name-root head-id v stxes))
              (enforest-step (datum->syntax #f (cons head tail)) current-op current-op-stx stop-on-unbound?)]
             [(prefix-operator? v)
              (dispatch-prefix-operator v #'tail stxes head-id)]
             [(infix-operator? v)
              (raise-syntax-error #f "infix operator without preceding argument" #'head.name)]
             [(identifier? #'head)
              (enforest-step (make-identifier-form #'head) #'tail current-op current-op-stx stop-on-unbound?)]
             [else
              (if make-operator-form
                  (enforest-step (make-operator-form #'head.name) #'tail current-op current-op-stx stop-on-unbound?)
                  (raise-unbound-operator #'head.name))])]
          [(((~datum parsed) inside) . tail)
           (enforest-step #'inside #'tail current-op current-op-stx stop-on-unbound?)]
          [(head . _)
           (define-values (implicit-name ctx) (select-prefix-implicit #'head))
           (dispatch-prefix-implicit implicit-name ctx #'head)])

        . where .

        (define (dispatch-prefix-operator op tail stxes op-stx)
          (cond
            [(eq? (operator-protocol op) 'macro)
             ;; it's up to the transformer to consume whatever it wants after the operator
             (define-values (form new-tail) (apply-prefix-transformer-operator op op-stx stxes check-result))
             (enforest-step form new-tail current-op current-op-stx stop-on-unbound?)]
            [else
             ;; new operator sets precedence, defer application of operator until a suitable
             ;; argument is parsed
             (define-values (form new-tail) (enforest-step (check-empty op-stx tail form-kind-str) op op-stx stop-on-unbound?))
             (enforest-step (apply-prefix-direct-operator op form op-stx check-result)
                            new-tail
                            current-op
                            current-op-stx
                            stop-on-unbound?)]))

        (define (dispatch-prefix-implicit implicit-name context-stx head-stx)
          (define-values (op op-stx) (lookup-prefix-implicit implicit-name context-stx head-stx in-space
                                                             prefix-operator-ref
                                                             operator-kind-str form-kind-str))
          (define synthetic-stxes (datum->syntax #f (cons op-stx stxes)))
          (dispatch-prefix-operator op stxes synthetic-stxes op-stx)))]

      [(init-form stxes current-op current-op-stx stop-on-unbound?)
       ;; Has a preceding expression, so dispatch to infix (possibly implicit)
       ((syntax-parse stxes
          [() (values init-form stxes)]
          [(head::name . tail)
           (define head-id (in-space #'head.name))
           (define name-path? (starts-like-name-path? #'head #'tail))
           (define v (syntax-local-value* head-id
                                          (lambda (v) (or (and name-path?
                                                               (name-root-ref v))
                                                          (infix-operator-ref v)
                                                          (prefix-operator-ref v)))))
           (cond
             [(name-root? v)
              (define-values (head tail) (apply-name-root head-id v stxes))
              (enforest-step init-form (cons head tail) current-op current-op-stx stop-on-unbound?)]
             [(infix-operator? v)
              (dispatch-infix-operator v #'tail stxes head-id)]
             [(prefix-operator? v)
              (dispatch-infix-implicit juxtapose-implicit-name #'head #'head)]
             [stop-on-unbound? (values init-form stxes)]
             [(identifier? #'head)
              (dispatch-infix-implicit juxtapose-implicit-name #'head #'head)]
             [else
              (if make-operator-form
                  (dispatch-infix-implicit juxtapose-implicit-name #'head #'head)
                  (raise-unbound-operator #'head.name))])]
          [((~and head ((~datum parsed) . _)) . _)
           (dispatch-infix-implicit juxtapose-implicit-name #'head #'head)]
          [(head . _)
           (define-values (implicit-name ctx) (select-infix-implicit #'head))
           (dispatch-infix-implicit implicit-name ctx #'head)])

        . where . 

        (define (dispatch-infix-operator op tail stxes op-stx)
          (define rel-prec (if (not current-op)
                               'weaker
                               (relative-precedence current-op op)))
          (cond
            [(eq? rel-prec 'weaker)
             (cond
               [(eq? (operator-protocol op) 'macro)
                ;; it's up to the transformer to consume whatever it wants after the operator
                (define-values (form new-tail) (apply-infix-transformer-operator op op-stx init-form stxes check-result))
                (enforest-step form new-tail current-op current-op-stx stop-on-unbound?)]
               [else
                ;; new operator sets precedence, defer application of operator until a suitable
                ;; right-hand argument is parsed
                (define-values (form new-tail) (enforest-step (check-empty op-stx tail form-kind-str) op op-stx stop-on-unbound?))
                (enforest-step (apply-infix-direct-operator op init-form form op-stx check-result)
                               new-tail
                               current-op
                               current-op-stx
                               stop-on-unbound?)])]
            [(eq? rel-prec 'stronger)
             (values init-form stxes)]
            [else
             (cond
               [(or (eq? rel-prec 'inconsistent-prec)
                    (eq? rel-prec 'inconsistent-assoc))
                (raise-syntax-error #f
                                    (format
                                     (string-append "inconsistent operator ~a declared\n"
                                                    "  left operator: ~a\n"
                                                    "  right operator: ~a")
                                     (if (eq? rel-prec 'inconsistent-prec)
                                         "precedence"
                                         "associativity")
                                     (syntax-e current-op-stx)
                                     (syntax-e op-stx))
                                    op-stx)]
               [(eq? rel-prec 'same)
                (raise-syntax-error #f
                                    "non-associative operator needs explicit parenthesization"
                                    op-stx)]
               [(eq? rel-prec 'same-on-left)
                (raise-syntax-error #f
                                    (format
                                     (string-append 
                                      "combination of ~as at same precedence, but only in the other order,"
                                      " needs explicit parenthesization\n"
                                      "  earlier operator: ~a")
                                     operator-kind-str
                                     (syntax-e current-op-stx))
                                    op-stx
                                    #f
                                    (list current-op-stx))]
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

        (define (dispatch-infix-implicit implicit-name context-stx head-stx)
          (define-values (op op-stx) (lookup-infix-implicit implicit-name init-form context-stx head-stx in-space
                                                            infix-operator-ref
                                                            operator-kind-str form-kind-str
                                                            stop-on-unbound?))
          (cond
            [(not op) ; => `stop-on-unbound?`
             (values init-form stxes)]
            [else
             (define synthetic-stxes (datum->syntax #f (cons op-stx stxes)))
             (dispatch-infix-operator op stxes synthetic-stxes op-stx)])))]))

  (define (starts-like-name-path? head tail)
    (and (identifier? head)
         (syntax-parse tail
           [(((~datum op) sep) . _) (eq? (syntax-e #'sep) name-path-op)]
           [_ #f])))

  ;; improves errors when nothing appears after an operator:
  (define (check-empty op-stx tail form-kind-str)
    (cond
      [(or (null? tail)
           (and (syntax? tail)
                (null? (syntax-e tail))))
       (raise-syntax-error #f
                           (format "missing ~a after operator" form-kind-str)
                           op-stx)]
      [else tail]))

  enforest-step)

;; see `relative-precedence` in "operator.rkt" for possible results,
;; but add 'unbound to the set of possibilities
(define (make-relative-precedence who
                                  operator-kind-str
                                  in-space
                                  name-path-op prefix-operator-ref infix-operator-ref)
  (lambda (left-mode left-op-stx right-mode right-op-stx)
    (define (lookup mode op-stx)
      (case mode
        [(prefix)
         (prefix-operator-ref (syntax-local-value* (in-space op-stx)
                                                   prefix-operator-ref))]
        [(infix)
         (infix-operator-ref (syntax-local-value* (in-space op-stx)
                                                  infix-operator-ref))]
        [else
         (raise-argument-error who "(or/c 'prefix 'infix)" mode)]))
    (define left-op (lookup left-mode left-op-stx))
    (define right-op (lookup right-mode right-op-stx))
    (if (and left-op right-op)
        (relative-precedence left-op right-op)
        'unbound)))
