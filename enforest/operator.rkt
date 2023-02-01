#lang racket/base
(require racket/base
         syntax/stx
         "private/transform.rkt"
         "syntax-local.rkt")

;; See "main.rkt" for general information about operators and parsing.
;;
;; An operator's predence is represented as a list of
;;
;;   (cons/c (or/c identifier? 'default)
;;           (or/c 'stronger 'same 'weaker))
;;
;; where the `car`s of the pairs should be distinct, and 'default
;; stands for every identifier not mentioned. The value 'stronger
;; means that the operator that has this list has a stronger
;; precedence than the one referenced by the identifier. An operator
;; is implicitly the 'same as itself (i.e., not covered by 'default).

(provide operator?
         operator-name
         operator-precedences
         operator-protocol
         operator-proc ; convention depends on category

         prefix-operator
         prefix-operator?

         infix-operator
         infix-operator?
         infix-operator-assoc)

(module+ for-parse
  (provide relative-precedence

           lookup-infix-implicit
           lookup-prefix-implicit

           information-about-bindings
           lookup-space-description

           apply-prefix-direct-operator
           apply-infix-direct-operator
           apply-prefix-transformer-operator
           apply-infix-transformer-operator))

(struct operator (name precedences protocol proc)
  #:guard (lambda (name precedences protocol proc who)
            (unless (identifier? name)
              (raise-argument-error who "identifier?" name))
            (unless (and (list? precedences)
                         (for/and ([p (in-list precedences)])
                           (and (pair? p)
                                (or (eq? (car p) 'default)
                                    (identifier? (car p)))
                                (memq (cdr p) '(weaker stronger same same-on-left same-on-right)))))
              (raise-argument-error who
                                    (string-append "(listof (cons/c (or/c identifier? 'default)"
                                                   " (or/c 'stronger 'weaker 'same 'same-on-left 'same-on-right)))")
                                    precedences))
            (unless (memq protocol '(automatic macro))
              (raise-argument-error who "(or/c 'automatic 'macro)" protocol))
            (unless (procedure? proc)
              (raise-argument-error who "procedure?" proc))
            (values name precedences protocol proc)))
            
(struct prefix-operator operator ())
(struct infix-operator operator (assoc)
  #:guard (lambda (name precedences protocol proc assoc who)
            (unless (memq assoc '(left right none))
              (raise-argument-error who "(or/c 'left 'right 'none)" assoc))
            (values name precedences protocol proc assoc)))

;; `op` is the operator just found, and `left-op` is the
;; "current" operator previously found on the left;
;; returns either
;;   * a successful comparison:
;;       - 'stronger (left takes precedence)
;;       - 'weaker (right takes precedence)
;;   * an error comparision, where the result desribes why:
;;       - 'inconsistent-prec
;;       - 'inconsistent-assoc
;;       - 'same (error because no associativity)
;;       - 'same-on-left (error because on right)
;;       - #f (no precedence relation)
(define (relative-precedence left-op op)
  (define (find op-name this-op-name precs)
    (let loop ([precs precs] [default #f])
      (cond
        [(null? precs) (if (free-identifier=? op-name this-op-name)
                           'same
                           default)]
        [(eq? (caar precs) 'default) (loop (cdr precs) (cdar precs))]
        [(free-identifier=? op-name (caar precs)) (cdar precs)]
        [else (loop (cdr precs) default)])))
  (define (invert dir)
    (case dir
      [(stronger) 'weaker]
      [(weaker) 'stronger]
      [(same-on-right) 'same-on-left]
      [(same-on-left) 'same-on-right]
      [else dir]))
  (define op-name (operator-name op))
  (define left-op-name (operator-name left-op))
  (define dir1 (find left-op-name op-name (operator-precedences op)))
  (define dir2 (invert (find op-name left-op-name (operator-precedences left-op))))
  (cond
    [(and dir1 dir2 (not (eq? dir1 dir2)))
     'inconsistent-prec]
    [else
     (define dir (or dir1 dir2
                     (and (free-identifier=? (operator-name op)
                                             (operator-name left-op))
                          'same)))
     (cond
       [(or (eq? 'same dir)
            (eq? 'same-on-right dir))
        (define op-a (infix-operator-assoc op))
        (cond
          [(and (infix-operator? left-op)
                (not (eq? op-a (infix-operator-assoc left-op))))
           'inconsistent-assoc]
          [else
           (case op-a
             [(left) 'stronger]
             [(right) 'weaker]
             [else 'same])])]
       [(eq? 'stronger dir) 'weaker]
       [(eq? 'weaker dir) 'stronger]
       [else dir])]))

(define (lookup-prefix-implicit alone-name adj-context adj-form in-space operator-ref operator-kind form-kind)
  (define op-stx (datum->syntax adj-context alone-name))
  (define op (syntax-local-value* (in-space op-stx) operator-ref))
  (unless op
    (raise-syntax-error #f
                        (format (string-append
                                 "misplaced ~a;\n"
                                 " no infix operator is between this ~a and the previous one"
                                 #;
                                 ",\n and `~a` is not bound as an implicit prefix ~a")
                                form-kind form-kind
                                #;
                                alone-name
                                #;
                                operator-kind)
                        adj-form))
  (values op op-stx))

(define (lookup-infix-implicit adjacent-name prev-form adj-context adj-form in-space operator-ref operator-kind form-kind
                               stop-on-unbound? lookup-space-description)
  (define op-stx (datum->syntax adj-context adjacent-name))
  (define op (syntax-local-value* (in-space op-stx) operator-ref))
  (unless op
    (cond
      [(identifier? prev-form)
       (raise-syntax-error #f
                           (format
                            (string-append
                             "unbound or misplaced ~a;\n"
                             " the identifier is not bound as a macro,"
                             " and no infix operator appears afterward"
                             #;
                             ",\n and `~a` is not bound as an implicit infix ~a")
                            form-kind
                            #;
                            adjacent-name
                            #;
                            operator-kind)
                           prev-form
                           #f
                           null
                           (information-about-bindings prev-form lookup-space-description))]
      [(not stop-on-unbound?)
       (raise-syntax-error #f
                           (format
                            (string-append
                             "misplaced ~a;\n"
                             " no infix operator is between this ~a and the previous one"
                             #;
                             ",\n and `~a` is not bound as an implicit infix ~a")
                            form-kind form-kind
                            #;
                            adjacent-name
                            #;
                            operator-kind)
                           adj-form)]))
  (values op op-stx))


(define (information-about-bindings id lookup-space-description)
  (let ([syms (append
               (if (identifier-binding id)
                   (list #f)
                   null)
               (for/list ([sym (in-list (syntax-local-module-interned-scope-symbols))]
                          #:when (identifier-distinct-binding ((make-interned-syntax-introducer sym) id 'add)
                                                              id))
                 sym))])
    (if (null? syms)
        ""
        (apply string-append
               "\n  bound in spaces:"
               (for/list ([sym (in-list syms)])
                 (format "\n   ~a" (or (lookup-space-description sym)
                                       (format "space with path ~s" sym))))))))

(define (lookup-space-description space-sym)
  #f)

(define (apply-prefix-direct-operator op form stx checker)
  (call-as-transformer
   stx
   (lambda (in out)
     (define proc (operator-proc op))
     (out (checker (proc (in form) stx) proc)))))

(define (apply-infix-direct-operator op form1 form2 stx checker)
  (call-as-transformer
   stx
   (lambda (in out)
     (define proc (operator-proc op))
     (checker (out (proc (in form1) (in form2) stx)) proc))))

(define (apply-prefix-transformer-operator op op-stx tail checker)
  (define proc (operator-proc op))
  (call-as-transformer
   op-stx
   (lambda (in out)
     (define-values (form new-tail) (proc (in tail)))
     (check-transformer-result (out (checker form proc))
                               (out new-tail)
                               proc))))

(define (apply-infix-transformer-operator op op-stx form1 tail checker)
  (define proc (operator-proc op))
  (call-as-transformer
   op-stx
   (lambda (in out)
     (define-values (form new-tail) (proc (in form1) (in tail)))
     (check-transformer-result (out (checker form proc))
                               (out new-tail)
                               proc))))
