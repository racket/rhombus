#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/list
                     "operator-parse.rkt"
                     "srcloc.rkt"
                     "consistent.rkt"
                     "same-expression.rkt")
         "expression.rkt"
         (only-in "repetition.rkt"
                  in-repetition-space
                  repet-quote
                  repetition-prefix+infix-operator)
         "compound-repetition.rkt"
         "dotted-sequence-parse.rkt"
         "parse.rkt"
         "macro-macro.rkt"
         "definition.rkt"
         "static-info.rkt"
         (submod "function.rkt" for-build)
         (only-in "entry-point.rkt" no-adjustments))

;; The `operator` form takes something that looks like a function-style
;; operator definition and generates a combination of a transformer and
;; a function

(provide (rename-out
          [rhombus-operator operator]))

(begin-for-syntax

  (define-syntax-class :not-op
    #:description "non-operator"
    #:datum-literals (op)
    (pattern (~not (op _))))
  
  (define-splicing-syntax-class :prefix-case
    #:description "prefix operator case"
    (pattern (~seq (parens (~and g (group op-name-seq::dotted-operator-or-identifier-sequence arg::not-op)))
                   ret::ret-annotation
                   ((~and tag block) (~var options (:prefix-operator-options '#f))
                                     body ...))
             #:with op-name::dotted-operator-or-identifier #'op-name-seq
             #:attr name #'op-name.name
             #:attr extends #'op-name.extends
             #:attr prec #'options.prec
             #:attr rhs #'(tag body ...)
             #:attr ret-predicate #'ret.predicate
             #:attr ret-static-infos #'ret.static-infos))
  
  (define-splicing-syntax-class :infix-case
    #:description "infix operator case"
    (pattern (~seq (parens (~and g (group left::not-op op-name-seq::dotted-operator-or-identifier-sequence right::not-op)))
                   ret::ret-annotation
                   ((~and tag block) (~var options (:infix-operator-options '#f))
                                     body ...))
             #:with op-name::dotted-operator-or-identifier #'op-name-seq
             #:attr name #'op-name.name
             #:attr extends #'op-name.extends
             #:attr prec #'options.prec
             #:attr assc #'options.assc
             #:attr rhs #'(tag body ...)
             #:attr ret-predicate #'ret.predicate
             #:attr ret-static-infos #'ret.static-infos))

  (define-splicing-syntax-class :postfix-case
    #:description "postfix operator case"
    (pattern (~seq (parens (~and g (group arg::not-op op-name-seq::dotted-operator-or-identifier-sequence)))
                   ret::ret-annotation
                   ((~and tag block) (~var options (:prefix-operator-options '#f))
                                     body ...))
             #:with op-name::dotted-operator-or-identifier #'op-name-seq
             #:attr name #'op-name.name
             #:attr extends #'op-name.extends
             #:attr prec #'options.prec
             #:attr rhs #'(tag body ...)
             #:attr ret-predicate #'ret.predicate
             #:attr ret-static-infos #'ret.static-infos))

  (define (make-prefix name op-proc prec static-infos)
    (with-syntax ([op-proc op-proc])
      #`(make-expression&repetition-prefix-operator
         (expr-quote #,name)
         (repet-quote #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (arg self-stx)
           (relocate (span-srcloc self-stx arg)
                     (wrap-static-info*
                      #`(op-proc #,arg)
                      (quote-syntax #,static-infos)))))))

  (define (make-infix name op-proc prec assc static-infos)
    (with-syntax ([op-proc op-proc])
      #`(make-expression&repetition-infix-operator
         (expr-quote #,name)
         (repet-quote #,name)
         #,(convert-prec prec)
         'automatic
         (lambda (left right self-stx)
           (relocate (span-srcloc left right)
                     (wrap-static-info*
                      #`(op-proc #,left #,right)
                      (quote-syntax #,static-infos))))
         #,(convert-assc assc))))

  (define (make-postfix name op-proc prec static-infos)
    (with-syntax ([op-proc op-proc])
      #`(make-expression&repetition-infix-operator
         (expr-quote #,name)
         (repet-quote #,name)
         #,(convert-prec prec)
         'macro
         (lambda (left stx)
           (syntax-parse stx
             [(self . tail)
              (values (relocate (span-srcloc left #'self)
                                (wrap-static-info*
                                 #`(op-proc #,left)
                                 (quote-syntax #,static-infos)))
                      #'tail)]))
         'none)))

  (define (parse-binding arg)
    (syntax-parse #`(group #,arg)
      [arg::binding #'arg.parsed]))

  (define (build-unary-function name args rhss start end ret-predicates)
    (define arg-parseds (map parse-binding args))
    (define falsess (for/list ([a (in-list args)]) #'(#f)))
    (define (->stx l) (datum->syntax #f l))
    (define-values (proc arity)
      (cond
        [(= (length args) 1)
         (build-function no-adjustments
                         name
                         (car falsess) (->stx args) (->stx arg-parseds) (car falsess)
                         #'#f #'#f
                         #'#f #'#f
                         (car ret-predicates)
                         (car rhss)
                         start end)]
        [else
         (define falses (->stx (for/list ([a (in-list args)]) #'#f)))
         (build-case-function no-adjustments
                              name
                              (->stx falsess) (->stx (map list args)) (->stx (map list arg-parseds))
                              falses falses
                              falses falses
                              (->stx ret-predicates)
                              (->stx rhss)
                              start end)]))
    proc)

  (define (build-binary-function name lefts rights rhss start end ret-predicates)
    (define-values (left-parseds right-parseds)
      (for/lists (left-parseds right-parseds) ([left (in-list lefts)]
                                               [right (in-list rights)])
        (values (parse-binding left) (parse-binding right))))
    (define falsess (for/list ([a (in-list lefts)]) #'(#f #f)))
    (define (->stx l) (datum->syntax #f l))
    (define-values (proc arity)
      (cond
        [(= (length lefts) 1)
         (build-function no-adjustments
                         name
                         (car falsess)
                         (->stx (list (car lefts) (car rights)))
                         (->stx (list (car left-parseds) (car right-parseds)))
                         (car falsess)
                         #'#f #'#f
                         #'#f #'#f
                         (car ret-predicates)
                         (car rhss)
                         start end)]
        [else
         (define falses (->stx (for/list ([a (in-list lefts)]) #'#f)))
         (build-case-function no-adjustments
                              name
                              (->stx falsess)
                              (->stx (map list lefts rights))
                              (->stx (map list left-parseds right-parseds))
                              falses falses
                              falses falses
                              (->stx ret-predicates)
                              (->stx rhss)
                              start end)]))
    proc)

  (define (generate-prefix form-id gs name extends args prec rhss ret-predicates ret-static-infos)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (cons
       #`(define op-proc
           #,(build-unary-function name args rhss form-id (last gs) ret-predicates))
       (build-syntax-definitions/maybe-extension
        '(#f rhombus/repet) name extends
        (make-prefix name #'op-proc prec ret-static-infos)))))

  (define (generate-infix form-id gs name extends lefts rights prec assc rhss ret-predicates ret-static-infos)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (cons
       #`(define op-proc
           #,(build-binary-function name lefts rights rhss form-id (last gs) ret-predicates))
       (build-syntax-definitions/maybe-extension
        '(#f rhombus/repet) name extends
        (make-infix name #'op-proc prec assc ret-static-infos)))))
    
  (define (generate-postfix form-id gs name extends args prec rhss ret-predicates ret-static-infos)
    (with-syntax ([(op-proc) (generate-temporaries (list name))])
      (cons
       #`(define op-proc
           #,(build-unary-function name args rhss form-id (last gs) ret-predicates))
       (build-syntax-definitions/maybe-extension
        '(#f rhombus/repet) name extends
        (make-postfix name #'op-proc prec ret-static-infos)))))

  (define (generate-prefix+infix stx
                                 p-gs p-name p-extends p-args p-prec p-rhss p-ret-predicates p-ret-static-infos
                                 i-gs i-name i-extends i-lefts i-rights i-prec i-assc i-rhss i-ret-predicates i-ret-static-infos)
    (with-syntax ([(p-op-proc i-op-proc) (generate-temporaries (list p-name i-name))])
      (list*
       #`(define p-op-proc
           #,(build-unary-function p-name p-args p-rhss (first p-gs) (last p-gs) p-ret-predicates))
       #`(define i-op-proc
           #,(build-binary-function i-name i-lefts i-rights i-rhss (first i-gs) (last i-gs) i-ret-predicates))
       (build-syntax-definitions/maybe-extension
        '(#f rhombus/repet) p-name p-extends
        #`(let-values ([(prefix-expr prefix-repet)
                        #,(make-prefix p-name #'p-op-proc p-prec p-ret-static-infos)]
                       [(infix-expr infix-repet)
                        #,(make-infix i-name #'i-op-proc i-prec i-assc i-ret-static-infos)])
            (values
             (expression-prefix+infix-operator prefix-expr infix-expr)
             (repetition-prefix+infix-operator prefix-repet infix-repet)))))))

  (define (generate-prefix+postfix stx
                                   p-gs p-name p-extends p-args p-prec p-rhss p-ret-predicates p-ret-static-infos
                                   a-gs a-name a-extends a-args a-prec a-rhss a-ret-predicates a-ret-static-infos)
    (with-syntax ([(p-op-proc a-op-proc) (generate-temporaries (list p-name a-name))])
      (list*
       #`(define p-op-proc
           #,(build-unary-function p-name p-args p-rhss (first p-gs) (last p-gs) p-ret-predicates))
       #`(define a-op-proc
           #,(build-unary-function a-name a-args a-rhss (first a-gs) (last a-gs) a-ret-predicates))
       (build-syntax-definitions/maybe-extension
        '(#f rhombus/repet) p-name p-extends
        #`(let-values ([(prefix-expr prefix-repet)
                        #,(make-prefix p-name #'p-op-proc p-prec p-ret-static-infos)]
                       [(infix-expr infix-repet)
                        #,(make-postfix a-name #'a-op-proc a-prec a-ret-static-infos)])
            (values
             (expression-prefix+infix-operator prefix-expr infix-expr)
             (repetition-prefix+infix-operator prefix-repet infix-repet))))))))

(begin-for-syntax
  (struct opcase (g name extends prec rhs ret-predicate ret-static-infos))
  (struct unary-opcase opcase (arg))
  (struct binary-opcase opcase (left right assc))

  (define (intersect-static-infos static-infoss)
    (if (for/and ([static-infos (in-list (cdr static-infoss))])
          (same-expression? (car static-infoss) static-infos))
        (car static-infoss)
        #'())))

(define-syntax rhombus-operator
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (parens group block alts op)
        [(form-id p::prefix-case)
         (generate-prefix #'form-id (list #'p.g) #'p.name #'p.extends (list #'p.arg) #'p.prec (list #'p.rhs)
                          (list #'p.ret-predicate) #'p.ret-static-infos)]
        [(form-id i::infix-case)
         (generate-infix #'form-id (list #'i.g) #'i.name #'i.extends (list #'i.left) (list #'i.right) #'i.prec #'i.assc (list #'i.rhs)
                         (list #'i.ret-predicate) #'i.ret-static-infos)]
        [(form-id p::postfix-case)
         (generate-postfix #'form-id (list #'p.g) #'p.name #'p.extends (list #'p.arg) #'p.prec (list #'p.rhs)
                           (list #'p.ret-predicate) #'p.ret-static-infos)]
        [(form-id (alts . as))
         (define-values (all pres ins posts)
           (let loop ([as (syntax->list #'as)] [all '()] [pres '()] [ins '()] [posts '()])
             (cond
               [(null? as) (values (reverse all) (reverse pres) (reverse ins) (reverse posts))]
               [else
                (syntax-parse (car as)
                  #:datum-literals (parens group block alts op)
                  [(_ (_ p::prefix-case))
                   (define opc (unary-opcase #'p.g #'p.name #'p.extends
                                             #'p.prec #'p.rhs #'p.ret-predicate #'p.ret-static-infos
                                             #'p.arg))
                   (loop (cdr as)
                         (cons opc all)
                         (cons opc pres)
                         ins
                         posts)]
                  [(_ (_ i::infix-case))
                   (define opc (binary-opcase #'i.g #'i.name #'i.extends
                                              #'i.prec #'i.rhs #'i.ret-predicate #'i.ret-static-infos
                                              #'i.left #'i.right #'i.assc))
                   (loop (cdr as)
                         (cons opc all)
                         pres
                         (cons opc ins)
                         posts)]
                  [(_ (_ p::postfix-case))
                   (define opc (unary-opcase #'p.g #'p.name #'p.extends
                                             #'p.prec #'p.rhs #'p.ret-predicate #'p.ret-static-infos
                                             #'p.arg))
                   (loop (cdr as)
                         (cons opc all)
                         pres
                         ins
                         (cons opc posts))])])))
         (check-consistent stx (map opcase-name all) "operator")
         (when (and (pair? ins) (pair? posts))
           (raise-syntax-error #f
                               "combination of infix and postfix cases not allowed"
                               stx))
         (define (check-options opcs extract options what)
           (unless (null? opcs)
             (for ([opc (in-list (cdr opcs))])
               (when (and (syntax-e (extract opc))
                          (not (null? (syntax-e (extract opc)))))
                 (raise-syntax-error #f
                                     (format "~a option not allowed after first ~a case"
                                             options what)
                                     stx)))))
         (check-options pres opcase-prec "precedence" "prefix")
         (check-options ins opcase-prec "precedence" "infix")
         (check-options ins binary-opcase-assc "associativity" "infix")
         (check-options posts opcase-prec "precedence" "postfix")
         (cond
           [(and (null? ins) (null? posts))
            (generate-prefix #'form-id (map opcase-g pres) (opcase-name (car pres)) (opcase-extends (car pres))
                             (map unary-opcase-arg pres) (opcase-prec (car pres)) (map opcase-rhs pres)
                             (map opcase-ret-predicate pres) (intersect-static-infos (map opcase-ret-static-infos pres)))]
           [(and (null? pres) (null? posts))
            (generate-infix #'form-id (map opcase-g ins) (opcase-name (car ins)) (opcase-extends (car ins))
                            (map binary-opcase-left ins) (map binary-opcase-right ins)
                            (opcase-prec (car ins)) (binary-opcase-assc (car ins))
                            (map opcase-rhs ins)
                            (map opcase-ret-predicate ins) (intersect-static-infos (map opcase-ret-static-infos ins)))]
           [(pair? ins)
            (generate-prefix+infix stx
                                   (map opcase-g pres) (opcase-name (car pres)) (opcase-extends (car pres))
                                   (map unary-opcase-arg pres) (opcase-prec (car pres)) (map opcase-rhs pres)
                                   (map opcase-ret-predicate pres) (intersect-static-infos (map opcase-ret-static-infos pres))

                                   (map opcase-g ins) (opcase-name (car ins)) (opcase-extends (car ins))
                                   (map binary-opcase-left ins) (map binary-opcase-right ins)
                                   (opcase-prec (car ins)) (binary-opcase-assc (car ins))
                                   (map opcase-rhs ins)
                                   (map opcase-ret-predicate ins) (intersect-static-infos (map opcase-ret-static-infos ins)))]
           [else
            (generate-prefix+postfix stx
                                     (map opcase-g pres) (opcase-name (car pres)) (opcase-extends (car pres))
                                     (map unary-opcase-arg pres) (opcase-prec (car pres)) (map opcase-rhs pres)
                                     (map opcase-ret-predicate pres) (intersect-static-infos (map opcase-ret-static-infos pres))

                                     (map opcase-g posts) (opcase-name (car posts)) (opcase-extends (car posts))
                                     (map unary-opcase-arg posts) (opcase-prec (car posts)) (map opcase-rhs posts)
                                     (map opcase-ret-predicate posts) (intersect-static-infos (map opcase-ret-static-infos posts)))])]))))
