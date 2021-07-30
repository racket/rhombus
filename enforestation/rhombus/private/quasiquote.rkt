#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/operator-parse
                     "srcloc.rkt")
         syntax/parse
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "tail.rkt")

(provide ?
         ¿
         (rename-out [¿ ??]
                     [rhombus... ...]))

(module+ convert
  (provide (for-syntax convert-pattern)))

(begin-for-syntax
  (define-syntax-class repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))))

(define-for-syntax (convert-syntax e make-datum make-literal handle-escape deepen-escape handle-maybe-empty-group)
  (let convert ([e e])
    (syntax-parse e
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum block)))
        (~and g ((~datum group) . _)))
       ;; Special case: for a single group with (), [], or {}, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs can-be-empty?) (convert #'g))
       (if can-be-empty?
           (handle-maybe-empty-group #'tag p new-idrs)
           (values #`(#,(make-datum #'tag) #,p)
                   new-idrs 
                   #f))]
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum block) (~datum alts) (~datum group)))
        g ...)
       (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [ps '()] [can-be-empty? #t])
         (syntax-parse gs
           [()
            (values #`(#,(make-datum #'tag) . #,(reverse ps))
                    (append (or pend-idrs '()) idrs)
                    can-be-empty?)]
           [(op:repetition . gs)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                    (deepen-escape idr)))
            (loop #'gs #f (append new-pend-idrs idrs) (cons (quote-syntax ...) ps) can-be-empty?)]
           [(((~datum op) (~and (~literal ¿) ¿-id)) esc . gs)
            (define-values (id idr) (handle-escape #'¿-id #'esc e))
            (loop #'gs (list idr) (append (or pend-idrs '()) idrs) (cons id ps) (and can-be-empty? (not pend-idrs)))]
           [(g . gs)
            (define-values (p new-ids nested-can-be-empty?) (convert #'g))
            (loop #'gs new-ids (append (or pend-idrs '()) idrs) (cons p ps) (and can-be-empty? (not pend-idrs)))]))]
      [((~and tag (~datum op)) op-name)
       (values #`(#,(make-datum #'tag) #,(make-literal #'op-name)) null #f)]
      [id:identifier
       (values (make-literal #'id) null #f)]
      [_
       (values e null #f)])))

(define-for-syntax (convert-pattern e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-literal
                  (lambda (d)
                    #`(~literal #,d))
                  ;; handle-escape:
                  (lambda (¿-id e in-e)
                    (if (identifier? e)
                        (values e #`[#,e #,e])
                        (raise-syntax-error #f
                                            (format "expected an identifier after ~a"
                                                    (syntax-e #'¿-id))
                                            in-e
                                            e)))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id id-ref) #'(id (parens (group id-ref (... ...))))]))
                  ;; handle-maybe-empty-group
                  (lambda (tag pat idrs)
                    (values #`(~or* ((~datum #,tag) #,pat)
                                    (~and ((~datum #,tag))
                                          ;; sets all pattern variables to nested empties:
                                          ((~datum #,tag) . #,(syntax-parse pat
                                                                [(_ . tail) #'tail]))))
                            idrs
                            #f))))

(define-for-syntax (convert-template e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d) d)
                  ;; make-literal
                  (lambda (d) d)
                  ;; handle-escape:
                  (lambda (¿-id e in-e)
                    (define id (car (generate-temporaries (list e))))
                    (values id #`[#,id (rhombus-expression (group #,e)) 0]))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      #:literals (convert-empty-group)
                      [(id-pat (convert-empty-group (qs t) c-depth) 0)
                       #`[(id-pat (... ...)) (convert-empty-group (qs (t (... ...))) #,(add1 (syntax-e #'c-depth))) 0]]
                      [(id-pat e depth)
                       ;; defer conversion of `e` to `wrap-bindings`:
                       #`[(id-pat (... ...)) e #,(add1 (syntax-e #'depth))]]))
                  ;; handle-maybe-empty-group
                  (lambda (tag template idrs)
                    (define id (car (generate-temporaries '(group))))
                    (values #`(#,tag #,id (... ...))
                            (cons #`[(#,id (... ...))
                                     (convert-empty-group (#,(quote-syntax quasisyntax) #,template) 0)
                                     0]
                                  idrs)
                            #f))))

(define (convert-empty-group l at-depth)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (if (or (null? u)
             (and (syntax? u) (null? (syntax-e u))))
         null
         (list l))]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-group g (sub1 at-depth)))]))

(require (for-syntax racket/pretty))

(define-syntax ?
  (make-expression+binding-prefix-operator
   (quote-syntax ?)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (template idrs can-be-empty?) (convert-template #'e))
        (define (wrap-bindings idrs body)
          (cond
            [(null? idrs) body]
            [else
             (wrap-bindings
              (cdr idrs)
              (syntax-parse (car idrs)
                [(id-pat e depth)
                 #`(with-syntax ([id-pat (let ([r e])
                                           #,(let loop ([depth (syntax-e #'depth)])
                                               (cond
                                                 [(eqv? depth 0) #'r]
                                                 [(eqv? depth 1) #'(unpack-tail r 'unquote)]
                                                 [else
                                                  #`(for/list ([r (in-list (unpack-tail r 'unquote))])
                                                      #,(loop (sub1 depth)))])))])
                     #,body)]))]))
        (values (wrap-bindings idrs #`(#,(quote-syntax quasisyntax) #,template))
                #'tail)]))
   ;; pattern
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (pattern idrs can-be-empty?) (convert-pattern #'e))
        (with-syntax ([((id id-ref) ...) idrs]
                      [(false ...) (for/list ([idr (in-list idrs)]) #'#f)])
          (values
           (binding-form
            #'(id ...)
            #`(lambda (v)
                (if (syntax? v)
                    (syntax-parse v
                      [#,pattern (values #t (syntax id-ref) ...)]
                      [_ (values #f false ...)])
                    (values #f false ...)))
            #'(begin))
           #'tail))]))))

(define-syntax ¿
  (expression-prefix-operator
   (quote-syntax ¿)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op)]))))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op.name)]))))
