#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt"
                     "operator-parse.rkt")
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

(begin-for-syntax
  (define-syntax-class repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))))

(define-for-syntax (convert-syntax e make-datum make-literal handle-escape deepen-escape)
  (let convert ([e e])
       (syntax-parse e
         [((~and tag (~or (~datum parens) (~datum brackets) (~datum block) (~datum alts) (~datum group)))
           g ...)
          (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [ps '()])
            (syntax-parse gs
              [()
               (values #`(#,(make-datum #'tag) . #,(reverse ps))
                       (append (or pend-idrs '()) idrs))]
              [(op:repetition . gs)
               (unless pend-idrs
                 (raise-syntax-error #f
                                     "misplaced repetition"
                                     #'op.name))
               (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                       (deepen-escape idr)))
               (loop #'gs #f (append new-pend-idrs idrs) (cons (quote-syntax ...) ps))]
              [(((~datum op) (~and (~literal ¿) ¿-id)) esc . gs)
               (define-values (id idr) (handle-escape #'¿-id #'esc e))
               (loop #'gs (list idr) (append (or pend-idrs '()) idrs) (cons id ps))]
              [(g . gs)
               (define-values (p new-ids) (convert #'g))
               (loop #'gs new-ids (append (or pend-idrs '()) idrs) (cons p ps))]))]
         [((~and tag (~datum op)) op-name)
          (values #`(#,(make-datum #'tag) #,(make-literal #'op-name)) null)]
         [id:identifier
          (values (make-literal #'id) null)]
         [_
          (values e null)])))

(define-for-syntax (convert-pattern e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-literal
                  (lambda (d)
                    #`(~literal #,d))
                  ;; handle-esvape:
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
                      [(id id-ref) #'(id (parens (group id-ref (... ...))))]))))

(define-for-syntax (convert-template e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d) d)
                  ;; make-literal
                  (lambda (d) d)
                  ;; handle-esvape:
                  (lambda (¿-id e in-e)
                    (define id (car (generate-temporaries (list e))))
                    (values id #`[#,id #,e 0]))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id-pat e depth) #`[(id-pat (... ...)) e #,(add1 (syntax-e #'depth))]]))))

(define-syntax ?
  (make-expression+binding-prefix-operator
   (quote-syntax ?)
   '((default . stronger))
   #t ; transformer
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (template idrs) (convert-template #'e))
        (define (bind-variable idr)
          (syntax-parse idr
            [(id-pat e depth)
             #`[id-pat (let ([r (rhombus-expression (group e))])
                         #,(let loop ([depth (syntax-e #'depth)])
                             (cond
                               [(eqv? depth 0) #'r]
                               [(eqv? depth 1) #'(unpack-tail r 'unquote)]
                               [else
                                #`(for/list ([r (in-list (unpack-tail r 'unquote))])
                                    #,(loop (sub1 depth)))])))]]))
        (values #`(with-syntax #,(map bind-variable idrs)
                    (#,(quote-syntax quasisyntax) #,template))
                #'tail)]))
   ;; pattern
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (pattern idrs) (convert-pattern #'e))
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
   #t ; transformer
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
