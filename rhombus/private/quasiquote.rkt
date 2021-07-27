#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
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

(define-for-syntax (escape e [depth 0])
  (syntax-parse e
    [((~and tag (~or (~datum parens) (~datum brackets) (~datum block)))
      g ...)
     (with-syntax ([(new-g ...) (map (escape-group depth) (syntax->list #'(g ...)))])
       (syntax/loc e
         (tag new-g ...)))]
    [((~and tag (~datum alts))
      b ...)
     (with-syntax ([(new-b ...) (for/list ([b (in-list (syntax->list #'(b ...)))])
                                  (escape b depth))])
       (syntax/loc e
         (tag new-b ...)))]
    [_ e]))

(define-for-syntax ((escape-group depth) g)
  (syntax-parse g
    [((~and tag (~datum group)) e ...)
     (with-syntax ([(new-e ...)
                    (let loop ([es #'(e ...)])
                      (syntax-parse es
                        [() null]
                        [((~and op ((~datum op) (~literal ¿))) e . tail)
                         (if (zero? depth)
                             (syntax-parse #'tail
                               [(((~datum op) (~literal rhombus...)) . tail)
                                (cons #'(unsyntax-splicing (unpack-tail (rhombus-expression (group e)) 'unquote))
                                      (loop #'tail))]
                               [else
                                (cons #'(unsyntax (rhombus-expression (group e))) (loop #'tail))])
                             (list* #'op (escape #'e (sub1 depth)) (loop #'tail)))]
                        [((~and op ((~datum op) (~literal ?))) e . tail)
                         (list* #'op (escape #'e (add1 depth)) (loop #'tail))]
                        [(e . tail)
                         (cons (escape #'e depth) (loop #'tail))]))])
       (syntax/loc g (tag new-e ...)))]))

(begin-for-syntax
  (define-syntax-class repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))))

(define-for-syntax (convert-pattern e)
  (syntax-parse e
    [((~and tag (~or (~datum parens) (~datum brackets) (~datum block) (~datum alts) (~datum group)))
      g ...)
     (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [ps '()])
       (syntax-parse gs
         [()
          (values #`((~datum tag) . #,(reverse ps))
                  (append (or pend-idrs '()) idrs))]
         [(op:repetition . gs)
          (unless pend-idrs
            (raise-syntax-error #f
                                "misplaced repetition"
                                #'op.name))
          (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                  (syntax-parse idr
                                    [(id id-ref) #'(id (parens (group id-ref (... ...))))])))
          (loop #'gs #f (append new-pend-idrs idrs) (cons (quote-syntax ...) ps))]
         [(((~datum op) (~literal ¿)) id:identifier . gs)
          (loop #'gs (list #'(id id)) (append (or pend-idrs '()) idrs) (cons #'id ps))]
         [(((~datum op) (~and (~literal ¿) ¿-id)) g . gs)
          (raise-syntax-error #f
                              (format "expected an identifier after ~a"
                                      (syntax-e #'¿-id))
                              #'g
                              e)]
         [(g . gs)
          (define-values (p new-ids) (convert-pattern #'g))
          (loop #'gs new-ids (append (or pend-idrs '()) idrs) (cons p ps))]))]
    [((~and tag (~datum op)) op-name)
     (values #'((~datum op) (~literal op-name)) null)]
    [id:identifier
     (values #'(~literal id) null)]
    [_
     (values e null)]))

(define-syntax ?
  (make-expression+binding-prefix-operator
   (quote-syntax ?)
   '((default . stronger))
   #t ; transformer
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (values (relocate (span-srcloc #'op #'parens-tag)
                          #`(#,(quote-syntax quasisyntax) #,(escape #'e)))
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
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op.name)]))))
