#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/set)
         syntax/parse
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "definition.rkt"
         "name-root.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-syntax syntax]))

(define-simple-name-root rhombus-syntax
  class)

(define-for-syntax (generate-pattern-and-attributes stx)
  (syntax-parse stx
    #:datum-literals (alts group quotes block)
    [(block (group (quotes in-quotes)))
     (define-values (p idrs sidrs can-be-empty?)
       (convert-pattern #:splice? #t
                        #'in-quotes))
     (with-syntax ([((attr ...) ...)
                    (map (lambda (binding) (cons '#:attr binding)) idrs)])
       (values #`(pattern #,p attr ... ...)
               (map (lambda (binding) (syntax-e (car (syntax->list binding)))) idrs)))]
    [(block (group (quotes in-quotes)
                   (block groups ...)))
     (define-values (p idrs sidrs can-be-empty?)
       (convert-pattern #:splice? #t
                        #'in-quotes))
     (define explicit-attrs
       (for/fold ([other-forms null]
                  [attrs null]
                  #:result (reverse attrs))
                 ([g (in-list (syntax->list #'(groups ...)))])
         (syntax-parse g
           #:datum-literals (group block)
           [(group #:attr id (block in-block ...))
            (with-syntax ([(other ...) (reverse other-forms)])
              (values
               other-forms
               (cons #`[id (rhombus-body other ... in-block ...)] attrs)))]
           [other
            (values (cons #'other other-forms) attrs)])))
     (define all-attrs (append idrs explicit-attrs))
     (with-syntax ([((attr ...) ...)
                    (map (lambda (binding) (cons '#:attr binding)) all-attrs)])
       (values #`(pattern #,p attr ... ...)
               (map (lambda (binding) (syntax-e (car (syntax->list binding)))) all-attrs)))]))

(define-for-syntax (generate-syntax-class class-name alts)
  (let-values ([(patterns attributes)
                (for/lists (patterns attributes
                                     #:result (values patterns (apply set-intersect attributes)))
                           ([alt-stx (in-list alts)])
                  (generate-pattern-and-attributes alt-stx))])
    (list
     #`(define-splicing-syntax-class #,class-name
         #:datum-literals (block group quotes)
         #,@patterns)
     #`(define-syntax #,(in-syntax-class-space class-name)
         (rhombus-syntax-class 'term #'#,class-name '#,attributes #f)))))

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (alts group quotes block pattern description)
        ;; Classname and patterns shorthand
        [(form-id class-name (alts alt ...))
         (generate-syntax-class #'class-name (syntax->list #'(alt ...)))]
        ;; Specify patterns with "pattern"
        [(form-id class-name
                  (block (group pattern (alts alt ...))))
         (generate-syntax-class #'class-name (syntax->list #'(alt ...)))]
        [_
         (raise-syntax-error #f "expected alternatives" stx)]))))
