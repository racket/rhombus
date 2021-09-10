#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/boundmap
                     enforest/proc-name
                     "srcloc.rkt"
                     "tail.rkt")
         "name-root.rkt"
         "definition.rkt"
         "syntax.rkt"
         "parse.rkt")

(provide defn)

(define-syntax defn
  (simple-name-root macro
                    sequence_macro))

;; ----------------------------------------

(define-syntax macro
  (make-identifier-syntax-definition-transformer (lambda (x) x)
                                                 #'make-definition-transformer))

(define-for-syntax (make-definition-transformer proc)
  (definition-transformer
   (lambda (stx)
     (define defns (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (unpack-definitions defns proc))))

(define-for-syntax (unpack-definitions form proc)
  (syntax-parse form
    #:datum-literals (parens block group)
    [(parens (group (block (group d ...) ...)))
     #`((rhombus-definition (group d ...))
        ...)]
    [_ (raise-result-error (proc-name proc) "definition-list?" form)]))

;; ----------------------------------------

(define-syntax sequence_macro
  (make-identifier-syntax-definition-sequence-transformer
   (lambda (x) x)
   #'make-definition-sequence-transformer))

(define-for-syntax (make-definition-sequence-transformer proc)
  (definition-sequence-transformer
   (lambda (stx tail)
     (define-values (defns new-tail)
       (syntax-parse stx
         [(head . h-tail) (proc (pack-tail #'h-tail) (pack-block-tail tail) #'head)]))
     (values (unpack-definitions defns proc)
             (unpack-block-tail new-tail proc)))))

(begin-for-syntax
  (define block-with-raw (syntax-property
                          (syntax-property (datum->syntax #f 'block) 'raw "{ ")
                          'raw-tail " }"))
  (define parens-with-raw (syntax-property
                           (syntax-property (datum->syntax #f 'parens) 'raw "(")
                           'raw-tail ")"))
  (define group-with-raw (syntax-property (datum->syntax #f 'group) 'raw '()))

  (define (pack-block-tail tail)
    #`(#,parens-with-raw (#,group-with-raw (#,block-with-raw . #,tail))))

  (define (unpack-block-tail packed-tail proc)
    (syntax-parse packed-tail
      #:datum-literals (block group parens)
      [(parens (group (block . tail))) #'tail]
      [else
       (raise-result-error (if (symbol? proc) proc (proc-name proc))
                           "rhombus-syntax-block?"
                           packed-tail)])))
