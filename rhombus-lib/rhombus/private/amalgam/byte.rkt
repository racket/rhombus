#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "srcloc.rkt"
                     "extract-elem-from-literal.rkt")
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "number.rkt"
         "rhombus-primitive.rkt"
         "static-info.rkt"
         "provide.rkt"
         "literal.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/bind
                      rhombus/annot)
                     Byte))

(define-syntax Byte
  (expression-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id bstr . new-tail)
        (define byte (extract-byte-from-bytes #'form-id #'bstr))
        (values (wrap-static-info*
                 (relocate+reraw
                  (respan (datum->syntax #f (list #'form-id #'bstr)))
                  #`(quote #,byte))
                 (get-int-static-infos))
                #'new-tail)]))))

(define-repetition-syntax Byte
  (repetition-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id bstr . new-tail)
        (define byte (extract-byte-from-bytes #'form-id #'bstr))
        (values (make-repetition-info (respan (datum->syntax #f (list #'form-id #'bstr)))
                                      '()
                                      #`(quote #,byte)
                                      (get-int-static-infos)
                                      0)
                #'new-tail)]))))

(define-binding-syntax Byte
  (binding-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id bstr . new-tail)
        (define byte (extract-byte-from-bytes #'form-id #'bstr))
        (values (binding-form #'literal-infoer
                              #`([#,byte #,(string-append "Byte" (shrubbery-syntax->string #'bstr))]))
                #'new-tail)]))))

(define-for-syntax (extract-byte-from-bytes form-id bstr-stx)
  (extract-elem-from-literal form-id bstr-stx
                             bytes? bytes-length bytes-ref
                             "byte" "byte string"))

(void (set-primitive-contract! 'byte? "Byte"))
(define-annotation-syntax Byte
  (identifier-annotation byte? #,(get-int-static-infos)))
