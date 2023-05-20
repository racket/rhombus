#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "srcloc.rkt"
                     "pack.rkt"
                     "macro-result.rkt"
                     "tail-returner.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (for-syntax racket/base))
         "space-provide.rkt"
         "name-root.rkt"
         (submod "export.rkt" for-meta)
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space expo rhombus/expo
  #:fields
  (modifier
   macro))

(provide
 (for-syntax (for-space rhombus/namespace
                        expo_meta)))

(begin-for-syntax
  (define-name-root expo_meta
    #:fields
    (ParsedModifier
     Parsed
     AfterPrefixParsed
     AfterInfixParsed)))

;; ----------------------------------------

(define-identifier-syntax-definition-transformer modifier
  rhombus/expo
  #:extra ([#:export (quote-syntax ()) value])
  #'make-export-modifier)

(begin-for-syntax
  (define-transformer-parameterized-syntax-class
    ParsedModifier :export-modifier))

(define-for-syntax (extract-modifier form proc req)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [(~var i (:export-modifier req)) #'i.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "modified export" form)]))

(define-for-syntax (make-export-modifier proc)
  (export-modifier
   (lambda (req-in stx)
     (define req #`(parsed #,req-in))
     (define imp (syntax-parse stx
                   [(head . tail) (proc (pack-tail #'tail) #'head req)]))
     (extract-modifier imp proc req))))

;; ----------------------------------------

(define-operator-definition-transformer macro
  'macro
  #f
  #'make-export-prefix-operator
  #'make-export-infix-operator
  #'export-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :export
    AfterPrefixParsed :prefix-op+export+tail
    AfterInfixParsed :infix-op+export+tail))

(define-for-syntax (extract-export form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [i::export #'i.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "export" form)]))

(define-for-syntax (parsed-argument form) #`(parsed #,form))

(define-for-syntax (make-export-infix-operator name prec protocol proc assc)
  (export-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (extract-export (proc (parsed-argument form1) (parsed-argument form2) stx)
                         proc))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-export form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-export-prefix-operator name prec protocol proc)
  (export-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (extract-export (proc #`(parsed #,form) stx)
                         proc))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-export form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc)))))
