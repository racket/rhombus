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
         (submod "import.rkt" for-meta)
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space impo rhombus/impo
  #:fields
  (modifier
   macro))

(provide
 (for-syntax (for-space rhombus/namespace
                        impo_meta)))

(begin-for-syntax
  (define-name-root impo_meta
    #:fields
    (ParsedModifier
     Parsed
     AfterPrefixParsed
     AfterInfixParsed)))

;; ----------------------------------------

(define-identifier-syntax-definition-transformer modifier
  rhombus/impo
  #:extra ([#:import (quote-syntax ()) value])
  #'make-import-modifier)

(begin-for-syntax
  (define-transformer-syntax-class
    ParsedModifier :import-modifier #:rhombus/impo
    #:arity 2))

(define-for-syntax (extract-modifier form proc req)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [(~var i (:import-modifier req)) #'i.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "modified import" form)]))

(define-for-syntax (make-import-modifier proc)
  (import-modifier
   (lambda (req-in stx)
     (define req #`(parsed #:rhombus/impo #,req-in))
     (define imp (syntax-parse stx
                   [(head . tail) (proc (pack-tail #'tail) #'head req)]))
     (extract-modifier imp proc req))))

;; ----------------------------------------

(define-operator-definition-transformer macro
  'macro
  #f
  #'make-import-prefix-operator
  #'make-import-infix-operator
  #'import-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :import #:rhombus/impo
    AfterPrefixParsed :prefix-op+import+tail
    AfterInfixParsed :infix-op+import+tail))

(define-for-syntax (extract-import form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [i::import #'i.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "import" form)]))

(define-for-syntax (parsed-argument form) #`(parsed #:rhombus/impo #,form))

(define-for-syntax (make-import-infix-operator prec protocol proc assc)
  (import-infix-operator
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (extract-import (proc (parsed-argument form1) (parsed-argument form2) stx)
                         proc))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-import form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-import-prefix-operator prec protocol proc)
  (import-prefix-operator
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (extract-import (proc #`(parsed #:rhombus/impo #,form) stx)
                         proc))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-import form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc)))))
