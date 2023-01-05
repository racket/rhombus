#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "name-root.rkt"
         "expression.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "static-info.rkt"
         "dot-parse.rkt"
         "call-result-key.rkt"
         "composite.rkt")

(provide Srcloc
         (for-space rhombus/annot Srcloc))

(module+ for-builtin
  (provide srcloc-method-table))

(module+ for-static-info
  (provide (for-syntax srcloc-static-infos)))

(define-for-syntax srcloc-static-infos
  #'((#%dot-provider srcloc-instance)))

(define-static-info-syntax srcloc
  (#%call-result #,srcloc-static-infos))

(define-annotation-syntax Srcloc
  (identifier-annotation #'Srcloc #'srcloc? srcloc-static-infos))

(define-name-root Srcloc
  #:root (make-expression+binding-prefix-operator
          #'Srcloc
          '((default . stronger))
          'macro
          (lambda (stx)
            (syntax-parse stx
              [(_ . tail)
               (values #'srcloc #'tail)]))
          (make-composite-binding-transformer "Srcloc"
                                              #'srcloc?
                                              (list #'srcloc-source
                                                    #'srcloc-line
                                                    #'srcloc-column
                                                    #'srcloc-position
                                                    #'srcloc-span)
                                              #'(() () () () ())))
  #:fields
  ([source srcloc-source]
   [line srcloc-line]
   [column srcloc-column]
   [position srcloc-position]
   [span srcloc-span]))

(define srcloc-method-table
  (hash 'source srcloc-source
        'line srcloc-line
        'column srcloc-column
        'position srcloc-position
        'span srcloc-span))

(define-syntax srcloc-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(source) (field (lambda (e) #`(srcloc-source #,e)))]
        [(line) (field (lambda (e) #`(srcloc-line #,e)))]
        [(column) (field (lambda (e) #`(srcloc-column #,e)))]
        [(position) (field (lambda (e) #`(srcloc-position #,e)))]
        [(span) (field (lambda (e) #`(srcloc-span #,e)))]
        [else (fail-k)])))))
