#lang racket/base
(require syntax/parse
         "tag.rkt"
         (for-template
          (submod "annotation.rkt" for-class)
          "parens.rkt"
          "assign.rkt"
          (rename-in "equal.rkt"
                     [= rhombus=])
          "parse.rkt"
          (only-in "class-clause-parse.rkt" private)))

(provide :constructor-field
         parse-field-annotations)

(define-syntax-class :not-equal
  #:description "an annotation term"
  #:datum-literals (op)
  #:literals (rhombus=)
  (pattern (~not (op rhombus=))))

(define-syntax-class :id-field
  #:datum-literals (group op)
  #:literals (mutable private rhombus=)
  (pattern (group (~optional (~and private (~var private))
                             #:defaults ([private #'#f]))
                  (~optional (~and mutable (~var mutable))
                             #:defaults ([mutable #'#f]))
                  name:identifier
                  ann::not-equal ...
                  (op rhombus=)
                  default-form ...+)
           #:with ((~optional c::unparsed-inline-annotation)) #'(ann ...)
           #:attr ann-seq (if (attribute c)
                              #'c.seq
                              #'#f)
           #:attr default #`((rhombus-expression (#,group-tag default-form ...))))
  (pattern (group (~optional (~and private (~var private))
                             #:defaults ([private #'#f]))
                  (~optional (~and mutable (~var mutable))
                             #:defaults ([mutable #'#f]))
                  name:identifier
                  (~optional c::unparsed-inline-annotation))
           #:attr ann-seq (if (attribute c)
                              #'c.seq
                              #'#f)
           #:attr default #'#f))

(define-syntax-class :constructor-field
  #:datum-literals (group op)
  #:literals (mutable rhombus=)
  (pattern idf::id-field
           #:attr ann-seq #'idf.ann-seq
           #:attr name #'idf.name
           #:attr keyword #'#f
           #:attr default #'idf.default
           #:attr mutable #'idf.mutable
           #:attr private #'idf.private)
  (pattern (group kw:keyword (::block idf::id-field))
           #:attr ann-seq #'idf.ann-seq
           #:attr name #'idf.name
           #:attr keyword #'kw
           #:attr default #'idf.default
           #:attr mutable #'idf.mutable
           #:attr private #'idf.private)
  (pattern (group kw:keyword)
           #:attr ann-seq #'#f
           #:attr name (datum->syntax #'kw (string->symbol (keyword->string (syntax-e #'kw))) #'kw #'kw)
           #:attr keyword #'kw
           #:attr default #'#f
           #:attr mutable #'#f
           #:attr private #'#f)
  (pattern (group kw:keyword (op rhombus=) default-form ...+)
           #:attr ann-seq #'#f
           #:attr name (datum->syntax #'kw (string->symbol (keyword->string (syntax-e #'kw))) #'kw #'kw)
           #:attr keyword #'kw
           #:attr default #`((rhombus-expression (#,group-tag default-form ...)))
           #:attr mutable #'#f
           #:attr private #'#f))


(define (parse-field-annotations ann-seqs-stx)
  (for/list ([seq (in-list (syntax->list ann-seqs-stx))])
    (syntax-parse seq
      [#f (list #'#f #'#f #'())]
      [(c::inline-annotation) (list #'c.predicate #'c.annotation-str #'c.static-infos)])))
