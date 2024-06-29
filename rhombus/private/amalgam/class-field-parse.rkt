#lang racket/base
(require racket/keyword
         syntax/parse/pre
         enforest/name-parse
         "tag.rkt"
         (for-template (submod "annotation.rkt" for-class)
                       "parens.rkt"
                       (submod "equal.rkt" for-parse)
                       "parse.rkt"
                       (only-in "class-clause-primitive.rkt"
                                private
                                protected)
                       (submod "class-clause.rkt" for-class)
                       "var-decl.rkt"))

(provide :constructor-field
         parse-field-annotations)

(define-syntax-rule (define-field-modifier-class class mod desc)
  (define-syntax-class class
    #:attributes (name)
    #:description desc
    #:opaque
    (pattern ::name
      #:when (free-identifier=? (in-class-clause-space #'name)
                                (class-clause-quote mod)))))
(define-field-modifier-class :private-id private "the literal `private`")
(define-field-modifier-class :protected-id protected "the literal `protected`")
(define-field-modifier-class :mutable-id mutable "the literal `mutable`")

(define-syntax-class :protected-or-private
  #:attributes (exposure)
  (pattern _::private-id
           #:attr exposure #'private)
  (pattern _::protected-id
           #:attr exposure #'protected))

(define-syntax-class :id-field
  #:attributes (name exposure mutable ann-seq default)
  #:datum-literals (group op)
  (pattern (group (~optional ::protected-or-private
                             #:defaults ([exposure #'public]))
                  (~optional (~and _::mutable-id (~parse mutable #'#t))
                             #:defaults ([mutable #'#f]))
                  d::var-decl)
           #:with (name:identifier (~optional c::unparsed-inline-annotation)) #'(d.bind ...)
           #:with ann-seq #'(~? c.seq #f)
           #:with default #'d.default)
  (pattern (group (~optional ::protected-or-private
                             #:defaults ([exposure #'public]))
                  (~optional (~and _::mutable-id (~parse mutable #'#t))
                             #:defaults ([mutable #'#f]))
                  name:identifier (~optional c::unparsed-inline-annotation))
           #:with ann-seq #'(~? c.seq #f)
           #:with default #'#f))

(define (keyword->id kw)
  (datum->syntax kw
                 (string->symbol (keyword->immutable-string (syntax-e kw)))
                 kw
                 kw))

(define-syntax-class :constructor-field
  #:attributes (ann-seq name keyword default mutable exposure)
  #:datum-literals (group)
  (pattern ::id-field
           #:with keyword #'#f)
  (pattern (group keyword:keyword (_::block ::id-field)))
  (pattern (group keyword:keyword)
           #:with ann-seq #'#f
           #:with name (keyword->id #'keyword)
           #:with default #'#f
           #:with mutable #'#f
           #:with exposure #'public)
  (pattern (group keyword:keyword _::equal default-form ...+)
           #:with ann-seq #'#f
           #:with name (keyword->id #'keyword)
           #:with default #`(rhombus-expression (#,group-tag default-form ...))
           #:with mutable #'#f
           #:with exposure #'public))

(define (parse-field-annotations ann-seqs-stx)
  (for/list ([seq (in-list (syntax->list ann-seqs-stx))])
    (syntax-parse seq
      [#f (list #'#f #'#f #'())]
      [(c::inline-annotation) (list #'c.converter #'c.annotation-str #'c.static-infos)])))
