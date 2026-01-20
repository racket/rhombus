#lang racket/base
(require racket/mutability
         "dot-property.rkt"
         "annotation-failure.rkt"
         "syntax-map.rkt")

(provide (struct-out annotation-context)
         empty-annot-context
         (struct-out annotation-dependencies))

(struct annotation-context (argument-names this-pos)
  #:property prop:field-name->accessor
  (list* null
         ;; Duplicates the table that is constructed by
         ;; `define-primitive-class` in "annot-context-meta.rkt",
         ;; but this module is often used for-syntax, and we don't need
         ;; all of Rhombus's meta support for those uses
         (hasheq 'argument_names (lambda (e) (annotation-context-argument-names e)))
         #hasheq())
  #:guard (lambda (argument-names this-pos info)
            (define who 'annot_meta.Context)
            (unless (and (immutable-hash? argument-names)
                         (equal-name-and-scopes-map? argument-names))
              (raise-annotation-failure who argument-names "Map.by(equal_name_and_scopes)"))
            (values argument-names this-pos)))

(define empty-annot-context
  (annotation-context empty-equal_name_and_scopes-map #f))

(struct annotation-dependencies (args     ; list of static-infos
                                 kw-args  ; map of keyword -> static-infos
                                 rest?
                                 kw-rest?)
  #:property prop:field-name->accessor
  (list* null
         (hasheq 'arguments (lambda (e) (annotation-dependencies-args e))
                 'keyword_arguments (lambda (e) (annotation-dependencies-kw-args e))
                 'has_more_arguments (lambda (e) (annotation-dependencies-rest? e))
                 'has_more_keyword_arguments (lambda (e) (annotation-dependencies-kw-rest? e)))
         #hasheq())
  #:guard (lambda (args kw-args rest? kw-rest? info)
            (define who 'annot_meta.Dependencies)
            (unless (list? args)
              (raise-annotation-failure who args "List"))
            (unless (immutable-hash? kw-args)
              (raise-annotation-failure who kw-args "Map"))
            (values args kw-args (and rest? #t) (and kw-rest? #t))))
