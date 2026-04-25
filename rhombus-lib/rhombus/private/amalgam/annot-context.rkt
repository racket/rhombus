#lang racket/base
(require racket/mutability
         syntax/parse/pre
         "dot-property.rkt"
         "annotation-failure.rkt"
         "syntax-map.rkt")

(provide (struct-out annotation-context)
         empty-annot-context
         (struct-out annotation-dependencies)
         dependency-env-encode
         dependency-env-decode)

;; An `annotation-context` holds information analogous to a compile-time
;; environment: it reports what argument names can be used in
;; dependent annotations, and it maps each name to it's "run-time"
;; position, which is the position within a function call that uses
;; the dependent context.
(struct annotation-context (argument-names ;; identifier -> pos, keyword, identifier in closure, etc.
                            this-pos)      ;; #f or a pos
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

;; An `annotation-dependencies` holds information analogous to a
;; run-time environment: it holds the static-information "values" of
;; dependent variables. Each is either a by-position argument, keyword
;; argument, or captured in the current closure.
(struct annotation-dependencies (args     ; list of static-infos
                                 kw-args  ; map of keyword -> static-infos
                                 env      ; map of identifier -> static-infos
                                 rest?
                                 kw-rest?)
  #:property prop:field-name->accessor
  (list* null
         (hasheq 'arguments (lambda (e) (annotation-dependencies-args e))
                 'keyword_arguments (lambda (e) (annotation-dependencies-kw-args e))
                 'captured_arguments (lambda (e) (annotation-dependencies-env e))
                 'has_more_arguments (lambda (e) (annotation-dependencies-rest? e))
                 'has_more_keyword_arguments (lambda (e) (annotation-dependencies-kw-rest? e)))
         #hasheq())
  #:guard (lambda (args kw-args env rest? kw-rest? info)
            (define who 'annot_meta.Dependencies)
            (unless (list? args)
              (raise-annotation-failure who args "List"))
            (unless (immutable-hash? kw-args)
              (raise-annotation-failure who kw-args "Map"))
            (unless (immutable-hash? env)
              (raise-annotation-failure who env "Map"))
            (values args kw-args env (and rest? #t) (and kw-rest? #t))))

;; Takes a hash table that represents closure information in the sense of
;; a `annotation-context` or `annotation-dependencies` and encodes it as
;; a syntax object. (We use the same representation for both a compile-time
;; and run-time analog; they shouldn't get mixed up, but because they both
;; end up encoded in static information that could be manipulated otherwise,
;; they might.) A compile-time closure describes where to find the variable
;; at run time: position, keyword, or closure for enclosing variables. A
;; run-time closure maps each variable to its static information. See
;; `dependency-env-capture` as the place where a compile-time environment
;; is consulted to generate a "run-time" closure.
(define (dependency-env-encode ht)
  #`(#,@(for/list ([(k v) (in-hash ht)])
          #`(#,k #,v))))

;; The reverse of `dependency-env-decode`, but make it easy to filter
;; to environment entries that make sense for "run-time" closures, and
;; also make it easy to add to an existing decoding.
(define (dependency-env-decode stx only-si? [ht empty-equal_name_and_scopes-map])
  (syntax-parse stx
    [([k enc-v] . rest)
     (define v
       (cond
         [(identifier? #'enc-v) (and (not only-si?) #'env-v)]
         [(or (pair? (syntax-e #'enc-v))
              (null? (syntax-e #'enc-v)))
          #'enc-v]
         [else (and (not only-si?) (syntax->datum #'enc-v))]))
     (dependency-env-decode #'rest only-si? (if v (hash-set ht #'k v) ht))]
    [_ ht]))
