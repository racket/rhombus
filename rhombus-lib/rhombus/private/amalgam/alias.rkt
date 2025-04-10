#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "id-binding.rkt"
                     "dotted-sequence.rkt")
         "definition.rkt"
         "name-root.rkt"
         "name-root-space.rkt"
         "parens.rkt"
         "parse.rkt"
         "dotted-sequence-parse.rkt"
         "name-root-ref.rkt"
         "space-parse.rkt")

(provide (for-space rhombus/defn
                    alias))

(define-defn-syntax alias
  (definition-transformer
   (lambda (stx name-prefix)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::quotes (group new-name-seq::dotted-operator-or-identifier-sequence))
           (_::block
            (~alt (~optional (~and only-space (group #:only_space . _)))
                  (~optional (~and except-space (group #:except_space . _))))
            ...
            (group (_::quotes (group old-name-seq::dotted-operator-or-identifier-sequence)))))
        #:with new-name::dotted-operator-or-identifier #'new-name-seq
        (when (and (attribute only-space)
                   (attribute except-space))
          (raise-syntax-error #f
                              "cannot specify both `~only_space` and `~except_space`"
                              stx))
        (define only-spaces (and (attribute only-space)
                                 (extract-space-names stx #'only-space)))
        (define except-spaces (if (attribute except-space)
                                  (extract-space-names stx #'except-space)
                                  null))
        (define binds
          (apply
           append
           (for/list ([space-sym (in-list (or only-spaces
                                              (cons #f (syntax-local-module-interned-scope-symbols))))]
                      #:unless (memq space-sym except-spaces))
             (define in-space
               (if space-sym
                   (make-interned-syntax-introducer/add space-sym)
                   (lambda (x) x)))
             (define old-id
               (syntax-parse #'old-name-seq
                 [(old-id:identifier)
                  #'old-id]
                 [(~var old-name (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref/maybe))
                  #:with () #'old-name.tail
                  #'old-name.name]
                 [_
                  #false]))
             (define old-id-in-space (in-space old-id))
             (cond
               [(and (if (not space-sym)
                         (identifier-binding* old-id-in-space)
                         (identifier-distinct-binding* old-id-in-space old-id))
                     old-id-in-space)
                (cons
                 (build-syntax-definition/maybe-extension
                  space-sym #'new-name.name #'new-name.extends
                  #`(make-rename-transformer (quote-syntax #,old-id-in-space)))
                 (cond
                   [(and (eq? space-sym 'rhombus/namespace)
                         (extensible-name-root (list old-id-in-space)))
                    => (lambda (name-root-id)
                         ;; also alias any extensions
                         (define out-int-id (out-of-name-root-space old-id))
                          (let ns-loop ([out-int-id out-int-id] [name-root-id name-root-id])
                            (define prefix (string-append (symbol->immutable-string (syntax-e old-id))
                                                          "."))
                            (apply
                             append
                             (for/list ([space (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                                        #:do [(define intro (if space
                                                                (make-interned-syntax-introducer/add space)
                                                                (lambda (x) x)))]
                                        [sym (in-list (syntax-bound-symbols (intro out-int-id)))]
                                        #:do [(define str (symbol->immutable-string sym))]
                                        #:when (and (> (string-length str) (string-length prefix))
                                                    (string=? prefix (substring str 0 (string-length prefix))))
                                        #:do [(define id* (datum->syntax out-int-id sym))
                                              (define id (intro id*))]
                                        #:when (identifier-extension-binding? id name-root-id)
                                        #:when (or (not space)
                                                   (identifier-distinct-binding* id id*)))
                               (define ext-sym (string->symbol (string-append
                                                                (symbol->immutable-string (syntax-e #'new-name.name))
                                                                "."
                                                                (substring (symbol->immutable-string sym) (string-length prefix)))))
                               (cons
                                (build-syntax-definition/maybe-extension
                                 space (datum->syntax #'new-name.name ext-sym #'new-name.name)
                                 ((make-interned-syntax-introducer 'rhombus/namespace) #'new-name.name)
                                 #`(make-rename-transformer (quote-syntax #,id)))
                                (if (eq? space 'rhombus/namespace)
                                    (ns-loop id* id)
                                    null))))))]
                   [else
                    null]))]
               [else
                (when only-spaces
                  (raise-syntax-error #f
                                      (format "no binding found for aliased name in the `~a` space"
                                              space-sym)
                                      stx
                                      #'old-name-seq))
                null]))))
        (when (null? binds)
          (raise-syntax-error #f
                              "no binding found for aliased name"
                              stx
                              #'old-name-seq))
        (datum->syntax #f binds)]))))

(define-for-syntax (extract-space-names stx spaces-stx)
  (syntax-parse spaces-stx
    #:datum-literals (group)
    [(group _ (_::block (group space ...) ...))
     (parse-space-names stx #'((space ...) ...))]
    [(group _ space ...)
     (parse-space-names stx #'((space ...)))]
    [_
     (raise-syntax-error #f
                         "expected space names"
                         stx
                         spaces-stx)]))
