#lang racket/base
(require (for-syntax racket/base
                     racket/symbol
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt"
                     "introducer.rkt"
                     "id-binding.rkt")
         "name-root.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "space-parse.rkt"
         "dotted-sequence-parse.rkt"
         (only-in "forwarding-sequence.rkt" rhombus-forward))

(provide (for-syntax build-alias-definitions))

(begin-for-syntax
  (struct alias-defn (space-sym id extends rhs)
    #:authentic))

(define-for-syntax (build-alias-definitions stx
                                            old-name-seq
                                            new-name-name new-name-extends
                                            only-spaces except-spaces)
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
       (syntax-parse old-name-seq
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
          space-sym new-name-name new-name-extends
          #`(make-rename-transformer (quote-syntax #,old-id-in-space))
          #:form stx)
         (cond
           [(and (eq? space-sym 'rhombus/namespace)
                 (extensible-name-root (list old-id-in-space)))
            => (lambda (list-name-root-id)
                 (define name-root-id (car list-name-root-id))
                 ;; also alias any extensions
                 (define out-int-id (out-of-name-root-space old-id))
                 (define done-root-ids null)
                 (define aliases
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
                                                         (symbol->immutable-string (syntax-e new-name-name))
                                                         "."
                                                         (substring (symbol->immutable-string sym) (string-length prefix)))))
                        (cons
                         ;; aliases can turn a tree of namespaces into a DAG,
                         ;; so gather all relevant "extends" for a given binding
                         ;; by making an `alias-defn` and then using `aliases->defs`
                         (alias-defn
                          space (datum->syntax new-name-name ext-sym new-name-name)
                          (list ((make-interned-syntax-introducer 'rhombus/namespace) new-name-name))
                          #`(make-rename-transformer (quote-syntax #,id)))
                         (if (eq? space 'rhombus/namespace)
                             (ns-loop id* id)
                             null))))))
                 (if (pair? aliases)
                     ;; in case this `alias` form is after `export`, fon't export individual
                     ;; extensions, because that would use the last component of the extension's
                     ;; name; instead, leave export of extensions up to exporting the namespace
                     (append (list #'(rhombus-forward #:suspend-export))
                             (aliases->defs aliases stx)
                             (list #'(rhombus-forward #:resume-export)))
                     null))]
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


(define-for-syntax (aliases->defs aliases stx)
  ;; merge `extends` lists for multiple bindingings of the same identifier
  (define ht
    (for/fold ([ht (hash)]) ([a (in-list aliases)])
      (define key (cons (alias-defn-space-sym a)
                        (syntax-e (alias-defn-id a))))
      (define l (hash-ref ht key null))
      (define new-l
        (let loop ([l l])
          (cond
            [(null? l) (list a)]
            [(bound-identifier=? (alias-defn-id a) (alias-defn-id (car l)))
             (cons (struct-copy alias-defn (car l)
                                [extends (append (alias-defn-extends a)
                                                 (alias-defn-extends (car l)))])
                   (cdr l))]
            [else (cons (car l) (loop (cdr l)))])))
      (hash-set ht key new-l)))
  (for*/list ([l (in-hash-values ht)]
              [a (in-list l)])
    (build-syntax-definition/maybe-extension
     (alias-defn-space-sym a) (alias-defn-id a)
     (datum->syntax #f (alias-defn-extends a))
     (alias-defn-rhs a)
     #:form stx)))
