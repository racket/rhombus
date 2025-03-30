#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform
                     racket/phase+space
                     racket/symbol
                     syntax/parse/pre
                     "introducer.rkt"
                     "id-binding.rkt"
                     "skip-only-except-key.rkt")
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "dotted-sequence-parse.rkt")

(provide all-spaces-out
         all-spaces-defined-out)

(define-syntax all-spaces-out
  (make-provide-transformer
   (lambda (stx phase+spaces)
     (define phases (if (null? phase+spaces)
                        (list 0)
                        (hash-keys
                         (for/hash ([p+s (in-list phase+spaces)])
                           (values (phase+space-phase p+s) #t)))))
     (apply
      append
      (for/list ([stx (in-list (cdr (syntax->list stx)))])
        (define-values (id out-id)
          (syntax-parse stx
            [[id out-id] (values #'id #'out-id)]
            [id (values #'id #'id)]))
        (define (make-export phase space id [as-sym (syntax-e out-id)]
                             #:filter-space [filter-space #f])
          (export id
                  as-sym
                  (phase+space phase space)
                  #f ; not protected
                  (if filter-space
                      (syntax-property stx skip-only/except-space-key filter-space)
                      stx)))
        (define (adjust-prefix sym prefix)
          (if (eq? (syntax-e id) (syntax-e out-id))
              sym
              (string->symbol (string-append
                               (symbol->immutable-string (syntax-e out-id))
                               "."
                               (substring (symbol->immutable-string sym) (string-length prefix))))))
        (apply
         append
         (for/list ([phase (in-list phases)])
           (define abs-phase (phase+space+ phase (syntax-local-phase-level)))
           (define space+ids
             (for*/list ([sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                         [(intro) (in-value (if sym
                                                (make-interned-syntax-introducer sym)
                                                (lambda (x) x)))]
                         [(space-id) (in-value (intro id))]
                         #:when (and (identifier-binding* space-id abs-phase)
                                     (or (not sym)
                                         (not (free-identifier=? id space-id abs-phase)))))
               (cons sym space-id)))
           (when (null? space+ids)
             (raise-syntax-error 'export
                                 "identifier is not defined or imported"
                                 id))
           (apply
            append
            (for/list ([space+id (in-list space+ids)])
              (define space (car space+id))
              (define int-id (cdr space+id))
              (append
               (list (make-export phase space int-id))
               (cond
                 [(and (eq? space 'rhombus/namespace)
                       (extensible-name-root (list int-id)))
                  => (lambda (name-root-id)
                       ;; also export any extensions
                       (define out-int-id (out-of-name-root-space int-id))
                       (define prefix (string-append (symbol->immutable-string (syntax-e int-id)) "."))
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
                                             (identifier-distinct-binding* id id* abs-phase)))
                         (make-export phase space (intro (datum->syntax out-int-id sym out-int-id))
                                      (adjust-prefix sym prefix)
                                      #:filter-space 'rhombus/namespace)))]
                 [else null])))))))))))

(define-syntax all-spaces-defined-out
  (make-provide-transformer
   (lambda (stx phase+spaces)
     (define ht (syntax-local-module-defined-identifiers))
     (define phases (if (null? phase+spaces)
                        (list 0)
                        (hash-keys
                         (for/hash ([p+s (in-list phase+spaces)])
                           (values (phase+space-phase p+s) #t)))))
     (for*/list ([space-sym (in-list (cons #f (syntax-local-module-interned-scope-symbols)))]
                 #:do [(define intro (if space-sym
                                         (make-interned-syntax-introducer space-sym)
                                         (lambda (x mode) x)))]
                 [phase (in-list phases)]
                 #:do [(define abs-phase (phase+space+ phase (syntax-local-phase-level)))]
                 [space-id (in-list (hash-ref ht abs-phase '()))]
                 #:when (and (bound-identifier=? space-id (intro space-id 'add))
                             (free-identifier=? space-id
                                                (intro (datum->syntax stx (syntax-e space-id)) 'add)
                                                abs-phase)))
       (make-export space-id (syntax-e space-id) (phase+space phase space-sym) #f stx)))))
