#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-root
                     "srcloc.rkt"
                     "introducer.rkt")
         "name-root-space.rkt")

(provide define-name-root)

(define-syntax (define-name-root stx)
  (syntax-parse stx
    [(_ id (~alt (~once (~seq #:fields (content ...)))
                 (~optional (~seq #:extends extends)
                            #:defaults ([extends #'#f]))
                 (~optional (~seq #:orig-id orig-id)
                            #:defaults ([orig-id #'#f])))
        ...)
     #:with (norm-content ...) (for/list ([c (in-list (syntax->list #'(content ...)))])
                                 (syntax-parse c
                                   [_:identifier #`[#,c #,c]]
                                   ;; the `rule` component here can limit the use
                                   ;; of the field to certain spaces, and it can provide
                                   ;; alternate identifiers (so that the second one here
                                   ;; is ignored) for some specific spaces; see below for
                                   ;; more information
                                   [(_:identifier _:identifier . rule) c]))
     #:with space-id (in-name-root-space #'id)
     #:with the-orig-id (if (syntax-e #'orig-id)
                            #'orig-id
                            #'space-id)
     #'(begin
         ;; portal syntax with this shape is recognized by "name-root-ref.rkt",
         ;; "import.rkt", and "export.rkt"
         (#%require (portal space-id (nspace the-orig-id extends norm-content ...))))]))

;; Syntax of an export "rule":
;;
;;   - (): default val-id in all spaces, so equivalent to (#:except)
;;   - (#:only space ...): default val-id in listed spaces
;;   - (#:except space ...): default val-id in spaces other than listed ones
;;   - (#:space ([space val-id] ...) . rule): provides a `val-id` for specific spaces,
;;                                            shadowing anything later in the rule
;;
;; For merging purposes, the canonical form has only zero or one `#:space` cases.
;; Note that only one of `#:only` or `#:except` is possible.
