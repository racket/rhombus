#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "dotted-sequence.rkt")
         "alias-define.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parens.rkt"
         "parse.rkt"
         "dotted-sequence-parse.rkt"
         "name-root-ref.rkt"
         "space-parse.rkt")

(provide (for-space rhombus/defn
                    alias))

(define-defn-syntax alias
  (definition-transformer
   (lambda (stx name-prefix effect-id)
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
        (define binds (build-alias-definitions stx
                                               #'old-name-seq
                                               #'new-name.name #'new-name.extends
                                               only-spaces except-spaces))
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
