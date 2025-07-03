#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "module-path.rkt"
         (only-in syntax/module-reader
                  [#%module-begin module-reader-module-begin]))

;; `rhombus/reader` and `rhombus/lang_bridge` have a lot in common

(provide (for-syntax parse-module-begin))

(define-for-syntax (parse-module-begin stx mode)
  (syntax-parse stx
    [(_ body)
     #:with content (syntax-parse #'body
                      #:datum-literals (multi group)
                      [(multi . content) #'content]
                      [(group . _) (list #'body)]
                      [else
                       (raise-syntax-error #f "ill-formed body" stx)])
     (define all-content (syntax->list #'content))
     (let loop ([content all-content]
                [accum #hasheq()])
       (cond
         [(null? content)
          (unless (hash-ref accum 'lang #f)
            (raise-error "missing `~lang` specification" stx all-content))
          (case mode
            [(reader)
             #`(module-reader-module-begin
                #:language (collapse-module-path-index (module-path-index-join
                                                        '#,(hash-ref accum 'lang)
                                                        (variable-reference->module-path-index (#%variable-reference))))
                #:read (lambda (in) (list (syntax->datum (parse-all in))))
                #:read-syntax (lambda (src in) (list (parse-all in #:source src)))
                #:info get-info-proc
                #:whole-body-readers? #t
                (provide get-info-proc)
                (require shrubbery/parse
                         (only-in (submod rhombus/private/core reader) get-info-proc)
                         (only-in rhombus/parse rhombus-expression)
                         (only-in syntax/private/modcollapse-noctc collapse-module-path-index)
                         (only-in rhombus #%quotes)))]
            [(lang_bridge)
             (define lang ((make-syntax-introducer) (hash-ref accum 'lang)))
             #`(#%plain-module-begin
                (require #,lang)
                (provide (all-from-out #,lang))
                (module configure-runtime racket/base
                  (require (submod #,lang configure-runtime)))
                (module configure-expand racket/base
                  (require (submod #,lang configure-expand))
                  (provide enter-parameterization
                           exit-parameterization)))])]
         [else
          (syntax-parse (car content)
            #:datum-literals (group block)
            [(group #:lang (block (group e ...)))
             (loop (cdr content)
                   (hash-set accum 'lang (convert-module-path stx #'(group e ...))))]
            [(group #:lang (block . _))
             (raise-error "invalid `~lang` clause" stx (list (car content)))]
            [(group #:lang e ...)
             (loop (cdr content)
                   (hash-set accum 'lang (convert-module-path stx #'(group e ...))))]
            [(group #:lang . _)
             (raise-error "invalid `~lang` clause" stx (list (car content)))]
            [_
             (raise-error "unrecognized reader clause" stx (list (car content)))])]))]))

(define-for-syntax (raise-error msg stx content)
  (raise-syntax-error 'module
                      msg
                      (if (null? content)
                          (datum->syntax #f '(multi) stx)
                          (respan
                           (no-srcloc #`(multi . #,content))))))

(define-for-syntax (convert-module-path stx g)
  (parameterize ([current-module-path-context '|~lang|])
    (syntax-parse g
      [mod-path::module-path
       (module-path-convert-parsed #'mod-path.parsed)]
      [_
       (raise-error "not a valid module path" stx (list g))])))
