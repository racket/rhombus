#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     version/utils)
         rhombus/all-spaces-out
         rhombus/private/parse
         (prefix-in doc: scribble/doclang2)
         scribble/private/manual-defaults
         rhombus/private/bounce
         "scribble/private/util.rhm"
         rhombus/private/version-case)

(provide (rename-out [module-begin #%module-begin]))

(bounce "scribble/private/annot.rhm"
        "scribble/private/section.rhm"
        "scribble/private/include.rhm"
        "scribble/private/block.rhm"
        "scribble/private/element.rhm"
        "scribble/private/image.rhm"
        "scribble/private/spacing.rhm"
        "scribble/private/link.rhm"
        "scribble/private/index.rhm"
        "scribble/private/toc.rhm")

(define-syntax-rule (rhombus-out)
  (begin
    (require (except-in rhombus #%module-begin))
    (provide (all-from-out rhombus))))
(rhombus-out)

(module reader syntax/module-reader
  #:language 'rhombus/scribble
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           shrubbery/private/lang)
  (provide read-proc
           read-syntax-proc
           get-info-proc)
  (define (read-proc in)
    (list (syntax->datum (parse-all in #:mode 'text))))
  (define (read-syntax-proc src in)
    (list (parse-all in #:mode 'text #:source src)))
  (define (get-info-proc key default make-default)
    (case key
      [(drracket:keystrokes)
       (append (shrubbery-get-info-proc/mode key default make-default
                                             #:mode 'text)
               (dynamic-require 'scribble/private/indentation 'keystrokes))]
      [(drracket:toolbar-buttons)
       (dynamic-require 'scribble/tools/drracket-buttons 'drracket-buttons)]
      [(drracket:default-extension)
       "scrbl"]
      [else
       (shrubbery-get-info-proc/mode key default make-default
                                     #:mode 'text)])))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))

(define-syntax (module-begin stx)
  (check-unbound-identifier-early!)
  (syntax-parse stx
    #:datum-literals (brackets)
    [(_ (brackets g ...))
     #:with (g-unwrapped ...) (for/list ([g (in-list (version-prune (syntax->list #'(g ...))))])
                                (syntax-parse g
                                  #:datum-literals (group parens)
                                  [(group (parens sub-g)) #'sub-g]
                                  [_ g]))
     #`(doc:#%module-begin
        #:id doc
        #:begin [(module configure-runtime racket/base (require rhombus/runtime-config))]
        #:post-process post-process
        (rhombus-module-forwarding-sequence
         #:wrap-non-string convert-pre-part
         (scribble-rhombus-top g-unwrapped ...)))]))

(define-syntax (post-process-rhombus stx)
  (syntax-parse stx
    [(_ e) #'(parameterize ([decode-current-language-family (or (decode-current-language-family)
                                                                '("Rhombus"))])
               ;; chain to `post-process` from `scribble/private/manual-defaults`
               (post-process e))]))

(define (convert-pre-part v)
  (or (convert_pre_part v)
      ;; let Scribble complain:
      v))

;; Includes shortcut for string literals:
(define-syntax (scribble-rhombus-top stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ (group str:string) . rest)
       (loop #'(t . rest)
             (cons #'(quote str) accum))]
      [(_ . rest)
       (define top #`(rhombus-top-step scribble-rhombus-top #t #f #f () . rest))
       (if (null? accum)
           top
           #`(begin
               #,@(reverse accum)
               #,top))])))

(define-for-syntax (version-prune gs)
  (let loop ([gs gs])
    (cond
      [(null? gs) null]
      [else
       (syntax-parse (car gs)
         #:datum-literals (group parens)
         [(group (parens (group #:version_at_least vers:string)))
          (cond
            [(version<? (version) (syntax-e #'vers))
             null]
            [else
             (loop (cdr gs))])]
         [_ (cons (car gs) (loop (cdr gs)))])])))
