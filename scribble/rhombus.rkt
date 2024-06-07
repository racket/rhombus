#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         rhombus/all-spaces-out
         rhombus/private/parse
         (prefix-in doc: scribble/doclang2)
         scribble/private/manual-defaults
         rhombus/private/bounce
         "private/util.rhm")

(provide (rename-out [module-begin #%module-begin]))

(bounce "private/section.rhm"
        "private/include.rhm"
        "private/block.rhm"
        "private/element.rhm"
        "private/image.rhm"
        "private/spacing.rhm"
        "private/link.rhm"
        "private/index.rhm"
        "private/toc.rhm")

(define-syntax-rule (rhombus-out)
  (begin
    (require (except-in rhombus #%module-begin))
    (provide (all-from-out rhombus))))
(rhombus-out)

(module reader syntax/module-reader
  #:language 'scribble/rhombus
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require shrubbery/parse
           (only-in (submod shrubbery reader)
                    [get-info-proc shrubbery:get-info-proc]))
  (provide read-proc
           read-syntax-proc
           get-info-proc)
  (define (read-proc in)
    (list (syntax->datum (parse-all in #:mode 'text))))
  (define (read-syntax-proc src in)
    (list (parse-all in #:mode 'text #:source src)))
  (define (get-info-proc key default make-default)
    (case key
      [(color-lexer)
       (dynamic-require 'shrubbery/syntax-color
                        'shrubbery-text-mode-lexer)]
      [(drracket:keystrokes)
       (append (shrubbery:get-info-proc key default make-default)
               (dynamic-require 'scribble/private/indentation 'keystrokes))]
      [(drracket:toolbar-buttons)
       (dynamic-require 'scribble/tools/drracket-buttons 'drracket-buttons)]
      [else (shrubbery:get-info-proc key default make-default)])))

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))

(define-syntax (module-begin stx)
  (check-unbound-identifier-early!)
  (syntax-parse stx
    #:datum-literals (brackets)
    [(_ (brackets g ...))
     #:with (g-unwrapped ...) (for/list ([g (in-list (syntax->list #'(g ...)))])
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

(define (convert-pre-part v)
  (or (convert_pre_part v)
      ;; let Scribble complain:
      v))

;; Includes shortcut for string literals:
(define-syntax (scribble-rhombus-top stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ str:string . rest)
       (loop #'(t . rest)
             (cons #'str accum))]
      [(_ . rest)
       (define top #`(rhombus-top-step scribble-rhombus-top #t () . rest))
       (if (null? accum)
           top
           #`(begin
               #,@(reverse accum)
               top))])))
