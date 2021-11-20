#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         rhombus/private/parse
         rhombus/private/forwarding-sequence
         (prefix-in doc: scribble/doclang2)
         (rename-in scribble/base
                    [verbatim base:verbatim]
                    [table-of-contents table_of_contents]
                    [local-table-of-contents local_table_of_contents])
         scribble/private/manual-defaults
         "private/rhombus.rkt"
         "private/include.rkt")

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scribble/base)
                     base:verbatim
                     include-section)
         verbatim
         pkg
         include_section
         (all-from-out "private/rhombus.rkt"
                       "private/include.rkt"))
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
    (list (syntax->datum (parse-all in #:test-mode? #t))))
  (define (read-syntax-proc src in)
    (list (parse-all in #:text-mode? #t #:source src)))
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

(define-syntax (module-begin stx)
  (syntax-parse stx
    #:datum-literals (brackets)
    [(_ (brackets g ...))
     #:with (g-unwrapped ...) (for/list ([g (in-list (syntax->list #'(g ...)))])
                                (syntax-parse g
                                  #:datum-literals (group parens)
                                  [(group (parens sub-g)) #'sub-g]
                                  [else g]))
     #`(doc:#%module-begin
        #:id #,(datum->syntax stx 'doc)
        #:begin [(module configure-runtime racket/base (require rhombus/runtime-config))]
        #:post-process post-process
        (rhombus-forwarding-sequence
         (scribble-rhombus-top g-unwrapped ...)))]))

;; Includes shortcut for string literals:
(define-syntax (scribble-rhombus-top stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ str:string . rest)
       (loop #'(t . rest)
             (cons #'str accum))]
      [(_ . rest)
       (define top #`(rhombus-top-step scribble-rhombus-top . rest))
       (if (null? accum)
           top
           #`(begin
               #,@(reverse accum)
               top))])))
    
;; ----------------------------------------

(define (verbatim #:indent [indent 0] l)
  (apply base:verbatim #:indent indent l))

(define (pkg l)
  (tt l))
