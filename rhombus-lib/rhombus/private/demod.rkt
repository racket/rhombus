#lang racket/base
(require (for-syntax racket/base
                     "version-case.rkt")
         "version-case.rkt")

(meta-if-version-at-least
 "8.14.0.3"
 (require (for-syntax compiler/cm-accomplice)
          (submod compiler/demod module-begin))
 (require (for-syntax syntax/modread
                      syntax/parse/pre)
          racket/include))

(provide (rename-out
          [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (meta-if-version-at-least
   "8.14.0.3"
   (begin
     ;; Since we expand to a module that no longer has this module as an import,
     ;; we need to register it explicitly for cm:
     (register-external-module (collection-file-path "demod.rkt" "rhombus/private"))
     (demod-module-begin stx))
   (syntax-parse stx
     [(_ mod-path . tail)
      #`(#%module-begin
         (require #,(datum->syntax #'mod-path 'racket/base))
         (include-at/relative-to/reader mod-path mod-path mod-path
                                        read-syntax/strip-module))])))

(meta-if-version-at-least
 "8.14.0.3"
 (begin)
 (define-for-syntax (read-syntax/strip-module src in)
   (define v (with-module-reading-parameterization
               (lambda ()
                 (read-syntax src in))))
   (if (eof-object? v)
       v
       (syntax-parse v
         [(module name lang (mb body ...))
          #'(begin body ...)]))))
