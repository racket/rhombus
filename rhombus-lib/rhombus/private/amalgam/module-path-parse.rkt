#lang racket/base
(require racket/symbol)

(provide module-symbol-to-lib-string
         module-lib-string-to-lib-string
         module-path-convert-parsed)

(define (module-symbol-to-lib-string sym)
  (define str (symbol->immutable-string sym))
  (if (regexp-match? #rx"/" str)
      (string-append str ".rhm")
      (string-append str "/main.rhm")))

(define (module-lib-string-to-lib-string str)
  (define (maybe-add-rhm-suffix s)
    (if (regexp-match? #rx"[.]" s) s (string-append s ".rhm")))
  (define new-str (maybe-add-rhm-suffix str))
  (and (module-path? `(lib ,new-str))
       (regexp-match? #rx"/" new-str)
       new-str))

(define (module-path-convert-parsed mod-stx)
  (cond
    [(identifier? mod-stx)
     (datum->syntax mod-stx
                    `(lib ,(module-symbol-to-lib-string (syntax-e mod-stx)))
                    mod-stx
                    mod-stx)]
    [else mod-stx]))
