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
  (define new-str
    (cond
      [(regexp-match? #rx"/" str)
       (if (regexp-match? #rx"[.]" str)
           str
           (string-append str ".rhm"))]
      [else
       (string-append str "/main.rhm")]))
  (and (module-path? `(lib ,new-str))
       new-str))

(define (module-path-convert-parsed mod-stx)
  (cond
    [(identifier? mod-stx)
     (datum->syntax mod-stx
                    `(lib ,(module-symbol-to-lib-string (syntax-e mod-stx)))
                    mod-stx
                    mod-stx)]
    [else mod-stx]))
