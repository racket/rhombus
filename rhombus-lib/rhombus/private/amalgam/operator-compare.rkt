#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "pack.rkt"
                     "annotation-failure.rkt"
                     "name-path-op.rkt"
                     "dotted-sequence.rkt")
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "ends-parse.rkt")

(provide
 (for-syntax extract-name
             get-relative-precedence
             ends-parse?))

(begin-for-syntax
  (define (extract-name who stx space-sym
                        #:build-dotted? [build-dotted? #f])
    (define in-space (if space-sym
                         (make-interned-syntax-introducer space-sym)
                         (lambda (x) x)))
    (define s (unpack-term stx #f #f))
    (or (cond
          [(identifier? s) (in-space s)]
          [s
           (syntax-parse s
             #:datum-literals (op)
             [(op id) (in-space #'id)]
             [_ #f])]
          [else
           (define g (unpack-group stx #f #f))
           (and g
                (syntax-parse g
                  #:datum-literals (group)
                  [(group . (~var name (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref/maybe)))
                   #:when (null? (syntax-e #'name.tail))
                   (in-space #'name.name)]
                  [(group n::dotted-operator-or-identifier-sequence)
                   (cond
                     [build-dotted?
                      (define l (syntax->list #'n))
                      (datum->syntax (car l)
                                     (build-dot-symbol l #:skip-dots? #t))]
                     [else
                      (datum->syntax #f 'none)])]
                  [_ #f]))])
        (and who
             (raise-annotation-failure who stx "Name"))))

  (define (check-mode who mode)
    (unless (or (eq? mode 'prefix) (eq? mode 'infix))
      (raise-annotation-failure who mode "matching(#'prefix || #'infix)")))

  (define (get-relative-precedence who left-mode left-stx right-stx
                                   space-sym relative-precedence)
    (check-mode who left-mode)
    (define left-id (extract-name who left-stx space-sym))
    (define right-id (extract-name who right-stx space-sym))
    (case (relative-precedence left-mode left-id 'infix right-id)
      [(stronger) 'stronger]
      [(weaker) 'weaker]
      [else #f]))

  (define (ends-parse? who left-mode left-stx tail-in
                       space-sym relative-precedence infix-operator-ref)
    (check-mode who left-mode)
    (define left-id (extract-name who left-stx space-sym))
    (define tail (or (unpack-tail tail-in #f #f)
                     (raise-annotation-failure who tail-in "Sequence")))
    (define in-space (if space-sym
                         (make-interned-syntax-introducer space-sym)
                         (lambda (x) x)))
    (do-ends-parse? left-mode left-id tail
                    in-space relative-precedence infix-operator-ref)))
