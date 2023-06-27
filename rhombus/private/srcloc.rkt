#lang racket/base
(require shrubbery/srcloc
         shrubbery/property)

(provide syntax-srcloc
         no-srcloc
         no-srcloc*
         span-srcloc
         relocate
         relocate-id
         respan-empty
         respan
         maybe-respan
         with-syntax-error-respan)

;; Source locations and 'raw properties for shrubbery forms as syntax
;; objects:
;;
;;  * `group` and `multi` aren't expected to have source locations;
;;    ideally, they have 'raw as "", but it's best not to rely on that;
;;    the S-expression parentheses around `group` from the Shrubbery
;;    reader will have a spanning srcloc, but not from a Rhombus-level
;;    template construction
;;
;;  * `parens` and similar (including `block`) are expected to have a
;;    source locations that span their content; they have 'raw,
;;    'raw-suffix, etc.; the reader copies that source location to
;;    sourringing parentheses, but a Rhombus template construction
;;    doesn't
;;
;;  * `op` normally has the same source location as its symbol, but
;;    it's best not to rely on that
;;
;;  * `alts` is like `gruop`: it isn't expected to have a source
;;    location, although the shrubbery reader will associate a
;;    spanning source location to surrounding parentheses.
;;
;; "Respan" means to give a syntax object (i.e., the immediate wrapper)
;; a source location that corresponds to the content. That may involve
;; moving out a `parens`, etc., tag or walking through a `group` content
;; to create a source location that spans all the content, for example.
;;
;; For most calls to `raise-syntax-error`, `respan` is applied
;; automatically to the arguments. When `raise-syntax-error` is called
;; in a trampolining macro, though, like the way `for` and `class` are
;; implemented, then explicit `respan` may be needed. In those cases,
;; `respan` may also be needed for input to `syntax-parse`, in case it
;; is responsible for raising an exception when a match files.
;;
;; When a primitive expression form expands to a parsed term, it should
;; `relocate` the result using a `respan` of the input shrubbery. It's
;; tempting to try to propagate the raw form of an input shrubbery to
;; the output, but we don't try, currently. Note that `wrap-static-info`
;; and `wrap-static-info*` propagate a relocation from the wrapped
;; expression to the wrapper, so prefer to add the relocation inside,
;; and that works for both the wrapped and unwrapped forms.

;; Make a source location that spans `start` to `end`, assuming that
;; `end` is after `start`
(define (span-srcloc start end)
  (vector (syntax-source start)
          (syntax-line start)
          (syntax-column start)
          (syntax-position start)
          (let ([e (syntax-position end)]
                [s (syntax-position start)]
                [sp (syntax-span end)])
            (and s e sp
                 (max 0 (+ (- e s) sp))))))

(define (no-srcloc stx)
  (datum->syntax #f
                 (syntax-e stx)
                 #f
                 #f))

(define (no-srcloc* stx)
  (map no-srcloc (syntax->list stx)))

(define (relocate srcloc stx)
  (datum->syntax stx (syntax-e stx) srcloc stx #;(if (syntax? srcloc) srcloc stx)))

(define (relocate-id head id)
  (syntax-raw-property (relocate head id) (or (syntax-raw-property head)
                                              (symbol->string (syntax-e head)))))

;; If the tail is empty, give it a source location
;; that matches the end of `op-stx`
(define (respan-empty op-stx tail)
  (cond
    [(or (null? tail)
         (and (syntax? tail)
              (null? tail)))
     (define o-loc (syntax-srcloc op-stx))
     (cond
       [o-loc
        (define pos (srcloc-position o-loc))
        (define span (srcloc-span o-loc))
        (cond
          [(and pos span)
           (datum->syntax #f '() (srcloc (srcloc-source o-loc)
                                         #f #f
                                         (+ pos span)
                                         0))]
          [else tail])]
       [else tail])]
    [else tail]))

;; This function should work reliably when `stx` is a shrubbery
;; representation. It should also handle a syntax object that is
;;  list of terms; there's a danger of misinterpreting a `group`
;; or `multi` term as constructing a group or multi-group sequence,
;; so we against that by treating an identifier with a non-empty 'raw
;; property as not constructing a group or multi-gropu sequence.
(define (maybe-respan stx)
  (cond
    [(syntax-srcloc stx) stx]
    [else (respan stx)]))

(define (respan stx)
  (define e (syntax-e stx))
  (define (not-identifier-term? head)
    (define r (syntax-raw-property head))
    (not (and r (not (null? r)) (not (equal? r "")))))
  (define (block-tag? a)
    (and (eq? (syntax-e a) 'block)
         (or (equal? (syntax-raw-property a) ":")
             (equal? (syntax-raw-property a) "|"))))
  ;; look inside `stx` for `op` or 
  (define (term->stx stx)
    (define r (syntax-e stx))
    (cond
      [(pair? r)
       (define a (car r))
       (or (and (eq? (syntax-e a) 'op)
                (not-identifier-term? a)
                (let* ([d (cdr r)]
                       [d (if (syntax? d) (syntax-e d) d)])
                  (or (and (pair? d) (car d))
                      a)))
           (and (and (eq? (syntax-e a) 'alts)
                     (not-identifier-term? a))
                (maybe-respan stx))
           (and (memq (syntax-e a) '(parens brackets braces quotes))
                (maybe-respan stx))
           (and (block-tag? a)
                (maybe-respan stx))
           stx)]
      [else stx]))
  ;; compute span from a list of terms
  (define (from-list wrap-stx stxes element->stx)
    (define head (element->stx (car stxes)))
    (define pos (and head (syntax-position head)))
    (cond
      [pos
       (define end-pos (let loop ([stxes (cdr stxes)]
                                  [pos (and pos
                                            (+ pos
                                               (or (and head
                                                        (syntax-span head))
                                                   0)))])
                         (cond
                           [(null? stxes) pos]
                           [else
                            (define a (element->stx (car stxes)))
                            (define p (syntax-position a))
                            (define sp (syntax-span a))
                            (loop (cdr stxes)
                                  (if (and p sp
                                           (equal? (syntax-source head)
                                                   (syntax-source a)))
                                      (max pos (+ p sp))
                                      pos))])))
       (datum->syntax wrap-stx
                      (syntax-e wrap-stx)
                      (vector (syntax-source head)
                              (syntax-line head)
                              (syntax-column head)
                              pos
                              (max 0 (- end-pos pos)))
                      wrap-stx)]
      [else wrap-stx]))
  (cond
    [(pair? e)
     (define head (car e))
     (define v (syntax-e head))
     (cond
       [(and (eq? v 'group)
             (not-identifier-term? head)
             (syntax->list stx))
        => (lambda (l)
             (from-list stx (cdr l) term->stx))]
       [(and (or (eq? v 'multi)
                 (eq? v 'alts))
             (not-identifier-term? head)
             (syntax->list stx))
        => (lambda (l)
             (from-list stx (cdr l) (lambda (g)
                                      ;; we expect `g` to be a group or block
                                      (maybe-respan g))))]
       [(and (block-tag? head)
             (syntax->list stx))
        => (lambda (l)
             (from-list stx l (lambda (g)
                                ;; we expect `g` to be a group, usually,
                                ;; but it will be an identifier for the
                                ;; head of `l`
                                (maybe-respan g))))]
       [(syntax->list stx)
        => (lambda (l)
             ;; assume a list of terms
             (from-list stx l term->stx))]
       [else stx])]
    [else stx]))

(define-syntax-rule (with-syntax-error-respan body ...)
  (call-with-syntax-error-respan
   (lambda ()
     body ...)))

(define (call-with-syntax-error-respan thunk)
  (with-handlers ([exn:fail:syntax?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (define new-exprs (map maybe-respan exprs))
                     (raise
                      (if (equal? exprs new-exprs)
                          exn
                          (struct-copy exn:fail:syntax exn
                                       [exprs new-exprs]))))])
    (thunk)))
