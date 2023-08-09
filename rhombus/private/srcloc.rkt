#lang racket/base
(require shrubbery/srcloc
         shrubbery/property)

(provide syntax-srcloc
         no-srcloc
         no-srcloc*
         span-srcloc
         relocate
         relocate-id
         relocate+reraw
         respan-empty
         respan
         maybe-respan
         with-syntax-error-respan
         shift-origin)

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
;;  * `alts` is like `group`: it isn't expected to have a source
;;    location, although the shrubbery reader will associate a
;;    spanning source location to surrounding parentheses.
;;
;; "Respan" means to give a syntax object (i.e., the immediate wrapper)
;; a source location that corresponds to the content. That may involve
;; moving out a `parens`, etc., tag or walking through a `group` content
;; to create a source location that spans all the content, for example.
;; A "respan" operation can make sense for a group or unwrapped term
;; sequence, since the span can be reconstructed if the immediate wrapper
;; is lost; attaching information to the wrapper can act as a kind of
;; cache.
;;
;; For most calls to `raise-syntax-error`, `respan` is applied
;; automatically to the arguments. When `raise-syntax-error` is called
;; in a trampolining macro, though, like the way `for` and `class` are
;; implemented, then explicit `respan` may be needed. In those cases,
;; `respan` may also be needed for input to `syntax-parse`, in case it
;; is responsible for raising an exception when a match files.
;;
;; "Relocate" means to take source location from one syntax object and
;; use it for another. The target of a relocation is normally a
;; Racket-level expression or a single term, since relocating a group
;; or term/group sequence is fragile. Note, however, that
;; `Syntax.relocate` works on a group syntax object, especially to
;; transfer prefix and suffix information from one group to another
;; for especially careful transfers (e.g., as in the `rhombus` macro
;; in Scribble to preserve source formatting).
;;
;; "Reraw" means to take the printed form of a syntax object and
;; attach it as the opaque raw form of another syntax object. An
;; opaque raw property means that raw-text information of nested
;; syntax objects is ignored. Note that opaque-raw information is
;; *not* preserved by default, unlike raw information. The target of a
;; reraw operation should be a Racket expression or an invidual term,
;; so there's a clear place to attach and so the information does not
;; get lost.
;;
;; When a primitive expression form expands to a parsed term, it
;; should `relocate+reraw` the result using a `respan` of the input
;; shrubbery. Note that `wrap-static-info` and `wrap-static-info*`
;; propagate a relocation+reraw from the wrapped expression to the
;; wrapper, so prefer to add the relocation+reraw inside, and that
;; works for both the wrapped and unwrapped forms.

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

(define (relocate srcloc stx [prop-stx stx])
  (datum->syntax stx (syntax-e stx) srcloc prop-stx))

;; unlike `relocate`, copies props and potentially updates 'raw
(define (relocate-id head id)
  (syntax-raw-property (relocate head id head) (or (syntax-raw-property head)
                                                   (symbol->string (syntax-e head)))))

;; `stx` should be a Racket expression, while `src-stx` can be a srcloc
;; or a shrubbery form
(define (relocate+reraw src-stx stx)
  (cond
    [(syntax? src-stx)
     (define-values (pfx raw sfx) (extract-raw src-stx))
     (let* ([stx (syntax-opaque-raw-property (relocate (maybe-respan src-stx) stx) raw)]
            [stx (if (null? pfx)
                     stx
                     (syntax-raw-prefix-property stx pfx))]
            [stx (if (null? sfx)
                     stx
                     (syntax-raw-suffix-property stx sfx))])
       stx)]
    [else (relocate src-stx stx)]))

(define (extract-raw stx)
  (define (cons-raw a b)
    (cond
      [(or (not a) (null? a) (equal? a "")) (or b null)]
      [(or (not b) (null? b) (equal? b "")) a]
      [else (cons a b)]))
  (cond
    [(syntax? stx)
     (cond
       [(syntax-opaque-raw-property stx)
        (values (or (syntax-raw-prefix-property stx) null)
                (syntax-opaque-raw-property stx)
                (or (syntax-raw-suffix-property stx) null))]
       [(syntax->list stx)
        => (lambda (l) (extract-raw l))]
       [else
        (values (or (syntax-raw-prefix-property stx) null)
                (syntax-raw-property stx)
                (or (syntax-raw-suffix-property stx) null))])]
    [(and (pair? stx) (list? stx))
     (define tail (syntax-raw-tail-property (car stx)))
     (define tail-sfx (syntax-raw-tail-suffix-property (car stx)))
     (let loop ([stx stx] [accum null] [pre? #t] [sfx null])
       (cond
         [(null? stx)
          (if (null? (cons-raw tail '()))
              (values null accum (cons-raw sfx tail-sfx))
              (values null (cons-raw accum (cons-raw sfx tail)) tail-sfx))]
         [else
          (define-values (pfx raw new-sfx) (extract-raw (car stx)))
          (cond
            [pre?
             (define-values (no-pfx all-raw sfx) (loop (cdr stx)
                                                       (cons-raw accum raw)
                                                       #f
                                                       new-sfx))
             (values pfx all-raw sfx)]
            [else
             (loop (cdr stx)
                   (cons-raw (cons-raw accum sfx)
                             (cons-raw pfx raw))
                   #f
                   new-sfx)])]))]
    [else (values null null null)]))

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

(define (maybe-respan stx)
  (cond
    [(syntax-srcloc stx) stx]
    [else (respan stx)]))

;; This function should work reliably when `stx` is a shrubbery
;; representation. It should also handle a syntax object that is
;; list of terms; there's a danger of misinterpreting a `group`
;; or `multi` term as constructing a group or multi-group sequence,
;; so we against that by treating an identifier with a non-empty 'raw
;; property as not constructing a group or multi-gropu sequence.
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
           ;; concession to using `datum->syntax` in `Syntax.relocate_span`
           (and (and (or (eq? (syntax-e a) 'multi)
                         (eq? (syntax-e a) 'group))
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
             (if (null? (cdr l))
                 stx  ;; only happens with 'multi
                 (from-list stx (cdr l) (lambda (g)
                                          ;; we expect `g` to be a group or block
                                          (maybe-respan g)))))]
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

(define (shift-origin stx from-stx)
  (let ([o (syntax-property from-stx 'origin)])
    (if o
        (let ([o2 (syntax-property stx 'origin)])
          (syntax-property stx 'origin (if o2 (cons o o2) o)))
        stx)))
