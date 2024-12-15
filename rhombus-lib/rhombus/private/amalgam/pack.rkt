#lang racket/base
(require syntax/stx
         enforest/proc-name
         shrubbery/property
         "treelist.rkt"
         "to-list.rkt"
         "realm.rkt"
         "annotation-failure.rkt"
         "srcloc.rkt"
         "syntax-wrap.rkt")

;; We represent Rhombus syntax objects as a syntax object with one of
;; the following forms:
;;   (multi (group term ...) ...) - as a general multi-group syntax object
;;   (group term ...) - for a group syntax object
;;   term - for a single-term syntax object
;; Coercions among these happen automatically. So, the Rhombus expression
;;   'x'
;; could be represented for most purposes as #'(multi (group x)) or just #'x.

;; "Pack" means going form the internal Racket-side representation to
;; the Rhombus representation, such as when sending syntax to a
;; Rhombus-implemented macro, and "unpack" means going back.

;; "Tail" below refers to the argument to a macro for the rest of the
;; enclosing sequence. We represent it using `multi` on the Rhombus
;; side, but it's just a syntax list on the Racket side, which means
;; adding both `group` and them `multi` to get from here to there. On
;; the way back, we coerce from any of the three possible shapes,
;; containing `multi` to a single group.

;; When we "unpack" here, group unpacking is asymmetric and is really
;; just about coercsions, mot about removing a `group` wrapper.

(provide pack-term
         unpack-term
         unpack-term/maybe
         pack-group
         unpack-group
         pack-multi
         pack-tagged-multi
         unpack-multi

         pack-tail
         unpack-tail
         pack-multi-tail
         unpack-multi-tail

         pack-group-or-empty
         unpack-group-or-empty

         pack-term*
         unpack-term*
         unpack-maybe-term*
         unpack-term-list*
         pack-group*
         unpack-group*
         unpack-maybe-group*
         unpack-group-list*
         pack-multi*
         pack-tagged-multi*
         pack-block*
         unpack-multi*
         unpack-multi-as-term*

         pack-tail*
         unpack-tail*
         pack-multi-tail*
         unpack-multi-tail*

         pack-group-or-empty*
         unpack-group-or-empty*

         pack-tail-list*
         unpack-tail-list*
         pack-multi-tail-list*
         unpack-multi-tail-list*
         unpack-list-tail*
         unpack-multi-list-tail*

         pack-element*
         unpack-element*
         pack-nothing*
         pack-parsed*
         unpack-parsed*
         
         repack-as-term
         repack-as-multi

         insert-multi-front-group
         insert-multi-front-head-group
         check-valid-group)

(define multi-blank (syntax-raw-property (datum->syntax #f 'multi) null))
(define group-blank (syntax-raw-property (datum->syntax #f 'group) null))

(define (group-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'group (syntax-e (car (syntax-e r))))))

(define (multi-syntax? r)
  (and (syntax? r)
       (pair? (syntax-e r))
       (eq? 'multi (syntax-e (car (syntax-e r))))))

;; ----------------------------------------

;; `stx` is a single term
(define (pack-term stx) stx)

;; Unpacking is mostly about coercing from different representations.
;; The result is always a syntax object.
(define (unpack-term form who at-stx)
  (define (fail)
    (raise-error who "multi-term syntax not allowed in term context" form))
  (let loop ([r form])
    (cond
      [(syntax-wrap? r) (loop (syntax-unwrap r))]
      [(multi-syntax? r)
       (define l (syntax->list r))
       (if (and (pair? (cdr l)) (null? (cddr l)))
           (loop (cadr l))
           (fail))]
      [(group-syntax? r)
       (define l (syntax->list r))
       (if (and (pair? (cdr l)) (null? (cddr l)))
           (cadr l)
           (fail))]
      [(or (treelist? r) (list? r)) (cannot-coerce-list who r)]
      [else (datum->syntax at-stx r)])))

;; For meta functions that take terms as arguments
(define (unpack-term/maybe v)
  (and (syntax*? v)
       (unpack-term v #f #f)))

;; "Packs" to a `group` form, but `r` starts with `group` already
(define (pack-group r) r)

;; "Packs" to a `group` or empty `multi` form, where `r` starts with `group` already,
;; but it might be empty
(define (pack-group-or-empty r)
  (if (stx-null? (cdr (syntax-e r)))
      (datum->syntax #f (list multi-blank))
      r))

;; "Unpacks" to a `(group term ...)` form, as opposed to just `(term
;; ...)`, which makes it symmetric with `pack-group` and preserves
;; properties on the `group` tag. So, unpacking here is really about
;; coercing from different representations, as opposed to changing a
;; `group` representation. The result is always a syntax object.
(define (unpack-group r who at-stx)
  (cond
    [(syntax-wrap? r) (unpack-group (syntax-unwrap r) who at-stx)]
    [(multi-syntax? r)
     (define l (syntax->list r))
     (cond
       [(and (pair? (cdr l)) (null? (cddr l))) (cadr l)]
       [else (raise-error who "multi-group syntax not allowed in group context" r)])]
    [(group-syntax? r) r]
    [(to-list #f r)
     => (lambda (es)
          (when (null? es) (cannot-coerce-empty-list who r))
          (define elems (for/list ([e (in-list es)])
                          (unpack-term e who at-stx)))
          (check-valid-group who elems '())
          (and elems
               (datum->syntax #f (cons group-blank elems))))]
    [else (datum->syntax #f (list group-blank (datum->syntax at-stx r)))]))

;; "Unpacks" to a `group` form that might be empty
(define (unpack-group-or-empty r who at-stx)
  (cond
    [(syntax-wrap? r) (unpack-group (syntax-unwrap r) who at-stx)]
    [(and (multi-syntax? r)
          (stx-null? (cdr (syntax-e r))))
     (datum->syntax #f (list group-blank))]
    [else
     (unpack-group r who at-stx)]))

;; make sure 'block or 'alts doesn't end up mid-group in the list `terms`
;; where `tail` (list or syntax) does not need to be checked but supplies
;; a continuation of the list
(define (check-valid-group who terms tail)
  (let loop ([terms terms])
    (cond
      [(null? terms) (void)]
      [else
       (define e (car terms))
       (define rest-terms (cdr terms))
       (define d (syntax-e e))
       (cond
         [(not (pair? d)) (loop rest-terms)]
         [else
          (define t (syntax-e (car d)))
          (cond
            [(eq? t 'alts)
             (cond
               [(and (null? rest-terms) (stx-null? tail))
                #t]
               [else
                (raise-error who
                             "alternatives not allowed in non-tail position of a group"
                             e)])]
            [(eq? t 'block)
             (cond
               [(and (null? rest-terms)
                     (stx-null? tail))
                (void)]
               [else
                (define b (syntax-e (car (if (null? rest-terms)
                                             (if (syntax? tail)
                                                 (syntax-e tail)
                                                 tail)
                                             rest-terms))))
                (cond
                  [(and (pair? b)
                        (eq? 'alts (syntax-e (car b))))
                   (when (pair? rest-terms)
                     (loop rest-terms))]
                  [else
                   (raise-error who
                                "block not allowed in non-tail position of a group (unless just before alternatives)"
                                e)])])]
            [else (loop rest-terms)])])])))

;; this function makes sure the list is valid as a group
;; (i.e., has no 'block or 'alts in non-tail position), although
;; that check may be redundant with an enclosing check when
;; used in the middle of a template
(define (unpack-term-list r who at-stx)
  (cond
    [(syntax-wrap? r) (unpack-term-list (syntax-unwrap r) who at-stx)]
    [(syntax? r)
     (cond
       [(multi-syntax? r)
        (define l (syntax->list r))
        (cond
          [(null? (cdr l)) null]
          [(null? (cddr l)) (cdr (syntax->list (cadr l)))]
          [else (raise-error who "multi-group syntax not allowed in group context" r)])]
       [(group-syntax? r) (cdr (syntax->list r))]
       [else (list r)])]
    [(or (and (treelist? r) (treelist->list r))
         (and (list? r) r))
     => (lambda (l)
          (define terms (for/list ([e (in-list l)])
                          (unpack-term e who at-stx)))
          (check-valid-group who terms '())
          terms)]
    [else (list (datum->syntax at-stx r))]))

;; Unpacks a multi-group sequence into a list of groups,
;; but otherwise produces a list with one group. A list is
;; treated as a list of elements instead of a list of groups
(define (unpack-group-list r who at-stx)
  (cond
    [(syntax-wrap? r) (unpack-group-list (syntax-unwrap r) who at-stx)]
    [(and (syntax? r)
          (multi-syntax? r))
     (cdr (syntax->list r))]
    [else (list (unpack-group r who at-stx))]))

;; `r` is a sequence of groups
(define (pack-multi r)
  (datum->syntax #f (cons multi-blank r)))

;; `r` is a term like `(parens ....)` or `(block ....)`, and
;; the `parens` or `block` part gets changed to `multi`
(define (pack-tagged-multi r)
  (datum->syntax #f (cons multi-blank (stx-cdr r))))

;; `r` can be any of the allowed representations (multi-group,
;; single-group, or single-term), and the result is a list of group
;; syntax objects (symmetric to `pack-multi`); the result is always a
;; plain list of syntax objects
(define (unpack-multi r who at-stx)
  (cond
    [(syntax-wrap? r) (unpack-multi (syntax-unwrap r) who at-stx)]
    [(multi-syntax? r) (cdr (syntax->list r))]
    [(group-syntax? r) (list r)]
    [(to-list #f r)
     => (lambda (es)
          (cond
            [(null? es) null]
            [else
             ;; unpack assuming a list of elements instead of a list of groups;
             ;; this means that we don't really have a multi-group splicing form,
             ;; but that constraint avoids ambiguity
             (list (unpack-group es who at-stx))]))]
    [else (list (datum->syntax #f (list group-blank (datum->syntax at-stx r))))]))

;; assumes that `tail` is a syntax list of terms, and wraps it with `multi`;
;; an empty list turns into `multi` with no groups
(define (pack-tail tail #:after [after #f])
  (if (stx-null? tail)
      (cond
        [(and after
              (syntax-position after)
              (syntax-span after))
         (define loc (srcloc (syntax-source after)
                             (syntax-line after)
                             (let ([col (syntax-column after)])
                               (and col (+ col (syntax-span after))))
                             (+ (syntax-position after) (syntax-span after))
                             0))
         (datum->syntax #f (list (syntax-property (syntax/loc loc multi) 'raw "")))]
        [else (datum->syntax #f (list multi-blank))])
      (datum->syntax #f (list multi-blank (cons group-blank tail)))))

;; unpacks by removing `multi` and/or `group` wrapper to arrive at a
;; sequence of terms; the result is a list-shaped syntax object
(define (unpack-tail r who at-stx)
  (datum->syntax
   #f
   (let ([r (syntax-unwrap r)])
     (cond
       [(multi-syntax? r)
        (define l (syntax->list r))
        (cond
          [(null? (cdr l)) '()]
          [(null? (cddr l)) (cdr (syntax-e (cadr l)))]
          [else (raise-error who "multi-group syntax not allowed in group context" r)])]
       [(group-syntax? r) (cdr (syntax-e r))]
       [(pair? r) (cannot-coerce-pair who r)]
       [else (list (datum->syntax at-stx r))]))))

;; similar to `pack-tail` but for a list of groups, so no
;; special case for empty is needed
(define (pack-multi-tail tail)
  (datum->syntax #f (cons multi-blank tail)))

;; produces a sequence of groups
(define (unpack-multi-tail r who at-stx)
  (datum->syntax
   #f
   (let ([r (syntax-unwrap r)])
     (cond
       [(multi-syntax? r) (cdr (syntax-e r))]
       [(group-syntax? r) (list r)]
       [(pair? r) (cannot-coerce-pair who r)]
       [else (list (datum->syntax at-stx r))]))))

;; ----------------------------------------

;; The `pack*` and `unpack*` variants deal with repetition
;; `depth` layers deep as needed for patterns and templates;
;; we can assume that the lists and nesting have a suitable
;; shape, because they come from `syntax-parse` or from a
;; repetition binding

(define (pack* stx depth wrap)
  (cond
    [(eqv? depth 0) (wrap stx)]
    [else (for/list ([t (in-list (syntax->list stx))])
            (pack* t (sub1 depth) wrap))]))

(define (unpack* qs r depth unpack)
  (let unpack* ([r r] [depth depth])
    (cond
      [(eqv? depth 0) (unpack r (syntax-e qs) qs)]
      [(treelist? r)
       (for/list ([r (in-treelist r)])
         (unpack* r (sub1 depth)))]
      [(list? r)
       (for/list ([r (in-list r)])
         (unpack* r (sub1 depth)))]
      [else
       (raise-annotation-failure (syntax-e qs) r "Listable")])))

(define (pack-term* stx depth)
  (pack* stx depth pack-term))

(define (unpack-term* qs r depth)
  (unpack* qs r depth unpack-term))

(define (unpack-maybe-term* qs r depth)
  (unpack* qs r depth (lambda (form who at-stx)
                        (and form
                             (unpack-term form who at-stx)))))

;; responsible for checking group validity, although the
;; check is redundant when used in a non-tail position
(define (unpack-term-list* qs r depth)
  (unpack* qs r depth unpack-term-list))

;; Packs to a `group` form
(define (pack-group* stx depth)
  (pack* stx depth pack-group))

;; Packs to a `group` or empty `multi` form
(define (pack-group-or-empty* stx depth)
  (pack* stx depth pack-group-or-empty))

;; "Unpacks" to a `group` form, which is really more about coercsions
(define (unpack-group* qs r depth)
  (unpack* qs r depth unpack-group))

;; "Unpacks" to a `group` form, potentially empty
(define (unpack-group-or-empty* qs r depth)
  (unpack* qs r depth unpack-group-or-empty))

(define (unpack-maybe-group* qs r depth)
  (unpack* qs r depth (lambda (form who at-stx)
                        (and form
                             (unpack-group form who at-stx)))))

(define (unpack-group-list* qs r depth)
  (unpack* qs r depth unpack-group-list))

;; Packs to a `multi` form
(define (pack-multi* stxes depth)
  (pack* stxes depth pack-multi))

;; Packs to a `multi` form, because that's the useful result from matching
(define (pack-tagged-multi* stxes depth)
  (pack* stxes depth pack-tagged-multi))

;; Unpacks a multi to a list
(define (unpack-multi* qs r depth)
  (unpack* qs r depth unpack-multi))

;; Unpacks a multi to a `multi` form, instead of a list
(define (unpack-multi-as-term* qs r depth)
  (unpack* qs r depth (lambda (r name qs)
                        (datum->syntax #f (cons multi-blank (unpack-multi r name qs))))))

(define (pack-element* r depth)
  (pack* r depth (lambda (r) r)))

(define (unpack-element* qs r depth)
  (unpack* qs r depth (lambda (r name qs) r)))

;; like `pack-element*`, but assuming the right shape already
(define (pack-nothing* r depth) r)

;; `stx` comes from Racket, so it should be in `parsed`
(define ((pack-parsed kw) stx)
  (relocate+reraw stx #`(parsed #,kw #,stx)))

(define ((pack-parsed* kw) r depth)
  (pack* r depth (pack-parsed kw)))

(define ((unpack-parsed* kw) qs r depth)
  (unpack-element* qs r depth))

;; An extra layer of unpacking to convert to a list
(define (unpack-multi-group* qs r depth)
  (cond
    [(= depth 0) r]
    [else (unpack-multi-as-term* qs r (sub1 depth))]))

;; Like `pack-multi*, but preserves `block` instead of converting to `multi`:
(define (pack-block* stxes depth)
  (pack* stxes depth (lambda (r) r)))

(define (pack-tail* stxes depth)
  (pack* stxes depth pack-tail))

(define (unpack-tail* qs r depth)
  (unpack* qs r depth unpack-tail))

(define (pack-multi-tail* stxes depth)
  (pack* stxes depth pack-multi-tail))

(define (unpack-multi-tail* qs r depth)
  (unpack* qs r depth unpack-multi-tail))

;; similar to `unpack-tail*`, but converts each tail syntax
;; list to a plain list, and expects `depth` to be off by 1
(define (unpack-tail-list* qs r depth)
  (unpack* qs r (sub1 depth) (lambda (r name qs)
                               (syntax->list (unpack-tail r (syntax-e qs) qs)))))

;; similar to `unpack-multi-tail*`, but converts each multi-tail syntax
;; list to a plain list, and expects `depth` to be off by 1
(define (unpack-multi-tail-list* qs r depth)
  (unpack* qs r (sub1 depth) (lambda (r name qs)
                               (syntax->list (unpack-multi-tail r (syntax-e qs) qs)))))

(define (pack-tail-list* stxes depth)
  (pack* stxes (sub1 depth) pack-tail))

(define (pack-multi-tail-list* stxes depth)
  (pack* stxes (sub1 depth) pack-multi-tail))

;; similar to `unpack-tail*`, but each leaf is a plain list of term splices;
;; also responsible for checking validity of the result
(define (unpack-list-tail* qs r depth)
  (unpack* qs r depth (lambda (r name qs)
                        (define terms (apply append (unpack-term-list* qs r 1)))
                        (check-valid-group (syntax-e qs) terms '())
                        terms)))

;; similar to `unpack-multi-tail*`, but each leaf is a plain list of groups
(define (unpack-multi-list-tail* qs r depth)
  (unpack* qs r depth (lambda (r name qs)
                        (apply append (unpack-group-list* qs r 1)))))

;; normalize for multi-term pattern matching:
(define (repack-as-multi r)
  (cond
    [(syntax-wrap? r) (repack-as-multi (syntax-unwrap r))]
    [(group-syntax? r) (list multi-blank r)]
    [(multi-syntax? r) r]
    [else (list multi-blank (list group-blank r))]))

;; normalize for single-term pattern matching:
(define (repack-as-term r)
  (or (unpack-term r #f #f)
      ;; can't match a term pattern:
      #'(group)))

;; insert `term` at the front of the first group, and strip away
;; `multi` and `group` wrappers for a splicing match; this improves
;; error reporting when a match fails; the `error-syntax->name-handler`
;; parameter also solves this problem, and we should instead rely on
;; the parameter when it becomes reliably available
(define (insert-multi-front-group term r)
  (cond
    [(syntax-wrap? r) (insert-multi-front-group term (syntax-unwrap r))]
    [(group-syntax? r) (cons term
                             (cdr (syntax-e r)))]
    [(multi-syntax? r) (let ([r (cdr (syntax-e r))])
                         (if (stx-null? r)
                             (list term)
                             (if (stx-null? (stx-cdr r))
                                 (insert-multi-front-group term (stx-car r))
                                 (error "unexpected multi-group sequence for macro match"))))]
    [else (list term r)]))

(define (insert-multi-front-head-group head r)
  (cond
    [(syntax-wrap? r) (insert-multi-front-head-group head (syntax-unwrap r))]
    [(multi-syntax? r) (datum->syntax
                        #f
                        (cons (stx-car r)
                              (cons head
                                    (stx-cdr r))))]
    [else (error "unexpected multi-group sequence")]))

(define (->name v)
  (cond
    [(syntax? v) (syntax-e v)]
    [(procedure? v) (proc-name v)]
    [else v]))

(define (cannot-coerce-list who r)
  (and who
       (raise-arguments-error* (->name who)
                               rhombus-realm
                               "cannot coerce list to syntax"
                               "list" r)))

(define (cannot-coerce-empty-list who r)
  (and who
       (raise-arguments-error* (->name who)
                               rhombus-realm
                               "cannot coerce empty list to group syntax")))

(define (cannot-coerce-pair who r)
  (and who
       (raise-arguments-error* (->name who)
                               rhombus-realm
                               "cannot coerce pair to syntax"
                               "pair" r)))

(define (raise-error who msg r)
  (cond
    [(procedure? who)
     (raise
      (exn:fail:contract
       (error-message->adjusted-string
        (proc-name who)
        rhombus-realm
        (string-append
         "invalid macro result"
         ";\n " msg
         "\n  received: " ((error-value->string-handler)
                           (syntax-unwrap r)
                           (error-print-width)))
        rhombus-realm)
       (current-continuation-marks)))]
    [who
     (raise-arguments-error* (->name who)
                             rhombus-realm
                             msg
                             "syntax" (syntax-unwrap r))]
    [else #f]))
