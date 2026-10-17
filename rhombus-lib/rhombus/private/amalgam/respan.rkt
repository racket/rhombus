#lang racket/base
(require racket/symbol
         racket/syntax-srcloc
         shrubbery/property
         shrubbery/print)

(provide respan
         maybe-respan
         find-shrubberies)

(define (maybe-respan stx)
  (cond
    [(syntax-srcloc stx) stx]
    [else (respan stx)]))


;; This function should work reliably when `stx` is a shrubbery
;; representation. It should also handle a syntax object that is
;; list of terms; there's a danger of misinterpreting a `group`
;; or `multi` term as constructing a group or multi-group sequence,
;; so we against that by treating an identifier with a non-empty 'raw
;; property as not constructing a group or multi-group sequence.
(define (respan stx)
  (define stxes (find-shrubberies stx))

  ;; compute span from a nonempty list of shrubberies
  (define (from-list wrap-stx stxes)
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

  ;; extract from `op` and/or respan compound forms
  ;; that do not have a srclon on the wrapper
  (define (element->stx stx)
    (cond
      [(syntax-srcloc stx) stx]
      [else
       (define r (syntax-e stx))
       (cond
         [(and (pair? r)
               (syntax->list stx))
          => (lambda (l)
               (define a (car l))
               (case (syntax-e a)
                 [(op)
                  (if (and (pair? (cdr l))
                           (null? (cddr l)))
                      (cadr l)
                      a)]
                 [(group multi alts)
                  (if (null? (cdr l))
                      stx
                      (from-list stx (cdr l)))]
                 [(parsed)
                  (if (= 3 (length l))
                      (caddr l)
                      stx)]
                 [else
                  (if (syntax-raw-property a)
                      ;; use srcloc on head
                      a
                      ;; infer srcloc
                      (from-list stx l))]))]
         [else stx])]))

  (if (null? stxes)
      stx
      (from-list stx stxes)))

;; Extract a list of shrubberies from `stx`, even recurring into
;; subexpressions as needed. Rerawed non-shrubbery terms are treated
;; as shrubbery terms, since the intent is to pick up the recorded raw
;; text. To avoid confusing a `group` shrubbery container and a `group`
;; shrubbery identifier, we check `syntax-raw-property`; this is not
;; perfect, but it should only misinterpret an identifier in a term
;; sequence, and not go wrong with intact shrubbery forms.
(define (find-shrubberies stx)
  (define (filter-shrubberies l)
    (apply append (map find-shrubberies l)))
  (cond
    [(list? stx) (filter-shrubberies stx)]
    [(not (syntax? stx)) null]
    [(syntax-opaque-raw-property stx)
     (list stx)]
    [(null? (syntax-e stx)) null]
    [(not (pair? (syntax-e stx)))
     (cond
       [(syntax-raw-property stx) (list stx)]
       [else null])]
    [(syntax->list stx)
     => (lambda (l)
          (define head (car l))
          (case (syntax-e head)
            [(parens brackets braces quotes block alts op group multi parsed)
             (define r (syntax-raw-property head))
             (if (not (and r (equal? r (symbol->immutable-string (syntax-e head)))))
                 (list stx)
                 (filter-shrubberies l))]
            [else
             (filter-shrubberies l)]))]
    [else null]))
