#lang racket/base

(provide (struct-out PrintDesc)
         (struct-out pretty-ref)

         pretty-display
         pretty-write

         pretty-text
         pretty-concat
         pretty-concat-list
         pretty-flat
         pretty-newline
         pretty-nest
         pretty-align
         pretty-or
         
         pretty-listlike
         pretty-blocklike

         resolve-references)

(struct PrintDesc (doc))

(struct pretty-ref ([doc #:mutable]))

(define (pretty-display v [op/ht #f])
  (cond
    [(or (string? v) (bytes? v)) (pretty-text v)]
    [else (pretty-text (format "~a" v))]))

(define (pretty-write v [op/ht #f])
  (pretty-text (format "~s" v)))

(define (pretty-text str)
  str)

(define (pretty-concat . docs)
  (pretty-concat-list docs))

(define (pretty-concat-list docs)
  (cons 'seq docs))

(define (pretty-flat doc)
  (list 'flat doc))

(define (pretty-newline)
  'nl)

(define (pretty-nest n b)
  (list 'nest n b))

(define (pretty-align a)
  (list 'align a))

(define (pretty-or a b)
  (list 'or a b))

(define (pretty-listlike pre elems post)
  (cond
    [(null? elems)
     (pretty-concat pre post)]
    [else
     (pretty-or
      ;; single-line layout, allowing only last element to be multi-line
      (pretty-concat pre
                      (pretty-align
                       (pretty-concat-list
                        (let loop ([elems elems])
                          (cond
                            [(null? elems) null]
                            [(null? (cdr elems)) elems]
                            [else (list* (pretty-flat (car elems)) ", " (loop (cdr elems)))]))))
                      post)
      ;; multi-line layout, all elements can be multi-line
      (let ([elems (pretty-align
                    (pretty-concat-list
                     (let loop ([elems elems])
                       (cond
                         [(null? elems) null]
                         [(null? (cdr elems)) elems]
                         [else (list* (car elems) "," (pretty-newline) (loop (cdr elems)))]))))])
        ;; two multi line options: first element on same line as opener, or newline
        ;; (and possible outdent) for arguments
        (pretty-or (pretty-concat pre elems post)
                   (pretty-concat pre
                                   (pretty-nest 2 (pretty-concat (pretty-newline) elems))
                                   (pretty-concat (pretty-newline) post)))))]))

(define (pretty-blocklike head body)
  (pretty-or
   (pretty-concat (pretty-flat head) (pretty-text ": ") body)
   (pretty-concat head (pretty-text ":")
                   (pretty-nest 2 (pretty-concat (pretty-newline)
                                                 body)))))

(define (resolve-references doc)
  (define ht (make-hasheq))
  (define graph? (print-graph))
  (define (pass build?)
    (define memo-ht (make-hasheq))
    (let loop ([doc doc] [saw-ht #hasheq()])
      (cond
        [(pretty-ref? doc)
         (cond
           [(hash-ref saw-ht doc #f)
            (define n (hash-ref ht doc (hash-count ht)))
            (hash-set! ht doc n)
            (values saw-ht
                    (and build?
                         (pretty-text (string-append "#" (number->string n) "#"))))]
           [else
            (define-values (new-saw-ht new-doc)
              (loop (pretty-ref-doc doc) (hash-set saw-ht doc #t)))
            (values (if graph? new-saw-ht saw-ht)
                    (and build?
                         (cond
                           [(hash-ref ht doc #f)
                            => (lambda (n)
                                 (pretty-concat
                                  (pretty-text (string-append "#" (number->string n) "="))
                                  new-doc))]
                           [else new-doc])))])]
        [(list? doc)
         (cond
           [(hash-ref (hash-ref memo-ht doc #hash()) saw-ht #f)
            => (lambda (p)
                 (values (car p) (cdr p)))]
           [else
            (define-values (new-saw-ht new-doc)
              (case (car doc)
                [(or)
                 (define-values (left-saw-ht left-doc)
                   (loop (cadr doc) saw-ht))
                 (define-values (right-saw-ht right-doc)
                   (loop (caddr doc) saw-ht))
                 (values left-saw-ht
                         (and build?
                              `(or ,left-doc ,right-doc)))]
                [else
                 (define-values (new-saw-ht rev-new-doc)
                   (for/fold ([saw-ht saw-ht] [rev-doc null]) ([doc (in-list (cdr doc))])
                     (define-values (new-saw new-doc) (loop doc saw-ht))
                     (values new-saw (cons new-doc rev-doc))))
                 (values new-saw-ht
                         (and build?
                              (cons (car doc) (reverse rev-new-doc))))]))
            (hash-set! memo-ht
                       doc
                       (hash-set (hash-ref memo-ht doc #hash()) saw-ht (cons new-saw-ht new-doc)))
            (values new-saw-ht new-doc)])]
        [else (values saw-ht doc)])))
  ;; discover graph references:
  (pass #f)
  ;; add graph tags:
  (define-values (final-saw-ht new-doc) (pass #t))
  new-doc)
