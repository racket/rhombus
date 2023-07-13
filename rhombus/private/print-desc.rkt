#lang racket/base

(require (prefix-in pe: pretty-expressive))

(provide (struct-out PrintDesc)
         (struct-out pretty-ref)
         current-print-as-pretty

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

         render-pretty)

(struct PrintDesc (doc))

(struct pretty-ref ([doc #:mutable]))

(define current-print-as-pretty (make-parameter #t
                                                (lambda (v) (and v #t))
                                                'Printable.current_as_pretty))

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
  (if (string? doc)
      doc
      (list 'flat doc)))

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
     ;; single-line layout, allowing only last element to be multi-line
     (define single-line
       (pretty-concat pre
                      (pretty-align
                       (pretty-concat-list
                        (let loop ([elems elems])
                          (cond
                            [(null? elems) null]
                            [(null? (cdr elems)) elems]
                            [else (list* (pretty-flat (car elems)) ", " (loop (cdr elems)))]))))
                      post))
     (cond
       [(current-print-as-pretty)
        (pretty-or
         single-line
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
                                     (pretty-concat (pretty-newline) post)))))]
       [else single-line])]))

(define (pretty-blocklike head body)
  (pretty-or
   (pretty-concat (pretty-flat head) (pretty-text ": ") (pretty-flat body))
   (pretty-concat head (pretty-text ":")
                  (pretty-nest 2 (pretty-concat (pretty-newline)
                                                body)))))

(define (resolve-references doc)
  (define ht (make-hasheq))
  (define graph? (print-graph))
  ;; to use dumb printer, uncomment and also change `render-pretty`
  #;
  (begin
    (define (pe:text str) str)
    (define pe:nl 'nl)
    (define (pe:u-concat lst) (cons 'seq lst))
    (define (pe:alt a b) (list 'or a b))
    (define (pe:nest n doc) (list 'nest n doc))
    (define (pe:align doc) (list 'align doc))
    (define (pe:flat doc) doc))
  (define (pass build?)
    (define memo-ht (make-hash))
    (let loop ([doc doc] [saw-ht #hasheq()] [doc-ht-in #f])
      (cond
        [(pretty-ref? doc)
         (cond
           [(hash-ref saw-ht doc #f)
            (define n (hash-ref ht doc (hash-count ht)))
            (hash-set! ht doc n)
            (values saw-ht
                    (and build?
                         (pe:text (string-append "#" (number->string n) "#"))))]
           [else
            (define-values (new-saw-ht new-doc)
              (loop (pretty-ref-doc doc) (hash-set saw-ht doc #t) #f))
            (values (if graph? new-saw-ht saw-ht)
                    (and build?
                         (cond
                           [(hash-ref ht doc #f)
                            => (lambda (n)
                                 (pe:u-concat
                                  (list
                                   (pe:text (string-append "#" (number->string n) "="))
                                   new-doc)))]
                           [else new-doc])))])]
        [(list? doc)
         (define doc-ht (or doc-ht-in
                            (hash-ref memo-ht saw-ht (lambda ()
                                                       (define ht (make-hasheq))
                                                       (hash-set! memo-ht saw-ht ht)
                                                       ht))))
         (cond
           [(hash-ref doc-ht doc #f)
            => (lambda (p)
                 (values (car p) (cdr p)))]
           [else
            (define-values (new-saw-ht new-doc)
              (case (car doc)
                [(or)
                 (define-values (left-saw-ht left-doc)
                   (loop (cadr doc) saw-ht doc-ht))
                 (define-values (right-saw-ht right-doc)
                   (loop (caddr doc) saw-ht doc-ht))
                 (values left-saw-ht
                         (and build?
                              (pe:alt left-doc right-doc)))]
                [(align)
                 (define-values (new-saw-ht new-doc) (loop (cadr doc) saw-ht doc-ht-in))
                 (values new-saw-ht
                         (and build?
                              (pe:align new-doc)))]
                [(nest)
                 (define-values (new-saw-ht new-doc) (loop (caddr doc) saw-ht doc-ht-in))
                 (values new-saw-ht
                         (and build?
                              (pe:nest (cadr doc) new-doc)))]
                [(flat)
                 (define-values (new-saw-ht new-doc) (loop (cadr doc) saw-ht doc-ht-in))
                 (values new-saw-ht
                         (and build?
                              (pe:flat new-doc)))]
                [(seq)
                 (define-values (new-saw-ht rev-new-doc new-doc-ht)
                   (for/fold ([saw-ht saw-ht] [rev-doc null] [doc-ht-in doc-ht]) ([doc (in-list (cdr doc))])
                     (define-values (new-saw new-doc) (loop doc saw-ht doc-ht-in))
                     (values new-saw (cons new-doc rev-doc) (and (eq? saw-ht new-saw) doc-ht-in))))
                 (values new-saw-ht
                         (and build?
                              (pe:u-concat (reverse rev-new-doc))))]
                [else
                 (error 'resolve-references "oops ~s" doc)]))
            (hash-set! doc-ht doc (cons new-saw-ht new-doc))
            (values new-saw-ht new-doc)])]
        [(eq? doc 'nl) (values saw-ht pe:nl)]
        [(bytes? doc) (values saw-ht (and build? (pe:text (bytes->string/utf-8 doc))))]
        [else (values saw-ht (and build? (pe:text doc)))])))
  ;; discover graph references:
  (pass #f)
  ;; add graph tags:
  (define-values (final-saw-ht new-doc) (pass #t))
  new-doc)

(define (render-pretty doc o
                       #:column [col 0]
                       #:indent [indent 0])
  ;; to use dumb printer, uncomment and also change `resolve-references`
  #;
  (begin
    (define (pe:pretty-print pe-doc #:out o #:offset col)
      (local-require shrubbery/private/simple-pretty)
      (render-pretty pe-doc o
                     #:multi-line? #t
                     #:column col)))
  (pe:pretty-print (resolve-references doc)
                   #:out o
                   #:offset col))
