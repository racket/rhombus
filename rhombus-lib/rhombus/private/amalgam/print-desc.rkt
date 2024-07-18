#lang racket/base
(require "realm.rkt"
         (prefix-in pe: pretty-expressive)
         (prefix-in sp: shrubbery/private/simple-pretty))


(provide (struct-out PrintDesc)
         (struct-out pretty-ref)
         current-print-as-pretty
         current-pretty-as-optimal
         current-page-width

         pretty-display
         pretty-write

         pretty-text
         pretty-special
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

(struct PrintDesc (doc) #:authentic #:sealed)

(struct pretty-ref ([doc #:mutable]) #:authentic #:sealed)

(define current-print-as-pretty (make-parameter #f
                                                (lambda (v) (and v #t))
                                                'Printable.current_pretty))
(define current-pretty-as-optimal (make-parameter #f
                                                  (lambda (v) (and v #t))
                                                  'Printable.current_optimal))
(define current-page-width
  (make-parameter 80
                  (lambda (v)
                    (unless (exact-nonnegative-integer? v)
                      (raise-argument-error* 'Printable.current_page_width rhombus-realm "NonnegInt" v))
                    v)
                  'Printable.current_page_width))

(define (pretty-display v [op/ht #f])
  (cond
    [(or (string? v) (bytes? v)) (pretty-text v)]
    [else (pretty-text (format "~a" v))]))

(define (pretty-write v [op/ht #f])
  (pretty-text (format "~s" v)))

(define (pretty-text str)
  str)

(define (pretty-special sp len mode doc)
  (list* 'special (list sp len mode) doc))

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

(define (resolve-references doc
                            build-text
                            built-nl
                            build-concat
                            build-alt
                            build-nest
                            build-align
                            build-special
                            can-special?)
  (define ht (make-hasheq))
  (define graph? (print-graph))
  (define (obviously-non-cyclic?)
    ;; a quick pre-test to see whether we need an extra pass
    (let loop ([doc doc] [saw-ht #hasheq()] [fuel 128])
      (cond
        [(eqv? fuel 0) #f]
        [(pretty-ref? doc)
         (and (not (hash-ref saw-ht doc #f))
              (loop (pretty-ref-doc doc) (hash-set saw-ht doc #t) fuel))]
        [(pair? doc)
         (case (car doc)
           [(or) (let ([fuel (loop (cadr doc) saw-ht (sub1 fuel))])
                   (and fuel (loop (caddr doc) saw-ht fuel)))]
           [(align flat) (loop (cadr doc) saw-ht (sub1 fuel))]
           [(nest) (loop (caddr doc) saw-ht (sub1 fuel))]
           [(seq)
            (let seq-loop ([docs (cdr doc)] [fuel (sub1 fuel)])
              (cond
                [(null? docs) fuel]
                [else (let ([fuel (loop (car docs) saw-ht fuel)])
                        (and fuel (seq-loop (cdr docs) fuel)))]))]
           [(special)
            (if can-special?
                (sub1 fuel)
                (loop (cddr doc) saw-ht (sub1 fuel)))]
           [else
            (error 'resolve-references "oops ~s" doc)])]
        [else (sub1 fuel)])))
  ;; a pass detects sharing and cycles, and it also removes/resolved `flat`
  ;; while ordering `or` to have a flat case as the first option
  (define (pass build? graph?)
    (define memo-ht (make-hash))
    (let loop ([doc doc] [saw-ht #hasheq()] [flat? #f] [doc-ht-in #f])
      (cond
        [(pretty-ref? doc)
         (cond
           [(hash-ref saw-ht doc #f)
            (define n (hash-ref ht doc (hash-count ht)))
            (hash-set! ht doc n)
            (values saw-ht
                    (if build?
                        (build-text (string-append "#" (number->string n) "#"))
                        'ok)
                    #t)]
           [else
            (define-values (new-saw-ht new-doc is-flat?)
              (loop (pretty-ref-doc doc) (hash-set saw-ht doc #t) flat? #f))
            (values (if graph? new-saw-ht saw-ht)
                    (and new-doc
                         (if build?
                             (cond
                               [(hash-ref ht doc #f)
                                => (lambda (n)
                                     (build-concat
                                      (list
                                       (build-text (string-append "#" (number->string n) "="))
                                       new-doc)))]
                               [else new-doc])
                             'ok))
                    is-flat?)])]
        [(pair? doc)
         (define doc-ht (or doc-ht-in
                            (hash-ref memo-ht (cons saw-ht flat?)
                                      (lambda ()
                                        (define ht (make-hasheq))
                                        (hash-set! memo-ht (cons saw-ht flat?) ht)
                                        ht))))
         (cond
           [(hash-ref doc-ht doc #f)
            => (lambda (p)
                 (values (caar p) (cdr p) (cdar p)))]
           [else
            (define-values (new-saw-ht new-doc is-flat?)
              (case (car doc)
                [(or)
                 (define-values (left-saw-ht left-doc left-is-flat?)
                   (loop (cadr doc) saw-ht flat? doc-ht))
                 (define-values (right-saw-ht right-doc right-is-flat?)
                   (loop (caddr doc) saw-ht flat? doc-ht))
                 (cond
                   [(not left-doc) (values right-saw-ht right-doc right-is-flat?)]
                   [(not right-doc) (values left-saw-ht left-doc left-is-flat?)]
                   [else (values left-saw-ht
                                 (if build?
                                     ;; put flat option on left to cooperate with
                                     ;; a greedy rendering algorithm
                                     (if (or left-is-flat? (not right-is-flat?))
                                         (build-alt left-doc right-doc)
                                         (build-alt right-doc left-doc))
                                     'ok)
                                 (and right-is-flat? left-is-flat?))])]
                [(align)
                 (define-values (new-saw-ht new-doc is-flat?) (loop (cadr doc) saw-ht flat? doc-ht-in))
                 (values new-saw-ht
                         (and new-doc
                              (if build?
                                  (build-align new-doc)
                                  'ok))
                         is-flat?)]
                [(nest)
                 (define-values (new-saw-ht new-doc is-flat?) (loop (caddr doc) saw-ht flat? doc-ht-in))
                 (values new-saw-ht
                         (and new-doc
                              (if build?
                                  (build-nest (cadr doc) new-doc)
                                  'ok))
                         is-flat?)]
                [(flat)
                 (define-values (new-saw-ht new-doc is-flat?) (loop (cadr doc) saw-ht #t (and flat? doc-ht-in)))
                 (values new-saw-ht new-doc #t)]
                [(seq)
                 (define-values (new-saw-ht rev-new-docs is-flat?)
                   (let seq-loop ([docs (cdr doc)] [saw-ht saw-ht] [rev-docs null] [is-flat? #t] [doc-ht-in doc-ht])
                     (cond
                       [(null? docs) (values saw-ht rev-docs is-flat?)]
                       [else
                        (define doc (car docs))
                        (define-values (new-saw new-doc new-is-flat?) (loop doc saw-ht flat? doc-ht-in))
                        (if new-doc
                            (seq-loop (cdr docs) new-saw (cons new-doc rev-docs) (and is-flat? new-is-flat?)
                                      (and (eq? saw-ht new-saw) doc-ht-in))
                            (values #f #f #f))])))
                 (values new-saw-ht
                         (and rev-new-docs
                              (if build?
                                  (build-concat (reverse rev-new-docs))
                                  'ok))
                         is-flat?)]
                [(special)
                 (cond
                   [can-special?
                    (values saw-ht (build-special (cadr doc)) flat?)]
                   [else
                    (loop (cddr doc) saw-ht flat? doc-ht-in)])]
                [else
                 (error 'resolve-references "oops ~s" doc)]))
            (hash-set! doc-ht doc (cons (cons new-saw-ht is-flat?) new-doc))
            (values new-saw-ht new-doc is-flat?)])]
        [(eq? doc 'nl) (if flat?
                           (values #f #f #f)
                           (values saw-ht built-nl #f))]
        [(bytes? doc) (values saw-ht (if build? (build-text (bytes->string/utf-8 doc)) 'ok) #t)]
        [else (values saw-ht (if build? (build-text doc) 'ok) #t)])))
  ;; discover graph references:
  (when (or graph?
            (not (obviously-non-cyclic?)))
    (pass #f graph?))
  ;; add graph tags:
  (define-values (final-saw-ht new-doc is-flat?) (pass #t (positive? (hash-count ht))))
  (unless new-doc (error 'print "no valid flat rendering"))
  new-doc)

(define (render-pretty doc o make-redirect
                       #:column [col 0])
  (if (current-pretty-as-optimal)
      (pe:pretty-print (resolve-references doc
                                           pe:text
                                           pe:nl
                                           pe:u-concat
                                           pe:alt
                                           pe:nest
                                           pe:align
                                           (lambda (spec) (pe:special spec (cadr spec)))
                                           (port-writes-special? o))
                        #:out o
                        #:offset col
                        #:page-width (current-page-width)
                        #:special (make-write-pretty-special make-redirect))
      (sp:render-pretty (resolve-references doc
                                            (lambda (str) str)
                                            'nl
                                            (lambda (lst) (cons 'seq lst))
                                            (lambda (a b) (list 'or a b))
                                            (lambda (n doc) (list 'nest n doc))
                                            (lambda (doc) (list 'align doc))
                                            (lambda (spec) (list 'special spec (cadr spec)))
                                            (port-writes-special? o))
                        o
                        #:write-special (make-write-pretty-special make-redirect)
                        #:width (and (current-print-as-pretty)
                                     (current-page-width))
                        #:column col)))

(define (make-write-pretty-special make-redirect)
  (lambda (spec o)
    (define v (car spec))
    (define mode (caddr spec))
    (case mode
      [(write-special) (write-special v o)]
      [(print) (print (make-redirect v) o)]
      [(write) (write v o)]
      [(display) (display v o)])))
