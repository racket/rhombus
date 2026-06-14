#lang racket/base
(require racket/file
         racket/path)

(provide merge-dir-catalog)

(define (merge-dir-catalog version dest-dir from-dest-dir)
  (define (move-path p)
    (path->string (simplify-path (build-path (find-relative-path dest-dir from-dest-dir) p) #f)))

  (define (merge-version ht extra-ht)
    (define sub-ht (hash-ref ht 'versions #hash()))
    (define new-sub-ht (hash-set sub-ht
                                 version
                                 (for/hash ([(k raw-v) (in-hash extra-ht)]
                                            #:do [(define v (if (eq? k 'source)
                                                                (move-path raw-v)
                                                                raw-v))]
                                            #:unless (equal? v (hash-ref ht k #f)))
                                   (values k v))))
    (hash-set ht 'versions new-sub-ht))

  (define from-pkg-dir (build-path from-dest-dir "pkg"))
  (for ([pkg (in-list (directory-list from-pkg-dir))])
    (define pkg-file (build-path dest-dir "pkg" pkg))
    (define ht (call-with-input-file* pkg-file read))
    (define from-pkg-file (build-path from-pkg-dir pkg))
    (define extra-ht (call-with-input-file* from-pkg-file read))
    (define new-ht (merge-version ht extra-ht))
    (call-with-output-file* pkg-file #:exists 'truncate (lambda (o) (writeln new-ht o)))))
