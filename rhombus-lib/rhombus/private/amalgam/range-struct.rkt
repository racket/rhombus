#lang racket/base

(provide (struct-out range)
         (struct-out sequence-range)
         (struct-out list-range))

(module+ descending
  (provide (struct-out descending-range)))

(struct range () #:authentic)
(struct sequence-range range () #:authentic)
(struct list-range sequence-range () #:authentic)

(struct descending-range () #:authentic)
