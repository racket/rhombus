#lang racket/base

(provide (struct-out range)
         (struct-out sequence-range)
         (struct-out list-range))

(struct range () #:authentic)
(struct sequence-range range () #:authentic)
(struct list-range sequence-range () #:authentic)
