#lang at-exp racket/base
(require scribble/manual
         (for-label racket/base
                    racket/match))

(provide racket_define_syntax
         racket_syntax_local_context
         racket_provide_for_space
         racket_match
         racket_q_parsed)
         
(define racket_define_syntax @racket[define-syntax])
(define racket_syntax_local_context @racket[syntax-local-context])
(define racket_match @racket[match])
(define racket_provide_for_space @racket[(provide (for-space ....))])
(define racket_q_parsed @racket['parsed])