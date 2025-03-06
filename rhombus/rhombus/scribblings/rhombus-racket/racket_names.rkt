#lang at-exp racket/base
(require scribble/manual
         (for-label racket/base
                    rhombus/parse
                    racket/shared))

(provide racket_require
         racket_lib
         racket_file
         racketmod_racket_math
         racketmod_rhombus_random
         racketmod_rhombus_parse
         racketmod_racket_treelist
         racketmod_racket_mutable_treelist
         racket_rhombus_expression
         racket_shared
         racket_cons)

(define racket_require @racket[require])
(define racket_lib @racket[lib])
(define racket_file @racket[file])

(define racketmod_racket_math @racketmodname[racket/math])
(define racketmod_rhombus_random @racketmodname[(lib "rhombus/random.rhm")])
(define racketmod_rhombus_parse @racketmodname[rhombus/parse])
(define racketmod_racket_treelist @racketmodname[racket/treelist])
(define racketmod_racket_mutable_treelist @racketmodname[racket/mutable-treelist])

(define racket_rhombus_expression @racket[rhombus-expression])
(define racket_shared @racket[shared])
(define racket_cons @racket[cons])
