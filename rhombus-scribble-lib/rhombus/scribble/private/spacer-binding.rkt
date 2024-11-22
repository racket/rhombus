#lang racket/base

(provide (struct-out spacer-binding))

(struct spacer-binding (datum
                        ns-mpi ns-sym ns-nom-mpi ns-nom-sym
                        ns-phase ns-import-phase ns-export-phase)
  #:prefab)
