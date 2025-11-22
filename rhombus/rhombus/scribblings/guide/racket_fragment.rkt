#lang racket
(require scribble/manual
         (for-label racket/base
                    setup/dirs))

(provide racket_require_setup_dirs
         racket_find_bin_dir)

(define racket_require_setup_dirs
  (racket (require setup/dirs)))

(define racket_find_bin_dir
  (racket (find-user-console-bin-dir)))
