#lang racket/base
(require racket/class
         "armor.rkt")

(provide shrubbery-keystrokes)

(define shrubbery-keystrokes
  (list (list "c:x;a"
              toggle-armor)))
