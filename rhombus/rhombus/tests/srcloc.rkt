#lang racket/base
(require racket/string
         raco/testing)

(define (try prog-str in-str #:rx [rx #f])
  (define e
    (with-handlers ([exn:fail:syntax? (lambda (x) x)])
      (parameterize ([read-accept-reader #t]
                     [error-print-source-location #t]
                     [current-namespace (make-base-namespace)])
        (define p (open-input-string prog-str))
        (port-count-lines! p)
        (expand (read-syntax "x.rhm" p)))))
  (define ok?
    (and (exn? e)
         (string-find (exn-message e) (format "in: ~a" in-str))
         (or (not rx)
             (regexp-match? rx (exn-message e))))  )
  (test-log! ok?)
  (unless ok?
    (fprintf (current-error-port)
             "wrong error for ~s: ~s" prog-str (and (exn? e) (exn-message e)))))

(try "#lang rhombus\ndef x = 1\ndef x = 2"
     "def x = 2")

(try "#lang rhombus\ndef x = 1\ndef [x, y] = 2"
     "def [x, y] = 2")

(try "#lang rhombus\ndef x = 1\ndef [x, ...] = 2"
     "def [x, ...] = 2")

(try "#lang rhombus\ndef x = 1\nclass Posn(x, y)\ndef Posn(x, _) = 2"
     "def Posn(x, _) = 2")

(try "#lang rhombus\nblock:\n  def x = 1\n  def x = 2"
     "def x = 2"
     #:rx #rx"def: ")

(try "#lang rhombus\nblock:\n  def x = 1\n  def [x, ...] = 2"
     "def [x, ...] = 2"
     #:rx #rx"def: ")

(try "#lang rhombus\nblock:\n  def x = 1\n  let [x, x] = 2"
     "x"
     #:rx #rx"x: ")

(try "#lang rhombus\nblock:\n  class C()\n  class C()"
     "class C()"
     #:rx #rx"class: ")

(try "#lang rhombus/and_meta\nblock:\n  annot.macro 'C': 'Int'\n  annot.macro 'C': 'Int'"
     "annot.macro 'C': 'Int'"
     #:rx #rx"annot.macro: ")

(try "#lang rhombus\nblock:\n  class C()\n  interface C"
     "interface C"
     #:rx #rx"interface: ")

(try "#lang rhombus\nblock:\n  class C()\n  veneer C(this :: Int)"
     "veneer C(this :: Int)"
     #:rx #rx"veneer: ")

(try "#lang rhombus\nblock:\n  class C()\n  veneer C(this :: ReadableString.to_string)"
     "veneer C(this :: ReadableString.to_string)"
     #:rx #rx"veneer: ")
