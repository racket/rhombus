#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         (submod "annotation.rkt" for-class)
         "call-result-key.rkt"
         "function-arity-key.rkt"
         (submod "bytes.rkt" static-infos)
         (submod "string.rkt" static-infos)
         "static-info.rkt"
         "define-arity.rkt"
         (submod "function.rkt" for-info)
         "class-primitive.rkt"
         "realm.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port))

(define-annotation-syntax Eof (identifier-annotation eof-object? ()))

(module+ for-builtin
  (provide input-port-method-table
           output-port-method-table))

(define-primitive-class Port port
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  (Input
   Output
   Eof
   eof
   ;; TEMP see `Input` and `Output`
   [current_input current-input-port]
   [current_output current-output-port]
   [current_error current-error-port])
  #:properties ()
  #:methods ())

(define-primitive-class Input input-port
  #:lift-declaration
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  ([current current-input-port]
   [open_bytes Port.Input.open_bytes]
   [open_string Port.Input.open_string])
  #:properties ()
  #:methods
  ([peek_byte Port.Input.peek_byte]
   [peek_char Port.Input.peek_char]
   [read_byte Port.Input.read_byte]
   [read_bytes Port.Input.read_bytes]
   [read_char Port.Input.read_char]
   [read_line Port.Input.read_line]))

(define-primitive-class Output output-port
  #:lift-declaration
  #:existing
  #:translucent
  #:fields ()
  #:namespace-fields
  ([current current-output-port]
   [current_error current-error-port]
   [open_bytes Port.Output.open_bytes]
   [open_string Port.Output.open_string])
  #:properties ()
  #:methods
  ([get_bytes Port.Output.get_bytes]
   [get_string Port.Output.get_string]
   [flush Port.Output.flush]))

(define-static-info-syntaxes (current-input-port current-output-port current-error-port)
  (#%function-arity 3)
  . #,(get-function-static-infos))

(define/arity (Port.Input.open_bytes bstr)
  #:inline
  #:primitive (open-input-bytes)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (open-input-bytes bstr))

(define/arity (Port.Input.open_string str)
  #:inline
  #:primitive (open-input-string)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (open-input-string str))

;; TODO these need a more specific annotation
(define/arity Port.Output.open_bytes
  #:inline
  #:primitive (open-output-bytes)
  #:static-infos ((#%call-result #,(get-output-port-static-infos)))
  (case-lambda
    [() (open-output-bytes)]
    [(name) (open-output-bytes name)]))

(define/arity Port.Output.open_string
  #:inline
  #:primitive (open-output-string)
  #:static-infos ((#%call-result #,(get-output-port-static-infos)))
  (case-lambda
    [() (open-output-string)]
    [(name) (open-output-string name)]))

(define/method (Port.Input.peek_byte port [skip 0])
  #:inline
  #:primitive (peek-byte)
  (peek-byte port skip))

(define/method (Port.Input.peek_char port)
  #:inline
  #:primitive (peek-char)
  (peek-char port))

(define/method (Port.Input.read_byte port)
  #:inline
  #:primitive (read-byte)
  (read-byte port))

(define/method (Port.Input.read_bytes port amt)
  #:inline
  #:primitive (read-bytes)
  (read-bytes amt port))

(define/method (Port.Input.read_char port)
  #:inline
  #:primitive (read-char)
  (read-char port))

(define/method (Port.Input.read_line port [mode 'linefeed])
  #:inline
  #:primitive (read-line)
  (string->immutable-string (read-line port mode)))

(define/method (Port.Output.get_bytes port)
  #:inline
  #:primitive (get-output-bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (get-output-bytes port))

(define/method (Port.Output.get_string port)
  #:inline
  #:primitive (get-output-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (get-output-string port)))

(define/method (Port.Output.flush [p (current-output-port)])
  (unless (output-port? p)
    (raise-argument-error* who rhombus-realm "Port.Output" p))
  (flush-output p))
