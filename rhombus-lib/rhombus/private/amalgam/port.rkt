#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
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
         "realm.rkt"
         "enum.rkt"
         (submod "print.rkt" for-port))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port))

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
   EOF
   eof
   ReadLineMode)
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
   [peek_bytes Port.Input.peek_bytes]
   [peek_char Port.Input.peek_char]
   [peek_string Port.Input.peek_string]
   [read_byte Port.Input.read_byte]
   [read_bytes Port.Input.read_bytes]
   [read_char Port.Input.read_char]
   [read_line Port.Input.read_line]
   [read_string Port.Input.read_string]))

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
   [flush Port.Output.flush]
   [print Port.Output.print]
   [println Port.Output.println]
   [show Port.Output.show]
   [showln Port.Output.showln]))

(define-static-info-syntax current-input-port
  (#%function-arity 3)
  (#%call-result #,(get-input-port-static-infos))
  . #,(get-function-static-infos))

(define-static-info-syntaxes (current-output-port current-error-port)
  (#%function-arity 3)
  (#%call-result #,(get-output-port-static-infos))
  . #,(get-function-static-infos))

(define-annotation-syntax EOF (identifier-annotation eof-object? ()))

(define-simple-symbol-enum ReadLineMode
  any any_one linefeed return return_linefeed)

(define/arity Port.Input.open_bytes
  #:inline
  #:primitive (open-input-bytes)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (case-lambda
    [(bstr) (open-input-bytes bstr)]
    [(bstr name) (open-input-bytes bstr name)]))

(define/arity Port.Input.open_string
  #:inline
  #:primitive (open-input-string)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (case-lambda
    [(str) (open-input-string str)]
    [(str name) (open-input-string str name)]))

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

(define (coerce-read-result v)
  (cond
    [(string? v) (string->immutable-string v)]
    [else v]))

(define/method (Port.Input.peek_byte port #:skip_bytes [skip 0])
  #:inline
  #:primitive (peek-byte)
  (peek-byte port skip))

(define/method (Port.Input.peek_bytes port amt #:skip_bytes [skip 0])
  #:inline
  #:primitive (peek-bytes)
  (peek-bytes amt skip port))

(define/method (Port.Input.peek_char port #:skip_bytes [skip 0])
  #:inline
  #:primitive (peek-char)
  (peek-char port skip))

(define/method (Port.Input.peek_string port amt #:skip_bytes [skip 0])
  #:inline
  #:primitive (peek-bytes)
  (coerce-read-result
   (peek-string amt skip port)))

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


(define/method Port.Input.read_line
  #:inline
  #:primitive (read-line)
  (case-lambda
    [(port) (coerce-read-result (read-line port 'any))]
    [(port mode)
     (unless (ReadLineMode? mode)
       (unless (input-port? port)
         (raise-argument-error* who rhombus-realm "Port.Input" port))
       (raise-argument-error* who rhombus-realm "Port.Input.ReadLineMode" mode))
     (coerce-read-result
      (read-line port
                 (case mode
                   [(return_linefeed) 'return-linefeed]
                   [(any_one) 'any-one]
                   [else mode])))]))

(define/method (Port.Input.read_string port amt)
  #:inline
  #:primitive (read-string)
  (coerce-read-result (read-string amt port)))

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

(define/method (Port.Output.print port
                                  #:mode [mode 'text]
                                  #:pretty [pretty? default-pretty]
                                  . vs)
  (do-print* who vs port mode pretty?))

(define/method (Port.Output.println port
                                    #:mode [mode 'text]
                                    #:pretty [pretty? default-pretty]
                                    . vs)
  (do-print* who vs port mode pretty?)
  (newline port))

(define/method (Port.Output.show port
                                 #:pretty [pretty? default-pretty]
                                 . vs)
  (do-print* who vs port 'expr pretty?))

(define/method (Port.Output.showln port
                                   #:pretty [pretty? default-pretty]
                                   . vs)
  (do-print* who vs port 'expr pretty?)
  (newline port))
