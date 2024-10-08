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
         "realm.rkt"
         "enum.rkt"
         (submod "print.rkt" for-port)
         "rhombus-primitive.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port))

(module+ for-builtin
  (provide input-port-method-table
           output-port-method-table
           output-string-port-method-table))

(define-primitive-class Port port
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([Input Port.Input]
   [Output Port.Output]
   EOF
   eof
   ReadLineMode)
  #:properties ()
  #:methods ())

(define-primitive-class Port.Input input-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([String Port.Input.String]
   [current current-input-port]
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

(define-primitive-class Port.Output output-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([String Port.Output.String]
   [current current-output-port]
   [current_error current-error-port]
   [open_bytes Port.Output.open_bytes]
   [open_string Port.Output.open_string]
   [get_bytes Port.Output.get_bytes]
   [get_string Port.Output.get_string]
   ExistsFlag)
  #:properties ()
  #:methods
  ([flush Port.Output.flush]
   [print Port.Output.print]
   [println Port.Output.println]
   [show Port.Output.show]
   [showln Port.Output.showln]))

(define (input-string-port? v)
  (and (input-port? v)
       (string-port? v)))

(define-annotation-syntax Port.Input.String
  (identifier-annotation input-string-port? #,(get-input-port-static-infos)))

(define (output-string-port? v)
  (and (output-port? v)
       (string-port? v)))

(void (set-primitive-subcontract! '(output-port? string-port?) 'output-string-port?))
(define-primitive-class Port.Output.String output-string-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent #f output-port
  #:fields ()
  #:namespace-fields
  (#:no-methods)
  #:properties ()
  #:methods
  ([get_bytes Port.Output.get_bytes]
   [get_string Port.Output.get_string]))

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
  linefeed
  return
  [return_linefeed return-linefeed]
  any
  [any_one any-one])

(define-simple-symbol-enum ExistsFlag
  error
  replace
  truncate
  [must_truncate must-truncate]
  [truncate_replace truncate/replace]
  update
  [can_update can-update]
  append)

(define (check-input-port who ip)
  (unless (input-port? ip)
    (raise-annotation-failure who ip "Port.Input")))

(define (check-output-port who op)
  (unless (output-port? op)
    (raise-annotation-failure who op "Port.Output")))

(define/arity Port.Input.open_bytes
  #:primitive (open-input-bytes)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (case-lambda
    [(bstr) (open-input-bytes bstr)]
    [(bstr name) (open-input-bytes bstr name)]))

(define/arity (Port.Input.open_file path)
  #:primitive (open-input-file)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (open-input-file path))

(define/arity Port.Input.open_string
  #:primitive (open-input-string)
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (case-lambda
    [(str) (open-input-string str)]
    [(str name) (open-input-string str name)]))

(define/arity Port.Output.open_bytes
  #:primitive (open-output-bytes)
  #:static-infos ((#%call-result #,(get-output-string-port-static-infos)))
  (case-lambda
    [() (open-output-bytes)]
    [(name) (open-output-bytes name)]))

(define/arity (Port.Output.open_file path #:exists [exists 'error])
  #:primitive (open-output-file)
  #:static-infos ((#%call-result #,(get-output-port-static-infos)))
  (open-output-file path #:exists (->ExistsFlag exists)))

(define/arity Port.Output.open_string
  #:primitive (open-output-string)
  #:static-infos ((#%call-result #,(get-output-string-port-static-infos)))
  (case-lambda
    [() (open-output-string)]
    [(name) (open-output-string name)]))

(define (coerce-read-result v)
  (cond
    [(string? v) (string->immutable-string v)]
    [else v]))

(define/method (Port.Input.peek_byte port #:skip_bytes [skip 0])
  #:primitive (peek-byte)
  (peek-byte port skip))

(define/method (Port.Input.peek_bytes port amt #:skip_bytes [skip 0])
  #:primitive (peek-bytes)
  (peek-bytes amt skip port))

(define/method (Port.Input.peek_char port #:skip_bytes [skip 0])
  #:primitive (peek-char)
  (peek-char port skip))

(define/method (Port.Input.peek_string port amt #:skip_bytes [skip 0])
  #:primitive (peek-bytes)
  (coerce-read-result
   (peek-string amt skip port)))

(define/method (Port.Input.read_byte port)
  #:primitive (read-byte)
  (read-byte port))

(define/method (Port.Input.read_bytes port amt)
  #:primitive (read-bytes)
  (read-bytes amt port))

(define/method (Port.Input.read_char port)
  #:primitive (read-char)
  (read-char port))

(define/method (Port.Input.read_line port
                                     #:mode [mode-in 'any])
  #:primitive (read-line)
  (define mode (->ReadLineMode mode-in))
  (unless mode
    (check-input-port who port)
    (raise-annotation-failure who mode-in "Port.Input.ReadLineMode"))
  (coerce-read-result
   (read-line port mode)))

(define/method (Port.Input.read_string port amt)
  #:primitive (read-string)
  (coerce-read-result (read-string amt port)))

(define/method (Port.Output.get_bytes port)
  #:primitive (get-output-bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (get-output-bytes port))

(define/method (Port.Output.get_string port)
  #:primitive (get-output-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (get-output-string port)))

(define/method (Port.Output.flush [p (current-output-port)])
  (check-output-port who p)
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
