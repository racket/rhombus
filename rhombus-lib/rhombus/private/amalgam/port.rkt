#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print)
         racket/private/port
         "../version-case.rkt"
         "expression.rkt"
         "pipe-port.rkt"
         "provide.rkt"
         (submod "annotation.rkt" for-class)
         "call-result-key.rkt"
         "function-arity-key.rkt"
         (submod "bytes.rkt" static-infos)
         (submod "string.rkt" static-infos)
         "static-info.rkt"
         "define-arity.rkt"
         (submod "parameter.rkt" for-info)
         (submod "map.rkt" for-info)
         "class-primitive.rkt"
         "enum.rkt"
         "binding.rkt"
         "literal.rkt"
         (submod "print.rkt" for-port)
         "port-using.rkt"
         "rename-parameter.rkt"
         "rhombus-primitive.rkt"
         "error-adjust.rkt"
         "evt.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Port)
         stdin
         stdout
         stderr)

(module+ for-builtin
  (provide input-port-method-table
           output-port-method-table
           output-string-port-method-table
           file-stream-port-method-table
           pipe-port-method-table
           input-progress-port-method-table
           output-special-port-method-table))

(define-primitive-class Port port
  #:lift-declaration
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([Input Port.Input]
   [Output Port.Output]
   [FileStream Port.FileStream]
   [Pipe Port.Pipe]
   EOF
   [eof Port.eof]
   [open_input_output_file Port.open_input_output_file]
   [current_enable_locations Port.current_enable_locations]
   Mode
   BufferMode
   WaitMode)
  #:properties ()
  #:methods
  (close
   is_closed
   buffer
   position
   locations_enabled
   next_location
   name))

(define-primitive-class Port.Input input-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Port port
  #:fields ()
  #:namespace-fields
  ([String Port.Input.String]
   [Progress Port.Input.Progress]
   [current Port.Input.current]
   [open_bytes Port.Input.open_bytes]
   [open_file Port.Input.open_file]
   [open_string Port.Input.open_string]
   [open_nowhere Port.Input.open_nowhere]
   [using Port.Input.using]
   ReadLineMode)
  #:properties ()
  #:methods
  ([close Port.Input.close]
   [peek_byte Port.Input.peek_byte]
   [peek_bytes Port.Input.peek_bytes]
   [peek_bytes_to Port.Input.peek_bytes_to]
   [peek_char Port.Input.peek_char]
   [peek_string Port.Input.peek_string]
   [peek_string_to Port.Input.peek_string_to]
   [read_byte Port.Input.read_byte]
   [read_bytes Port.Input.read_bytes]
   [read_bytes_to Port.Input.read_bytes_to]
   [read_char Port.Input.read_char]
   [read_line Port.Input.read_line]
   [read_bytes_line Port.Input.read_bytes_line]
   [read_string Port.Input.read_string]
   [read_string_to Port.Input.read_string_to]
   [copy_to Port.Input.copy_to]))

(define-primitive-class Port.Input.Progress input-progress-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Input.Port input-port
  #:fields ()
  #:namespace-fields
  ()
  #:properties ()
  #:methods
  ([evt Port.Input.Progress.evt]
   [is_evt Port.Input.Progress.is_evt]
   [commit Port.Input.Progress.commit]))

(define-primitive-class Port.Output output-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Port port
  #:fields ()
  #:namespace-fields
  ([String Port.Output.String]
   [Special Port.Output.Special]
   [current Port.Output.current]
   [current_error Port.Output.current_error]
   [open_bytes Port.Output.open_bytes]
   [open_file Port.Output.open_file]
   [open_string Port.Output.open_string]
   [get_bytes Port.Output.String.get_bytes #:deprecate (#f rhombus/statinfo) "15-Apr-2025"]
   [get_string Port.Output.String.get_string #:deprecate (#f rhombus/statinfo) "15-Apr-2025"]
   [open_nowhere Port.Output.open_nowhere]
   ExistsMode
   [using Port.Output.using])
  #:properties ()
  #:methods
  ([close Port.Output.close]
   [flush Port.Output.flush]
   [write_byte Port.Output.write_byte]
   [write_char Port.Output.write_char]
   [write_bytes Port.Output.write_bytes]
   [write_string Port.Output.write_string]
   [print Port.Output.print]
   [println Port.Output.println]
   [show Port.Output.show]
   [showln Port.Output.showln]))

(define-primitive-class Port.Output.Special output-special-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Output.Port output-port
  #:fields ()
  #:namespace-fields
  ()
  #:properties ()
  #:methods
  ([write Port.Output.Special.write]))

(define-primitive-class Port.FileStream file-stream-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Port port
  #:fields ()
  #:namespace-fields
  ([Terminal Port.FileStream.Terminal]
   LockMode)
  #:properties ()
  #:methods
  (identity
   stat
   truncate
   is_waiting_on_peer
   try_lock
   unlock))

(define-primitive-class Port.Pipe pipe-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent Port port
  #:fields ()
  #:namespace-fields
  ([make Port.Pipe.make])
  #:properties ()
  #:methods
  (content_length))

(define (input-string-port? v)
  (and (input-port? v)
       (string-port? v)))

(define (input-progress-port? p)
  (and (input-port? p)
       (port-provides-progress-evts? p)))

(define (output-special-port? p)
  (and (output-port? p)
       (port-writes-special? p)))

(define-annotation-syntax Port.Input.String
  (identifier-annotation input-string-port? #,(get-input-port-static-infos)))

(define-annotation-syntax Port.FileStream.Terminal
  (identifier-annotation terminal-port? #,(get-port-static-infos)))

(define-annotation-syntax Port.Output.Atomic
  (identifier-annotation output-atomic-port? #,(get-output-port-static-infos)))

(define (output-string-port? v)
  (and (output-port? v)
       (string-port? v)))

(void (set-primitive-contract! '(and/c output-port? string-port?) "Port.Output.String"))
(define-primitive-class Port.Output.String output-string-port
  #:lift-declaration
  #:existing
  #:just-annot
  #:parent #f output-port
  #:fields ()
  #:namespace-fields
  ()
  #:properties ()
  #:methods
  (get_bytes
   get_string))

(define Port.Input.current (rename-parameter current-input-port 'Port.Input.current))
(set-primitive-who! 'current-input-port 'Port.Input.current)
(define Port.Output.current (rename-parameter current-output-port 'Port.Output.current))
(set-primitive-who! 'current-output-port 'Port.Output.current)
(define Port.Output.current_error (rename-parameter current-error-port 'Port.Output.current_error))
(set-primitive-who! 'current-error-port 'Port.Output.current_error)

(define Port.current_enable_locations
  (rename-parameter port-count-lines-enabled 'Port.current_enable_locations))

(define-static-info-syntax Port.Input.current
  (#%call-result (#:at_arities
                  ((1 #,(get-input-port-static-infos))
                   (2 ()))))
  . #,(get-parameter-static-infos))

(define-static-info-syntaxes (Port.Output.current Port.Output.current_error)
  (#%call-result (#:at_arities
                  ((1 #,(get-output-port-static-infos))
                   (2 ()))))
  . #,(get-parameter-static-infos))

(define-annotation-syntax EOF (identifier-annotation eof-object? ()))

(define Port.eof eof)
(define-binding-syntax Port.eof
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(form-id . tail)
        (values (binding-form #'literal-infoer
                              #`([#,eof #,(shrubbery-syntax->string #'form-id)]))
                #'tail)]))))

(define-simple-symbol-enum Mode
  binary
  text)

(define-simple-symbol-enum ReadLineMode
  linefeed
  return
  [return_linefeed return-linefeed]
  any
  [any_one any-one])

(define-simple-symbol-enum ExistsMode
  error
  replace
  truncate
  [must_truncate must-truncate]
  [truncate_replace truncate/replace]
  update
  [can_update can-update]
  append)

(define-simple-symbol-enum BufferMode
  none
  line
  block)

(define-simple-symbol-enum LockMode
  shared
  exclusive)

(define-simple-symbol-enum WaitMode
  all
  some
  none
  enable_break)

(define (check-port who op)
  (unless (port? op)
    (raise-annotation-failure who op "Port")))

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

(define/arity (Port.Input.open_file path
                                    #:mode [mode 'binary])
  #:primitive (open-input-file)
  #:static-infos ((#%call-result #,(static-infos-and
                                    (get-input-port-static-infos)
                                    (get-file-stream-port-static-infos))))
  (open-input-file path #:mode mode))

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

(meta-if-version-at-least
 "8.15.0.2"
 (void)
 (define (open-input-nowhere name)
   (open-input-bytes #"" name)))

(define/arity (Port.Input.open_nowhere #:name [name 'nowhere])
  #:static-infos ((#%call-result #,(get-input-port-static-infos)))
  (open-input-nowhere name))

(define/arity (Port.Output.open_file path
                                     #:exists [exists-in 'error]
                                     #:mode [mode 'binary]
                                     #:permissions [permissions #o666]
                                     #:replace_permissions [replace-permissions? #f])
  #:primitive (open-output-file)
  #:static-infos ((#%call-result #,(static-infos-and
                                    (get-output-port-static-infos)
                                    (get-file-stream-port-static-infos))))
  (define exists (->ExistsMode exists-in))
  (unless exists
    (raise-annotation-failure who exists-in "Port.Output.ExistsMode"))
  (open-output-file path
                    #:exists exists
                    #:mode mode
                    #:permissions permissions
                    #:replace-permissions? replace-permissions?))

(define/arity Port.Output.open_string
  #:primitive (open-output-string)
  #:static-infos ((#%call-result #,(get-output-string-port-static-infos)))
  (case-lambda
    [() (open-output-string)]
    [(name) (open-output-string name)]))

(define/arity (Port.Output.open_nowhere #:name [name 'nowhere])
  #:static-infos ((#%call-result #,(get-output-port-static-infos)))
  (open-output-nowhere name))

(define/arity (Port.open_input_output_file path
                                           #:exists [exists-in 'error]
                                           #:mode [mode 'binary]
                                           #:permissions [permissions #o666]
                                           #:replace_permissions [replace-permissions? #f])
  #:primitive (open-input-output-file)
  #:static-infos ((#%call-result ((#%values (#,(get-input-port-static-infos)
                                             #,(get-output-port-static-infos))))))
  (define exists (->ExistsMode exists-in))
  (unless exists
    (raise-annotation-failure who exists-in "Port.Output.ExistsMode"))
  (open-input-output-file path
                          #:exists exists
                          #:mode mode
                          #:permissions permissions
                          #:replace-permissions? replace-permissions?))

(define/arity (Port.Pipe.make #:limit [limit #f]
                              #:input_name [input-name 'pipe]
                              #:output_name [output-name 'pipe])
  #:primitive (make-pipe)
  #:static-infos ((#%call-result ((#%values (#,(static-infos-and
                                                (get-input-port-static-infos)
                                                (get-pipe-port-static-infos))
                                             #,(static-infos-and
                                                (get-output-port-static-infos)
                                                (get-pipe-port-static-infos)))))))
  (make-pipe limit input-name output-name))

(define/method (Port.Pipe.content_length p)
  #:primitive (pipe-content-length)
  (pipe-content-length p))

(define/method (Port.is_closed p)
  #:primitive (port-closed?)
  (port-closed? p))

(define/method (Port.close port)
  (cond
    [(input-port? port) (close-input-port port)]
    [(output-port? port) (close-output-port port)]
    [else (raise-annotation-failure who port "Port")]))

(define/method Port.buffer
  #:primitive (file-stream-buffer-mode)
  (case-lambda
    [(p)
     (check-port who p)
     (file-stream-buffer-mode p)]
    [(p m)
     (check-port who p)
     (unless (BufferMode? m) (raise-annotation-failure who m "BufferMode"))
     (file-stream-buffer-mode p m)]))

(define/method Port.position
  #:primitive (port-position)
  (case-lambda
    [(p)
     (check-port who p)
     (file-position p)]
    [(p pos)
     (check-port who p)
     (unless (exact-nonnegative-integer? pos) (raise-annotation-failure who pos "NonnegInt"))
     (file-position p pos)]))

(define/method Port.locations_enabled
  #:primitive (port-count-lines! port-counts-lines?)
  (case-lambda
    [(p) (port-counts-lines? p)]
    [(p on?) (when on? (port-count-lines! p))]))

(define/method Port.next_location
  #:primitive (port-next-location)
  (case-lambda
    [(p) (port-next-location p)]
    [(p line col offset)
     (with-error-adjust-primitive ([set-port-next-location! Port.next_location])
       (set-port-next-location! p line col offset))]))

(define/method (Port.name p)
  (check-port who p)
  (object-name p))

(define/method (Port.FileStream.identity p)
  #:primitive (port-file-identity)
  (port-file-identity p))

(meta-if-version-at-least
 "8.15.0.6"
 (void)
 (define (port-file-stat p)
   (error 'Port.FileStream.stat "unsupported")))

(define/method (Port.FileStream.stat p)
  #:primitive (port-file-stat)
  #:static-infos ((#%call-result #,(get-map-static-infos)))
  (port-file-stat p))

(define/method (Port.FileStream.truncate p size)
  #:primitive (file-truncate)
  (file-truncate p size))

(define/method (Port.FileStream.is_waiting_on_peer p)
  #:primitive (file-truncate)
  (unless (file-stream-port? p) (raise-annotation-failure who p "Port.FileStream"))
  (port-waiting-peer? p))

(define/method (Port.FileStream.try_lock p mode)
  #:primitive (port-try-file-lock?)
  (unless (file-stream-port? p) (raise-annotation-failure who p "Port.FileStream"))
  (unless (LockMode? mode) (raise-annotation-failure who mode "Port.FileStream.LockMode"))
  (port-try-file-lock? p mode))

(define/method (Port.FileStream.unlock p)
  #:primitive (port-file-unlock)
  (port-file-unlock p))

(define (coerce-read-result v)
  (cond
    [(string? v) (string->immutable-string v)]
    [else v]))

(define/method (Port.Input.close port)
  #:primitive (close-input-port)
  (close-input-port port))

(define/method (Port.Input.peek_byte port
                                     #:skip_bytes [skip 0]
                                     #:special_wrap [special-wrap #f]
                                     #:source_name [source-name #f])
  #:primitive (peek-byte peek-byte-or-special)
  (if special-wrap
      (peek-byte-or-special port skip (if (eq? special-wrap values) #f special-wrap) source-name)
      (peek-byte port skip)))

(define (check-wait who wait)
  (unless (or (eq? wait 'all) (eq? wait 'some) (eq? wait 'none) (eq? wait 'enable_break))
    (raise-annotation-failure who wait "Port.WaitMode")))

(define/method (Port.Input.peek_bytes port amt
                                      #:skip_bytes [skip 0])
  #:primitive (peek-bytes)
  (peek-bytes amt skip port))

(define/method (Port.Input.peek_bytes_to port bstr
                                         #:start [start 0]
                                         #:end [end (and (bytes? bstr) (bytes-length bstr))]
                                         #:skip_bytes [skip 0]
                                         #:wait [wait 'all]
                                         #:progress [progress #f])
  #:primitive (peek-bytes! peek-bytes-avail! peek-bytes-avail!* peek-bytes-avail!/enable-break)
  (cond
    [(eq? wait 'all)
     (when progress (raise-arguments-error who "progress evt not supported in wait mode" "wait mode" wait))
     (peek-bytes! bstr skip port start end)]
    [(eq? wait 'some)
     (peek-bytes-avail! bstr skip progress port start end)]
    [(eq? wait 'none)
     (peek-bytes-avail!* bstr skip progress port start end)]
    [(eq? wait 'enable_break)
     (peek-bytes-avail!* bstr skip progress port start end)]
    [else (check-wait who wait)]))

(define/method (Port.Input.peek_char port
                                     #:skip_bytes [skip 0]
                                     #:special_wrap [special-wrap #f]
                                     #:source_name [source-name #f])
  #:primitive (peek-char peek-char-or-special)
  (if special-wrap
      (peek-char-or-special port skip (if (eq? special-wrap values) #f special-wrap) source-name)
      (peek-char port skip)))

(define/method (Port.Input.peek_string port amt #:skip_bytes [skip 0])
  #:primitive (peek-bytes)
  (coerce-read-result
   (peek-string amt skip port)))

(define/method (Port.Input.peek_string_to port str
                                          #:start [start 0]
                                          #:end [end (and (string? str) (string-length str))]
                                          #:skip_bytes [skip 0])
  #:primitive (peek-string!)
  (peek-string! str skip port start end))

(define/method (Port.Input.read_byte port
                                     #:special_wrap [special-wrap #f]
                                     #:source_name [source-name #f])
  #:primitive (read-byte read-byte-or-special)
  (if special-wrap
      (read-byte-or-special port (if (eq? special-wrap values) #f special-wrap) source-name)
      (read-byte port)))

(define/method (Port.Input.read_bytes port amt)
  #:primitive (read-bytes)
  (read-bytes amt port))

(define/method (Port.Input.read_bytes_to port bstr
                                         #:start [start 0]
                                         #:end [end (and (bytes? bstr) (bytes-length bstr))]
                                         #:wait [wait 'all])
  #:primitive (read-bytes! read-bytes-avail! read-bytes-avail!* read-bytes-avail!/enable-break)
  (cond
    [(eq? wait 'all)
     (read-bytes! bstr port start end)]
    [(eq? wait 'some)
     (read-bytes-avail! bstr port start end)]
    [(eq? wait 'none)
     (read-bytes-avail!* bstr port start end)]
    [(eq? wait 'enable_break)
     (read-bytes-avail!* bstr port start end)]
    [else (check-wait who wait)]))

(define/method (Port.Input.read_char port
                                     #:special_wrap [special-wrap #f]
                                     #:source_name [source-name #f])
  #:primitive (read-char read-char-or-special)
  (if special-wrap
      (read-char-or-special port (if (eq? special-wrap values) #f special-wrap) source-name)
      (read-char port)))

(define/method (Port.Input.read_line port
                                     #:mode [mode-in 'any])
  #:primitive (read-line)
  (unless (input-port? port) (raise-annotation-failure who port "Port.Input"))
  (define mode (->ReadLineMode mode-in))
  (unless mode
    (check-input-port who port)
    (raise-annotation-failure who mode-in "Port.Input.ReadLineMode"))
  (coerce-read-result
   (read-line port mode)))

(define/method (Port.Input.read_bytes_line port
                                           #:mode [mode-in 'any])
  #:primitive (read-line)
  (unless (input-port? port) (raise-annotation-failure who port "Port.Input"))
  (define mode (->ReadLineMode mode-in))
  (unless mode
    (check-input-port who port)
    (raise-annotation-failure who mode-in "Port.Input.ReadLineMode"))
  (read-bytes-line port mode))

(define/method (Port.Input.read_string port amt)
  #:primitive (read-string)
  (coerce-read-result (read-string amt port)))

(define/method (Port.Input.read_string_to port str
                                          #:start [start 0]
                                          #:end [end (and (string? str) (string-length str))])
  #:primitive (read-string!)
  (read-string! str port start end))

(define/method (Port.Input.copy_to p . outs)
  #:primitive (copy-port)
  (apply copy-port p outs))

(define/method (Port.Input.Progress.evt port)
  (unless (input-progress-port? port)
    (raise-annotation-failure who port "Input.Port.Progress"))
  (wrap-progress-evt (port-progress-evt port)))

(define/method (Port.Input.Progress.commit port amt progress evt)
  #:primitive (port-commit-peeked)
  (unless (input-progress-port? port)
    (raise-annotation-failure who port "Input.Port.Progress"))
  (port-commit-peeked amt (extract-progress-evt progress) (extract-commit-evt evt) port))

(define/method (Port.Input.Progress.is_evt port evt)
  (unless (input-progress-port? port)
    (raise-annotation-failure who port "Input.Port.Progress"))
  (progress-evt? (extract-progress-evt who evt) port))

(define/method (Port.Output.close port)
  #:primitive (close-output-port)
  (close-output-port port))

(define/method (Port.Output.String.get_bytes port)
  #:primitive (get-output-bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (get-output-bytes port))

(define/method (Port.Output.String.get_string port)
  #:primitive (get-output-string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (string->immutable-string (get-output-string port)))

(define/method (Port.Output.flush [p (current-output-port)])
  (check-output-port who p)
  (flush-output p))

(define/method (Port.Output.write_byte port b)
  #:primitive (write-byte)
  (write-byte b port))

(define/method (Port.Output.write_char port ch)
  #:primitive (write-char)
  (write-byte ch port))

(define/method (Port.Output.Special.write port v
                                          #:wait [wait 'all])
  #:primitive (write-special write-special-avail*)
  (cond
    [(eq? wait 'all)
     (write-special v port)]
    [(eq? wait 'none)
     (write-special-avail* v port)]
    [else
     (raise-annotation-failure who wait "Any.of(#'all, #'none)")]))

(define/method (Port.Output.write_bytes port bstr
                                        #:wait [wait 'all]
                                        #:start [start 0]
                                        #:end [end (and (bytes? bstr) (bytes-length bstr))])
  #:primitive (write-bytes write-bytes-avail write-bytes-avail* write-bytes-avail/enable-break)
  (cond
    [(eq? wait 'all)
     (write-bytes bstr port start end)]
    [(eq? wait 'some)
     (write-bytes-avail bstr port start end)]
    [(eq? wait 'none)
     (write-bytes-avail* bstr port start end)]
    [(eq? wait 'enable_break)
     (write-bytes-avail/enable-break bstr port start end)]
    [else (check-wait who wait)]))

(define/method (Port.Output.write_string port str
                                         #:start [start 0]
                                         #:end [end (and (string? str) (string-length str))])
  #:primitive (write-string)
  (write-string str port start end)
  (void))

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

(define-syntax stdin
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (wrap-static-info* #'(current-input-port)
                                   (get-input-port-static-infos))
                #'tail)]))))

(define-syntax stdout
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (wrap-static-info* #'(current-output-port)
                                   (get-output-port-static-infos))
                #'tail)]))))

(define-syntax stderr
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (wrap-static-info* #'(current-error-port)
                                   (get-output-port-static-infos))
                #'tail)]))))

(void (set-primitive-contract! '(or/c 'binary 'text) "Port.Mode"))
