#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "class-primitive.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         "define-arity.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "pair.rkt" for-static-info)
         (submod "syntax-object.rkt" for-quasiquote)
         (submod "srcloc-object.rkt" for-static-info)
         "annotation-failure.rkt"
         "name-root.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot
                      rhombus/class)
                     Exn))

(module+ for-builtin
  (provide get-exn-method-table))

(define-primitive-class Exn exn
  #:existing
  #:class
  #:transparent
  #:fields
  ([(message)]
   [(marks continuation-marks)])
  #:namespace-fields
  (Fail
   Break)
  #:properties
  ()
  #:methods
  ())

(define-syntax-rule (define-exn Name name
                      #:parent Parent parent
                      #:fields (field ...)
                      #:children (child ...))
  (define-primitive-class Name name
    #:existing
    #:class
    #:transparent
    #:parent Parent parent
    #:fields
    (field ...)
    #:namespace-fields
    (child ...)
    #:properties
    ()
    #:methods
    ()))

(define-exn Fail exn:fail
  #:parent Exn exn
  #:fields ()
  #:children (Contract
              Syntax
              Read
              Filesystem
              Network
              OutOfMemory
              Unsupported
              User
              Annot))

(define-exn Contract exn:fail:contract
  #:parent Fail exn:fail
  #:fields ()
  #:children (Arity
              DivideByZero
              NonFixnumResult
              Continuation
              Variable))

(struct exn:fail:annot exn:fail:contract (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (self)
                                (exn:fail:annot-srclocs self))
  #:guard (lambda (msg marks srclocs st)
            (define who 'Exn.Fail.Annot)
            (unless (string? msg) (raise-annotation-failure who msg "ReadableString"))
            (unless (continuation-mark-set? marks) (raise-annotation-failure who marks "Continuation.Marks"))
            (unless (and (list? srclocs) (andmap srcloc? srclocs))
              (raise-annotation-failure who srclocs "PairList.of(Srcloc)"))
            (values msg marks srclocs)))

(define-exn Annot exn:fail:annot
  #:parent Exn.Fail.Annot exn:fail:contract
  #:fields ([(srclocs) ((#%index-result #,(get-srcloc-static-infos))
                        . #,(get-list-static-infos))])
  #:children ())

(define-exn Arity exn:fail:contract:arity
  #:parent Contract exn:fail:contract
  #:fields ()
  #:children ())

(define-exn DivideByZero exn:fail:contract:divide-by-zero
  #:parent Contract exn:fail:contract
  #:fields ()
  #:children ())

(define-exn NonFixnumResult exn:fail:contract:non-fixnum-result
  #:parent Contract exn:fail:contract
  #:fields ()
  #:children ())

(define-exn Continuation exn:fail:contract:continuation
  #:parent Contract exn:fail:contract
  #:fields ()
  #:children ())

(define-exn Variable exn:fail:contract:variable
  #:parent Contract exn:fail:contract
  #:fields ([(id)])
  #:children ())

(define-exn Syntax exn:fail:syntax
  #:parent Fail exn:fail
  #:fields ([(exprs) ((#%index-result #,(get-syntax-static-infos))
                      . #,(get-list-static-infos))])
  #:children (Unbound
              MissingModule))

(define-exn Unbound exn:fail:syntax:unbound
  #:parent Syntax exn:fail:syntax
  #:fields ()
  #:children ())

(define-exn MissingModule exn:fail:syntax:missing-module
  #:parent Syntax exn:fail:syntax
  #:fields ([(path)])
  #:children ())

(define-exn Read exn:fail:read
  #:parent Fail exn:fail
  #:fields ([(srclocs) ((#%index-result #,(get-srcloc-static-infos))
                        . #,(get-list-static-infos))])
  #:children (EOF
              NonChar))

(define-exn EOF exn:fail:read:eof
  #:parent Read exn:fail:read
  #:fields ()
  #:children ())

(define-exn NonChar exn:fail:read:non-char
  #:parent Read exn:fail:read
  #:fields ()
  #:children ())

(define-exn Filesystem exn:fail:filesystem
  #:parent Fail exn:fail
  #:fields ()
  #:children (Exists
              Version
              Errno
              [MissingModule fs_MissingModule]))

(define-exn Exists exn:fail:filesystem:exists
  #:parent Filesystem exn:fail:filesystem
  #:fields ()
  #:children ())

(define-exn Version exn:fail:filesystem:version
  #:parent Filesystem exn:fail:filesystem
  #:fields ()
  #:children ())

(define-exn Errno exn:fail:filesystem:errno
  #:parent Filesystem exn:fail:filesystem
  #:fields ([(errno) #,(get-pair-static-infos)])
  #:children ())

(define-exn fs_MissingModule exn:fail:filesystem:missing-module
  #:parent Filesystem exn:fail:filesystem
  #:fields ([(path)])
  #:children ())

(define-exn Network exn:fail:network
  #:parent Fail exn:fail
  #:fields ()
  #:children ([Errno net_Errno]))

(define-exn new_Errno exn:fail:network:errno
  #:parent Network exn:fail:network
  #:fields ([(errno)])
  #:children ())

(define-exn OutOfMemory exn:fail:out-of-memory
  #:parent Fail exn:fail
  #:fields ()
  #:children ())

(define-exn Unsnpported exn:fail:unsupported
  #:parent Fail exn:fail
  #:fields ()
  #:children ())

(define-exn User exn:fail:user
  #:parent Fail exn:fail
  #:fields ()
  #:children ())

(define-exn Break exn:break
  #:parent Exn exn
  #:fields ([(continuation)])
  #:children (HangUp
              Terminate))

(define-exn HangUp exn:break:hang-up
  #:parent Break exn:break
  #:fields ()
  #:children ())

(define-exn Terminate exn:break:terminate
  #:parent Break exn:break
  #:fields ()
  #:children ())

(define (get-exn-method-table v)
  (cond
    [(exn:fail? v)
     (cond
       [(exn:fail:contract? v)
        (cond
          [(exn:fail:contract:arity? v) exn:fail:contract:arity-method-table]
          [(exn:fail:contract:divide-by-zero? v) exn:fail:contract:divide-by-zero-method-table]
          [(exn:fail:contract:non-fixnum-result? v) exn:fail:contract:non-fixnum-result-method-table]
          [(exn:fail:contract:continuation? v) exn:fail:contract:continuation-method-table]
          [(exn:fail:contract:variable? v) exn:fail:contract:variable-method-table]
          [else exn:fail:contract-method-table])]
       [(exn:fail:syntax? v)
        (cond
          [(exn:fail:syntax:unbound? v) exn:fail:syntax:unbound-method-table]
          [(exn:fail:syntax:missing-module? v) exn:fail:syntax:missing-module-method-table]
          [else exn:fail:syntax-method-table])]
       [(exn:fail:read? v)
        (cond
          [(exn:fail:read:eof? v) exn:fail:read:eof-method-table]
          [(exn:fail:read:non-char? v) exn:fail:read:non-char-method-table]
          [else exn:fail:read-method-table])]
       [(exn:fail:filesystem? v)
        (cond
          [(exn:fail:filesystem:exists? v) exn:fail:filesystem:exists-method-table]
          [(exn:fail:filesystem:version? v) exn:fail:filesystem:version-method-table]
          [(exn:fail:filesystem:errno? v) exn:fail:filesystem:errno-method-table]
          [(exn:fail:filesystem:missing-module? v) exn:fail:filesystem:missing-module-method-table]
          [else exn:fail:filesystem-method-table])]
       [(exn:fail:network? v)
        (cond
          [(exn:fail:network:errno? v) exn:fail:network:errno-method-table]
          [else exn:fail:network-method-table])]
       [(exn:fail:out-of-memory? v) exn:fail:out-of-memory-method-table]
       [(exn:fail:unsupported? v) exn:fail:unsupported-method-table]
       [(exn:fail:user? v) exn:fail:user-method-table]
       [else exn:fail-method-table])]
    [(exn:break? v)
     (cond
       [(exn:break:hang-up? v) exn:break:hang-up-method-table]
       [(exn:break:terminate? v) exn:break:terminate-method-table]
       [else exn:break-method-table])]
    [else exn-method-table]))
