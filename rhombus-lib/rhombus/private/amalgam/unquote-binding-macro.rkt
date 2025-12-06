#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     "name-root.rkt"
                     "pack.rkt"
                     "macro-result.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "class-meta.rkt" for-static-info)
                     (submod "symbol.rkt" for-static-info)
                     "define-arity.rkt"
                     "annotation-failure.rkt"
                     "realm.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "unquote-binding.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "sequence-pattern.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                unquote_bind_meta)))

(define+provide-space unquote_bind rhombus/unquote_bind
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root unquote_bind_meta
    #:fields
    (space
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     [unpack_kind unquote_bind_meta.unpack_kind]
     [pack_invalid unquote_bind_meta.pack_invalid])))

(define-for-syntax space
  (space-syntax rhombus/unquote_bind))

(define-operator-definition-transformer macro
  'macro
  rhombus/unquote_bind
  #:extra ([#:kind get-symbol-static-infos value])
  #'make-unquote-binding-prefix-operator
  #'make-unquote-binding-infix-operator
  #'unquote-binding-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :unquote-binding #:rhombus/unquote_bind
    NameStart in-unquote-binding-space
    AfterPrefixParsed :unquote-binding-prefix-op+form+tail
    AfterInfixParsed :unquote-binding-infix-op+form+tail
    #:extra-arity-mask 2))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/unquote_bind #,stx))

(define-for-syntax (make-unquote-binding-prefix-operator order prec protocol proc)
  (unquote-binding-prefix-operator
   order
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 stx ctx-kind)
         (cond
           [(syntax-e form1)
            (finish-auto (lambda () (proc (wrap-parsed form1) stx ctx-kind))
                         proc
                         ctx-kind)]
           [else #'#f]))
       (lambda (stx ctx-kind)
         (finish (lambda ()
                   (syntax-parse stx
                     [(head . tail) (proc (pack-tail #'tail) #'head ctx-kind)]))
                 proc
                 ctx-kind)))))

(define-for-syntax (make-unquote-binding-infix-operator order prec protocol proc assc)
  (unquote-binding-prefix-operator
   order
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx ctx-kind)
         (cond
           [(and (syntax-e form1) (syntax-e form2))
            (finish-auto (lambda () (proc (wrap-parsed form1) (wrap-parsed form2) stx ctx-kind))
                         proc
                         ctx-kind)]
           [else #'#f]))
       (lambda (form1 stx ctx-kind)
         (cond
           [(syntax-e form1)
            (finish (lambda ()
                      (syntax-parse stx
                        [(head . tail) (proc form1 (pack-tail #'tail) #'head ctx-kind)]))
                    proc
                    ctx-kind)]
           [else (values #'f #'())])))
   assc))

(define-for-syntax (finish-auto thunk proc ctx-kind)
  (finish-binds (thunk) proc ctx-kind))

(define-for-syntax (finish thunk proc ctx-kind)
  (define-values (binds tail)
    (call-with-values
     thunk
     (case-lambda
       [(binds tail) (values binds (unpack-tail tail proc #f))]
       [(binds) (values binds #'())])))
  (values (finish-binds binds proc ctx-kind)
          tail))

(define-for-syntax (finish-binds binds proc ctx-kind)
  (unless (syntax? binds)
    (raise-bad-macro-result (proc-name proc) "unquote binding" binds))
  (define result-g (unpack-group binds proc binds))
  (syntax-parse result-g
    #:datum-literals (group parsed)
    [(group (parsed #:rhombus/unquote_bind #f))
     ;; failure is always compatible
     #'#f]
    [(group (parsed #:rhombus/unquote_bind (kind pat idrs sidrs vars)))
     (unless (eq? (syntax-e #'kind) ctx-kind)
       (raise-bad-macro-result (proc-name proc)
                               (format "unquote binding `~a` context" ctx-kind)
                               result-g))
     #'(kind pat idrs sidrs vars)]
    [(~var esc (:unquote-binding ctx-kind)) #'esc.parsed]))

(begin-for-syntax
  (define/arity (unquote_bind_meta.unpack_kind stx)
    (define e (unpack-term stx #f #f))
    (unless e (raise-annotation-failure who "Term" stx))
    (syntax-parse e
      #:datum-literals (parsed)
      [(parsed #:rhombus/unquote_bind #f) #f]
      [(parsed #:rhombus/unquote_bind (kind . _)) (syntax-e #'kind)]
      [(parsed #:rhombus/unquote_bind id:identifier) 'id]
      [_  (raise-arguments-error* who rhombus-realm
                                  "not a parsed unquote binding annotation"
                                  "syntax object" stx)]))

  (define/arity (unquote_bind_meta.pack_invalid)
    #`(parsed #:rhombus/unquote_bind #f)))
