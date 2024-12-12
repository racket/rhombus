#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/name-parse
                     "pack.rkt"
                     "macro-result.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
                     "macro-result.rkt"
                     "annotation-failure.rkt"
                     "tail-returner.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     (submod "symbol.rkt" for-static-info)
                     (submod "list.rkt" for-listable)
                     "static-info-pack.rkt"
                     "syntax-wrap.rkt"
                     (for-syntax racket/base))
         "space-provide.rkt"
         "immediate-callee.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space immediate_callee rhombus/immediate_callee
  #:fields
  (macro))

(provide (for-syntax (for-space rhombus/namespace
                                immediate_callee_meta)))

(begin-for-syntax
  (define-name-root immediate_callee_meta
    #:fields
    (Parsed)))

(define-identifier-syntax-definition-transformer macro
  rhombus/immediate_callee
  #:extra ([#:static_infos get-treelist-of-syntax-static-infos value]
           [#:in_op_mode get-symbol-static-infos value]
           [#:in_op_stx get-syntax-static-infos value])
  #'make-immediate-callee-transformer)

(begin-for-syntax
  (define-syntax-class (:immediate-callee/split static-infoss op-mode op-stx)
    #:attributes (parsed tail)
    (pattern (~var callee (:immediate-callee (to-list 'immediate_callee_meta.Parsed static-infoss)
                                             (case op-mode
                                               [(infix prefix) op-mode]
                                               [else (raise-annotation-failure 'immediate_callee_meta.Parsed
                                                                               op-mode
                                                                               "matching(#'prefix || #'infix)")])
                                             (or (and (syntax? op-stx)
                                                      (syntax-parse op-stx
                                                        [op::name #'op.name]
                                                        [_ #f]))
                                                 (raise-annotation-failure 'immediate_callee_meta.Parsed
                                                                           op-stx
                                                                           "Name"))))
             #:with (parsed . tail) #'callee.parsed))

  (define-syntax-class-syntax Parsed
    (make-syntax-class #':immediate-callee/split
                       #:kind 'group
                       #:arity 8 ; actually an arity mask
                       #:fields #'((parsed #f parsed 0 (unpack-parsed* '#:rhombus/expr) stx)
                                   (tail #f tail tail unpack-tail-list* stx))
                       #:root-swap '(parsed . group))))

(define-for-syntax (extract-immediate-callee form tail proc static-infoss op-mode op-stx)
  (define stx (if (syntax*? form)
                  (unpack-group form proc #f)
                  (raise-bad-macro-result (proc-name proc) "expression" form)))
  (cond
    [tail
     (syntax-parse stx
       [(~var ic (:immediate-callee static-infoss op-mode op-stx))
        (define-values (parsed new-tail) (unpack-immediate-callee #'ic.parsed))
        (unless (null? (syntax-e new-tail))
          (raise-bad-macro-result (proc-name proc)
                                  "immediate callee form that parses with an empty tail"
                                  form #:syntax-for? #f))
        (pack-immediate-callee parsed  (unpack-tail tail #f #f))]
       [e::expression
        (pack-immediate-callee #'e.parsed (unpack-tail tail #f #f))])]
    [else
     (syntax-parse stx
       [(~var ic (:immediate-callee static-infoss op-mode op-stx))
        #'ic.parsed]
       [_
        (cond
          [(eq? op-mode 'prefix)
           (syntax-parse stx
             [(~var e (:prefix-op+expression+tail op-stx))
              (pack-immediate-callee #'e.parsed #'e.tail)])]
          [else
           (syntax-parse stx
             [(~var e (:infix-op+expression+tail op-stx))
              (pack-immediate-callee #'e.parsed #'e.tail)])])])]))

(define-for-syntax (make-immediate-callee-transformer proc)
  (immediate-callee-transformer
   (lambda (stx static-infoss op-mode op-stx)
     (define-values (form new-tail)
       (tail-returner
        #:empty-tail #f
        proc
        (syntax-parse stx
          [(head . tail) (proc (pack-tail #'tail)
                               #'head
                               (to-treelist #f
                                            (map (lambda (si) (unpack-static-infos #f si))
                                                 static-infoss))
                               op-mode
                               op-stx)])))
     (extract-immediate-callee form new-tail proc static-infoss op-mode op-stx))))
