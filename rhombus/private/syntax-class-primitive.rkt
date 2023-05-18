#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     "introducer.rkt")
         syntax/parse/pre
         "provide.rkt"
         "operator-parse.rkt"
         "name-root.rkt"
         "definition.rkt"
         "pack.rkt")

(provide (for-space rhombus/stxclass
                    Term
                    Identifier
                    Operator
                    Name
                    Group
                    Block
                    Multi
                    Keyword
                    String
                    Int))

(module+ for-quasiquote
  (begin-for-syntax
    (provide in-syntax-class-space
             (struct-out rhombus-syntax-class)
             syntax-class-ref
             (property-out syntax-class-parser)
             syntax-class-parser-ref
             syntax-class-parser-proc
             (struct-out pattern-variable)
             pattern-variable->list
             list->pattern-variable
             syntax-list->pattern-variable)))

(module+ for-syntax-class-syntax
  (provide (for-syntax rhombus-syntax-class
                       in-syntax-class-space)
           define-syntax-class-syntax))

(module+ for-syntax-class
  (provide (for-syntax make-syntax-class)
           define-operator-syntax-classes
           define-transformer-syntax-class
           define-transformer-parameterized-syntax-class))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/stxclass))

  (struct rhombus-syntax-class (kind class attributes splicing? arity root-swap))
  (define (syntax-class-ref v) (and (rhombus-syntax-class? v) v))

  ;; used to communicate `syntax.parse` as a anonymous-class form
  (property syntax-class-parser (proc))

  ;; used to represent parsed pattern variables and attributes in syntax classes:
  (struct pattern-variable (sym       ; external name
                            id        ; identifier name of external form, useful for errors
                            val-id    ; identifier that holds match
                            depth     ; repetition depth relative to base name
                            unpack*-id)) ; unpacker

  (define (pattern-variable->list pv #:keep-id? [keep-id? #t])
    (list (pattern-variable-sym pv)
          (and keep-id? (pattern-variable-id pv))
          (pattern-variable-val-id pv)
          (pattern-variable-depth pv)
          (pattern-variable-unpack*-id pv)))
  (define (list->pattern-variable l)
    (apply pattern-variable l))
  (define (syntax-list->pattern-variable pv)
    (define l (syntax->list pv))
    (pattern-variable (syntax-e (car l))
                      (let ([id (cadr l)]) (and (syntax-e id) id))
                      (caddr l)
                      (syntax-e (list-ref l 3))
                      (list-ref l 4)))

  (define (make-syntax-class pat
                             #:kind [kind 'term]
                             #:arity [arity #f]
                             #:fields [fields #'()]
                             #:splicing? [splicing? #f]
                             #:root-swap [root-swap #f])
    (rhombus-syntax-class kind pat fields splicing? arity root-swap)))

(define-syntax (define-syntax-class-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-class-space #'name) rhs))]))

(define-syntax-class-syntax Term (make-syntax-class #f))
(define-syntax-class-syntax Identifier (make-syntax-class #'identifier))
(define-syntax-class-syntax Operator (make-syntax-class #':operator))
(define-syntax-class-syntax Name (make-syntax-class #':operator-or-identifier))
(define-syntax-class-syntax Keyword (make-syntax-class #'keyword))
(define-syntax-class-syntax String (make-syntax-class #'string))
(define-syntax-class-syntax Int (make-syntax-class #'exact-integer))
(define-syntax-class-syntax Group (make-syntax-class #f #:kind 'group))
(define-syntax-class-syntax Multi (make-syntax-class #f #:kind 'multi))
(define-syntax-class-syntax Block (make-syntax-class #f #:kind 'block))

(define-syntax-rule (define-operator-syntax-classes
                      Parsed :form
                      AfterPrefixParsed :prefix-op+form+tail
                      AfterInfixParsed :infix-op+form+tail)
  (begin
    (define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                          #:kind 'group
                                                          #:fields #'((parsed #f parsed 0 unpack-parsed*))
                                                          #:root-swap '(parsed . group)))
    (define-syntax-class-syntax AfterPrefixParsed (make-syntax-class #':prefix-op+form+tail
                                                                     #:kind 'group
                                                                     #:arity 2
                                                                     #:fields #'((parsed #f parsed 0 unpack-parsed*)
                                                                                 (tail #f tail tail unpack-tail-list*))
                                                                     #:root-swap '(parsed . group)))
    (define-syntax-class-syntax AfterInfixParsed (make-syntax-class #':infix-op+form+tail
                                                                    #:kind 'group
                                                                    #:arity 2
                                                                    #:fields #'((parsed #f parsed 0 unpack-parsed*)
                                                                                (tail #f tail tail unpack-tail-list*))
                                                                    #:root-swap '(parsed . group)))))

(define-syntax-rule (define-transformer-syntax-class
                      Parsed :form)
  (define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                        #:kind 'group
                                                        #:fields #'((parsed #f parsed 0 unpack-parsed*))
                                                        #:root-swap '(parsed . group))))

(define-syntax-rule (define-transformer-parameterized-syntax-class
                      Parsed :form)
  (define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                        #:kind 'group
                                                        #:arity 2
                                                        #:fields #'((parsed #f parsed 0 unpack-parsed*))
                                                        #:root-swap '(parsed . group))))
