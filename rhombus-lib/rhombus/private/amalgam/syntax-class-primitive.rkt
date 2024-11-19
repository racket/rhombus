#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     "introducer.rkt"
                     "name-start-syntax-class.rkt")
         syntax/parse/pre
         "provide.rkt"
         "operator-parse.rkt"
         "dotted-sequence.rkt"
         "name-root.rkt"
         "definition.rkt"
         "pack.rkt")

(provide (for-space rhombus/stxclass
                    Term
                    Identifier
                    Operator
                    Name
                    IdentifierName
                    Sequence
                    Group
                    Block
                    Multi
                    Keyword
                    String
                    Int
                    Number
                    Boolean))

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
           define-transformer-syntax-class))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/stxclass))

  (struct rhombus-syntax-class (kind class attributes splicing? arity root-swap auto-args))
  (define (syntax-class-ref v) (and (rhombus-syntax-class? v) v))

  ;; used to communicate `syntax.parse` as a anonymous-class form
  (property syntax-class-parser (proc))

  ;; used to represent parsed pattern variables and attributes in syntax classes:
  (struct pattern-variable (sym       ; external name
                            id        ; identifier name of external form, useful for errors
                            val-id    ; identifier that holds match
                            depth     ; repetition depth relative to base name
                            unpack*)) ; unpacker, usually an identifier

  (define (pattern-variable->list pv #:keep-id? [keep-id? #t])
    (list (pattern-variable-sym pv)
          (and keep-id? (pattern-variable-id pv))
          (pattern-variable-val-id pv)
          (pattern-variable-depth pv)
          (pattern-variable-unpack* pv)))
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
                             #:root-swap [root-swap #f]
                             #:auto-args [auto-args #f])
    (rhombus-syntax-class kind pat fields splicing? arity root-swap auto-args)))

(define-splicing-syntax-class :sequence
  (pattern (~seq _ ...)))

(define-syntax (define-syntax-class-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-class-space #'name) rhs))]))

(define-syntax-class-syntax Term (make-syntax-class #f))
(define-syntax-class-syntax Identifier (make-syntax-class #'identifier))
(define-syntax-class-syntax Operator (make-syntax-class #':operator))
(define-syntax-class-syntax Name (make-syntax-class #':dotted-operator-or-identifier-sequence
                                                    #:splicing? #t
                                                    #:kind 'term))
(define-syntax-class-syntax IdentifierName (make-syntax-class #':dotted-identifier-sequence
                                                              #:splicing? #t
                                                              #:kind 'term))
(define-syntax-class-syntax Keyword (make-syntax-class #'keyword))
(define-syntax-class-syntax String (make-syntax-class #'string))
(define-syntax-class-syntax Int (make-syntax-class #'exact-integer))
(define-syntax-class-syntax Number (make-syntax-class #'number))
(define-syntax-class-syntax Boolean (make-syntax-class #'boolean))
(define-syntax-class-syntax Sequence (make-syntax-class #':sequence #:splicing? #t #:kind 'term))
(define-syntax-class-syntax Group (make-syntax-class #f #:kind 'group))
(define-syntax-class-syntax Multi (make-syntax-class #f #:kind 'multi))
(define-syntax-class-syntax Block (make-syntax-class #f #:kind 'block))

(begin-for-syntax
  (define name-start-fields
    #'((name name #f 0 unpack-term*)
       (head #f head tail unpack-tail-list*)
       (tail #f tail tail unpack-tail-list*))))

(define-syntax (define-operator-syntax-classes stx)
  (syntax-parse stx
    [(_ Parsed (~var :form) parsed-tag
        NameStart:id in-space
        (~optional (~seq AfterPrefixParsed:id (~var :prefix-op+form+tail)
                         AfterInfixParsed:id (~var :infix-op+form+tail)))
        (~optional (~seq #:extra-arity a)
                   #:defaults ([a #'#f])))
     #'(begin
         (define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                               #:kind 'group
                                                               #:arity a
                                                               #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag)))
                                                               #:root-swap '(parsed . group)))
         (~? (define-syntax-class-syntax AfterPrefixParsed (make-syntax-class #':prefix-op+form+tail
                                                                              #:kind 'group
                                                                              #:arity (if a (+ a 2) 2)
                                                                              #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag))
                                                                                          (tail #f tail tail unpack-tail-list*))
                                                                              #:root-swap '(parsed . group))))
         (~? (define-syntax-class-syntax AfterInfixParsed (make-syntax-class #':infix-op+form+tail
                                                                             #:kind 'group
                                                                             #:arity (if a (+ a 2) 2)
                                                                             #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag))
                                                                                         (tail #f tail tail unpack-tail-list*))
                                                                             #:root-swap '(parsed . group))))
         (define-syntax-class-syntax NameStart (make-syntax-class #':name-start
                                                                  #:auto-args #'(in-space)
                                                                  #:kind 'group
                                                                  #:fields name-start-fields)))]))

(define-syntax (define-transformer-syntax-class stx)
  (syntax-parse stx
    [(_ Parsed (~var :form) parsed-tag
        (~optional (~seq #:arity a)
                   #:defaults ([a #'#f])))
     #'(define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                             #:kind 'group
                                                             #:arity a
                                                             #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag)))
                                                             #:root-swap '(parsed . group)))]))
