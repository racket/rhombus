#lang racket/base
(require (for-syntax racket/base
                     (only-in racket/fixnum
                              fixnum-for-every-system?)
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         racket/hash-code
         (only-in racket/unsafe/ops
                  unsafe-fx+
                  unsafe-fx<
                  unsafe-fx<=
                  unsafe-fx>
                  unsafe-fx>=)
         (only-in racket/private/for
                  prop:stream)
         racket/treelist
         "range-struct.rkt"
         (submod "range-struct.rkt" descending)
         "expression.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-range)
         "parse.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "treelist-statinfo.rkt"
         "realm.rkt"
         "annotation-failure.rkt"
         "number.rkt"
         "provide.rkt"
         "printer-property.rkt"
         "print-desc.rkt"
         "class-primitive.rkt"
         "binding.rkt"
         (submod "dot.rkt" for-dot-provider)
         "define-arity.rkt"
         "call-result-key.rkt"
         "contains-key.rkt"
         "sequence-constructor-key.rkt"
         "index-result-key.rkt"
         "order.rkt"
         "order-primitive.rkt"
         "to-list.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Range
                     SequenceRange
                     ListRange
                     DescendingRange
                     DescendingListRange)
         (for-spaces (#f
                      rhombus/repet
                      rhombus/bind)
                     ..
                     ..=
                     <..
                     <..=)
         (for-spaces (#f
                      rhombus/repet)
                     >=..
                     >=..=
                     >..
                     >..=))

(module+ for-container
  (provide range?
           range-contains?))

(module+ for-substring
  (provide range-canonical-start+end))

(define-static-info-getter get-any-range-static-infos
  (#%contains Range.contains))

(define-primitive-class Range range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-range-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:fields ()
  #:namespace-fields
  ([from_to Range.from_to]
   [from_to_inclusive Range.from_to_inclusive]
   [from Range.from]
   [from_exclusive_to Range.from_exclusive_to]
   [from_exclusive_to_inclusive Range.from_exclusive_to_inclusive]
   [from_exclusive Range.from_exclusive]
   [to Range.to]
   [to_inclusive Range.to_inclusive]
   [full Range.full])
  #:properties
  ()
  #:methods
  (start
   end
   includes_start
   includes_end
   is_empty
   canonicalize
   contains
   encloses
   is_connected
   overlaps
   span
   gap
   intersect))

(define-static-info-getter get-any-sequence-range-static-infos
  (#%sequence-constructor SequenceRange.to_sequence/optimize)
  (#%sequence-element #,(get-int-static-infos))
  #,@(get-any-range-static-infos))

(define-primitive-class SequenceRange sequence-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-sequence-range-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f range
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ([first SequenceRange.first  (lambda (s) (get-int-static-infos))]
   [rest SequenceRange.rest (lambda (s) (get-sequence-range-static-infos))])
  #:methods
  (to_sequence
   step_by))

(define-primitive-class ListRange list-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-sequence-range-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f sequence-range
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ()
  #:methods
  (descending
   to_list))

(define-static-info-getter get-any-descending-range-static-infos
  (#%sequence-constructor DescendingRange.to_sequence/optimize)
  (#%sequence-element #,(get-int-static-infos)))

(define-primitive-class DescendingRange descending-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-descending-range-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:fields ()
  #:namespace-fields
  ([from_to DescendingRange.from_to]
   [from_to_inclusive DescendingRange.from_to_inclusive]
   [from DescendingRange.from]
   [from_exclusive_to DescendingRange.from_exclusive_to]
   [from_exclusive_to_inclusive DescendingRange.from_exclusive_to_inclusive]
   [from_exclusive DescendingRange.from_exclusive])
  #:properties
  ([first DescendingRange.first  (lambda (s) (get-int-static-infos))]
   [rest DescendingRange.rest (lambda (s) (get-descending-range-static-infos))])
  #:methods
  (is_empty
   to_sequence
   step_by))

(define-primitive-class DescendingListRange descending-list-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-descending-range-static-infos)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f descending-range
  #:fields ()
  #:namespace-fields
  ()
  #:properties
  ()
  #:methods
  (to_list))

(define-values-for-syntax (..-expr-prefix ..-repet-prefix)
  (make-expression&repetition-prefix-operator
   (lambda () (order-quote enumeration))
   '()
   'mixfix
   (case-lambda
     [(self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list self-stx)))
        #`(range-full/who '..))
       (get-range-static-infos))]
     [(right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list self-stx right)))
        #`(range-to/who '..
                        #,(discard-static-infos right)))
       (get-range-static-infos))])))

(define-values-for-syntax (..-expr-infix ..-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'mixfix
   (case-lambda
     [(left self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx)))
        #`(range-from/who '..
                          #,(discard-static-infos left)))
       (get-sequence-range-static-infos))]
     [(left right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx right)))
        #`(range-from-to/who '..
                             #,(discard-static-infos left)
                             #,(discard-static-infos right)))
       (get-list-range-static-infos))])
   'none))

(define-syntax ..
  (expression-prefix+infix-operator
   ..-expr-prefix
   ..-expr-infix))

(define-repetition-syntax ..
  (repetition-prefix+infix-operator
   ..-repet-prefix
   ..-repet-infix))

(define-values-for-syntax (..=-expr-prefix ..=-repet-prefix)
  (make-expression&repetition-prefix-operator
   (lambda () (order-quote enumeration))
   '()
   'prefix
   (lambda (right self-stx)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list self-stx right)))
       #`(range-to-inclusive/who '..=
                                 #,(discard-static-infos right)))
      (get-range-static-infos)))))

(define-values-for-syntax (..=-expr-infix ..=-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'infix
   (lambda (left right self-stx)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list left self-stx right)))
       #`(range-from-to-inclusive/who '..=
                                      #,(discard-static-infos left)
                                      #,(discard-static-infos right)))
      (get-list-range-static-infos)))
   'none))

(define-syntax ..=
  (expression-prefix+infix-operator
   ..=-expr-prefix
   ..=-expr-infix))

(define-repetition-syntax ..=
  (repetition-prefix+infix-operator
   ..=-repet-prefix
   ..=-repet-infix))

(define-values-for-syntax (<..-expr-infix <..-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'mixfix
   (case-lambda
     [(left self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx)))
        #`(range-from-exclusive/who '<..
                                    #,(discard-static-infos left)))
       (get-sequence-range-static-infos))]
     [(left right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx right)))
        #`(range-from-exclusive-to/who '<..
                                       #,(discard-static-infos left)
                                       #,(discard-static-infos right)))
       (get-list-range-static-infos))])
   'none))

(define-syntax <..
  <..-expr-infix)

(define-repetition-syntax <..
  <..-repet-infix)

(define-values-for-syntax (<..=-expr-infix <..=-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'infix
   (lambda (left right self-stx)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list left self-stx right)))
       #`(range-from-exclusive-to-inclusive/who '<..=
                                                #,(discard-static-infos left)
                                                #,(discard-static-infos right)))
      (get-list-range-static-infos)))
   'none))

(define-syntax <..=
  <..=-expr-infix)

(define-repetition-syntax <..=
  <..=-repet-infix)

(define-binding-syntax ..
  (binding-prefix+infix-operator
   (binding-prefix-operator
    (lambda () (order-quote enumeration))
    `()
    'macro
    (lambda (tail)
      (syntax-parse tail
        [(_)
         (values (parse-range-binding #'Range.full)
                 #'())]
        [(_ . more)
         #:with (~var rhs (:prefix-op+binding+tail #'..)) #`(#,group-tag . more)
         (values (parse-range-binding #'Range.to #'rhs.parsed)
                 #'rhs.tail)])))
   (binding-infix-operator
    (lambda () (order-quote enumeration))
    `()
    'macro
    (lambda (form1 tail)
      (syntax-parse tail
        [(_)
         (values (parse-range-binding #'Range.from form1)
                 #'())]
        [(_ . more)
         #:with (~var rhs (:infix-op+binding+tail #'..)) #`(#,group-tag . more)
         (values (parse-range-binding #'Range.from_to form1 #'rhs.parsed)
                 #'rhs.tail)]))
    'none)))

(define-binding-syntax ..=
  (binding-prefix+infix-operator
   (binding-prefix-operator
    (lambda () (order-quote enumeration))
    `()
    'automatic
    (lambda (form stx)
      (parse-range-binding #'Range.to_inclusive form)))
   (binding-infix-operator
    (lambda () (order-quote enumeration))
    `()
    'automatic
    (lambda (form1 form2 stx)
      (parse-range-binding #'Range.from_to_inclusive form1 form2))
    'none)))

(define-binding-syntax <..
  (binding-infix-operator
   (lambda () (order-quote enumeration))
   `()
   'macro
   (lambda (form1 tail)
     (syntax-parse tail
       [(_)
        (values (parse-range-binding #'Range.from_exclusive form1)
                #'())]
       [(_ . more)
        #:with (~var rhs (:infix-op+binding+tail #'<..)) #`(#,group-tag . more)
        (values (parse-range-binding #'Range.from_exclusive_to form1 #'rhs.parsed)
                #'rhs.tail)]))
   'none))

(define-binding-syntax <..=
  (binding-infix-operator
   (lambda () (order-quote enumeration))
   `()
   'automatic
   (lambda (form1 form2 stx)
     (parse-range-binding #'Range.from_exclusive_to_inclusive form1 form2))
   'none))

(define-for-syntax (parse-range-binding range . parseds)
  (syntax-parse #`(#,group-tag
                   #,range
                   (parens #,@(for/list ([parsed (in-list parseds)])
                                #`(#,group-tag (parsed #:rhombus/bind #,parsed)))))
    [b::binding #'b.parsed]))

(define-values-for-syntax (>=..-expr-infix >=..-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'mixfix
   (case-lambda
     [(left self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx)))
        #`(descending-range-from/who '>=..
                                     #,(discard-static-infos left)))
       (get-descending-range-static-infos))]
     [(left right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx right)))
        #`(descending-range-from-to/who '>=..
                                        #,(discard-static-infos left)
                                        #,(discard-static-infos right)))
       (get-descending-list-range-static-infos))])
   'none))

(define-syntax >=..
  >=..-expr-infix)

(define-repetition-syntax >=..
  >=..-repet-infix)

(define-values-for-syntax (>=..=-expr-infix >=..=-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'infix
   (lambda (left right self-stx)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list left self-stx right)))
       #`(descending-range-from-to-inclusive/who '>=..=
                                                 #,(discard-static-infos left)
                                                 #,(discard-static-infos right)))
      (get-descending-list-range-static-infos)))
   'none))

(define-syntax >=..=
  >=..=-expr-infix)

(define-repetition-syntax >=..=
  >=..=-repet-infix)

(define-values-for-syntax (>..-expr-infix >..-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'mixfix
   (case-lambda
     [(left self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx)))
        #`(descending-range-from-exclusive/who '>..
                                               #,(discard-static-infos left)))
       (get-descending-range-static-infos))]
     [(left right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx right)))
        #`(descending-range-from-exclusive-to/who '>..
                                                  #,(discard-static-infos left)
                                                  #,(discard-static-infos right)))
       (get-descending-list-range-static-infos))])
   'none))

(define-syntax >..
  >..-expr-infix)

(define-repetition-syntax >..
  >..-repet-infix)

(define-values-for-syntax (>..=-expr-infix >..=-repet-infix)
  (make-expression&repetition-infix-operator
   (lambda () (order-quote enumeration))
   '()
   'infix
   (lambda (left right self-stx)
     (wrap-static-info*
      (relocate+reraw
       (respan (datum->syntax #f (list left self-stx right)))
       #`(descending-range-from-exclusive-to-inclusive/who '>..=
                                                           #,(discard-static-infos left)
                                                           #,(discard-static-infos right)))
      (get-descending-list-range-static-infos)))
   'none))

(define-syntax >..=
  >..=-expr-infix)

(define-repetition-syntax >..=
  >..=-repet-infix)

(define (check-int who i)
  (unless (exact-integer? i)
    (raise-annotation-failure who i "Int")))

(define (check-pos-int who i)
  (unless (exact-positive-integer? i)
    (raise-annotation-failure who i "PosInt")))

(define (check-neg-int who i)
  (unless (and (integer? i) (exact? i) (negative? i))
    (raise-annotation-failure who i "NegInt")))

(define (check-start-end who start end)
  (unless (start . <= . end)
    (raise-arguments-error* who rhombus-realm
                            "starting point must be less than or equal to ending point"
                            "starting point" (unquoted-printing-string (number->string start))
                            "ending point" (unquoted-printing-string (number->string end)))))

(define (check-start-end/not-equal who start end)
  (unless (start . < . end)
    (raise-arguments-error* who rhombus-realm
                            "starting point must be less than ending point"
                            "starting point" (unquoted-printing-string (number->string start))
                            "ending point" (unquoted-printing-string (number->string end)))))

(define (check-start-end/descending who start end)
  (unless (start . >= . end)
    (raise-arguments-error* who rhombus-realm
                            "starting point must be greater than or equal to ending point"
                            "starting point" (unquoted-printing-string (number->string start))
                            "ending point" (unquoted-printing-string (number->string end)))))

(define (check-start-end/not-equal/descending who start end)
  (unless (start . > . end)
    (raise-arguments-error* who rhombus-realm
                            "starting point must be greater than ending point"
                            "starting point" (unquoted-printing-string (number->string start))
                            "ending point" (unquoted-printing-string (number->string end)))))

(define-syntax (define-range stx)
  (syntax-parse stx
    [(_ range name Name op-str (~or* (~and #:none)
                                     (~and #:left
                                           (~parse start #'start))
                                     (~and #:right
                                           (~parse end #'end))
                                     (~and #:both
                                           (~parse start #'start)
                                           (~parse end #'end)
                                           (~parse check-start-end #'check-start-end))
                                     (~and #:both-not-equal
                                           (~parse start #'start)
                                           (~parse end #'end)
                                           (~parse check-start-end #'check-start-end/not-equal))
                                     (~and #:left/descending
                                           (~parse start #'start)
                                           (~bind [descending? #t]))
                                     (~and #:both/descending
                                           (~parse start #'start)
                                           (~parse end #'end)
                                           (~parse check-start-end #'check-start-end/descending)
                                           (~bind [descending? #t]))
                                     (~and #:both-not-equal/descending
                                           (~parse start #'start)
                                           (~parse end #'end)
                                           (~parse check-start-end #'check-start-end/not-equal/descending)
                                           (~bind [descending? #t])))
        (~optional (~seq #:->sequence ->sequence))
        (~optional (~seq #:stream stream-is_empty stream-first stream-rest)))
     #:with range-method-table (datum->syntax
                                #'range
                                (string->symbol
                                 (format "~a-method-table" (syntax->datum #'range))))
     #:with unsafe-name (datum->syntax
                         #'name
                         (string->symbol
                          (format "unsafe-~a" (syntax->datum #'name))))
     #:attr name/who (datum->syntax
                      #'name
                      (string->symbol
                       (format "~a/who" (syntax->datum #'name))))
     #:attr name-start (and (attribute start)
                            (datum->syntax
                             #'name
                             (string->symbol
                              (format "~a-~a" (syntax->datum #'name) (syntax->datum #'start)))))
     #:attr name-end (and (attribute end)
                          (datum->syntax
                           #'name
                           (string->symbol
                            (format "~a-~a" (syntax->datum #'name) (syntax->datum #'end)))))
     #:with class-mode (if (attribute descending?)
                           #'#:just-constructor
                           #'#:just-constructor+binding)
     #:with default-step (if (attribute descending?)
                             #'-1
                             #'1)
     (syntax-local-lift-module-end-declaration
      #'(struct name range ((~? start) (~? end))
          #:omit-define-syntaxes
          #:authentic
          #:sealed
          #:constructor-name unsafe-name
          #:property prop:field-name->accessor (list* '() range-method-table #hasheq())
          #:property prop:equal+hash (list
                                      (lambda (v v2 eql?)
                                        (and (~? (eql? (name-start v)
                                                       (name-start v2)))
                                             (~? (eql? (name-end v)
                                                       (name-end v2)))))
                                      (lambda (v hc)
                                        (hash-code-combine
                                         (~? (hc (name-start v)))
                                         (~? (hc (name-end v)))))
                                      (lambda (v hc)
                                        (hash-code-combine
                                         (~? (hc (name-start v)))
                                         (~? (hc (name-end v))))))
          #:property prop:printer (lambda (v mode recur)
                                    (pretty-concat
                                     (~? (~@ (PrintDesc-doc (recur (name-start v)))
                                             (pretty-text " ")))
                                     (pretty-text 'op-str)
                                     (~? (~@ (pretty-text " ")
                                             (PrintDesc-doc (recur (name-end v)))))))
          (~? (~@ #:property prop:sequence (lambda (r) (->sequence r 'default-step))
                  #:property prop:stream (vector
                                          (lambda (s) ((~? stream-is_empty Range.is_empty) s))
                                          (lambda (s) ((~? stream-first SequenceRange.first) s))
                                          (lambda (s) ((~? stream-rest SequenceRange.rest) s)))))))
     (syntax-local-lift-module-end-declaration
      #'(define-primitive-class Name name
          #:existing
          class-mode #:no-primitive
          #:parent #f range
          #:fields
          ((~? [(start) #,(get-int-static-infos)])
           (~? [(end) #,(get-int-static-infos)]))
          #:properties
          ()
          #:methods
          ()))
     #'(begin
         (define (name/who who (~? start) (~? end))
           (~? (check-int who start))
           (~? (check-int who end))
           (~? (check-start-end who start end))
           (unsafe-name (~? start) (~? end)))
         (define name
           (let ([Name (lambda ((~? start) (~? end))
                         (name/who 'Name (~? start) (~? end)))])
             Name)))]))

(define-range list-range range-from-to Range.from_to ".." #:both
  #:->sequence range-from-to->sequence)

(define (range-from-to->sequence r step)
  (define start (range-from-to-start r))
  (define end (range-from-to-end r))
  (define (cont? i)
    (i . < . end))
  (range-sequence start step cont?))

(define (range-from-to->list r)
  (define start (range-from-to-start r))
  (define end (range-from-to-end r))
  (for/list ([i (in-range start end)])
    i))

(define (range-from-to->treelist r)
  (define start (range-from-to-start r))
  (define end (range-from-to-end r))
  (for/treelist ([i (in-range start end)])
    i))

(define-range list-range range-from-to-inclusive Range.from_to_inclusive "..=" #:both
  #:->sequence range-from-to-inclusive->sequence)

(define (range-from-to-inclusive->sequence r step)
  (define start (range-from-to-inclusive-start r))
  (define end (range-from-to-inclusive-end r))
  (define (cont? i)
    (i . <= . end))
  (range-sequence start step cont?))

(define (range-from-to-inclusive->list r)
  (define start (range-from-to-inclusive-start r))
  (define end (range-from-to-inclusive-end r))
  (for/list ([i (in-inclusive-range start end)])
    i))

(define (range-from-to-inclusive->treelist r)
  (define start (range-from-to-inclusive-start r))
  (define end (range-from-to-inclusive-end r))
  (for/treelist ([i (in-inclusive-range start end)])
    i))

(define-range sequence-range range-from Range.from ".." #:left
  #:->sequence range-from->sequence)

(define (range-from->sequence r step)
  (define start (range-from-start r))
  (range-sequence start step #f))

(define-range list-range range-from-exclusive-to Range.from_exclusive_to "<.." #:both-not-equal
  #:->sequence range-from-exclusive-to->sequence)

(define (range-from-exclusive-to->sequence r step)
  (define start (add1 (range-from-exclusive-to-start r)))
  (define end (range-from-exclusive-to-end r))
  (define (cont? i)
    (i . < . end))
  (range-sequence start step cont?))

(define (range-from-exclusive-to->list r)
  (define start (add1 (range-from-exclusive-to-start r)))
  (define end (range-from-exclusive-to-end r))
  (for/list ([i (in-range start end)])
    i))

(define (range-from-exclusive-to->treelist r)
  (define start (add1 (range-from-exclusive-to-start r)))
  (define end (range-from-exclusive-to-end r))
  (for/treelist ([i (in-range start end)])
    i))

(define-range list-range range-from-exclusive-to-inclusive Range.from_exclusive_to_inclusive "<..=" #:both
  #:->sequence range-from-exclusive-to-inclusive->sequence)

(define (range-from-exclusive-to-inclusive->sequence r step)
  (define start (add1 (range-from-exclusive-to-inclusive-start r)))
  (define end (range-from-exclusive-to-inclusive-end r))
  (define (cont? i)
    (i . <= . end))
  (range-sequence start step cont?))

(define (range-from-exclusive-to-inclusive->list r)
  (define start (add1 (range-from-exclusive-to-inclusive-start r)))
  (define end (range-from-exclusive-to-inclusive-end r))
  (for/list ([i (in-inclusive-range start end)])
    i))

(define (range-from-exclusive-to-inclusive->treelist r)
  (define start (add1 (range-from-exclusive-to-inclusive-start r)))
  (define end (range-from-exclusive-to-inclusive-end r))
  (for/treelist ([i (in-inclusive-range start end)])
    i))

(define-range sequence-range range-from-exclusive Range.from_exclusive "<.." #:left
  #:->sequence range-from-exclusive->sequence)

(define (range-from-exclusive->sequence r step)
  (define start (add1 (range-from-exclusive-start r)))
  (range-sequence start step #f))

(define-range range range-to Range.to ".." #:right)

(define-range range range-to-inclusive Range.to_inclusive "..=" #:right)

(define-range range range-full Range.full ".." #:none)

(define-syntax-rule (define-descending-range arg ...)
  (define-range arg ...
    #:stream DescendingRange.is_empty DescendingRange.first DescendingRange.rest))

(define-descending-range descending-list-range descending-range-from-to DescendingRange.from_to ">=.." #:both/descending
  #:->sequence descending-range-from-to->sequence)

(define (descending-range-from-to->sequence r step)
  (define start (descending-range-from-to-start r))
  (define end (descending-range-from-to-end r))
  (define (cont? i)
    (i . > . end))
  (range-sequence start step cont?))

(define (descending-range-from-to->list r)
  (define start (descending-range-from-to-start r))
  (define end (descending-range-from-to-end r))
  (for/list ([i (in-range start end -1)])
    i))

(define (descending-range-from-to->treelist r)
  (define start (descending-range-from-to-start r))
  (define end (descending-range-from-to-end r))
  (for/treelist ([i (in-range start end -1)])
    i))

(define-descending-range descending-list-range descending-range-from-to-inclusive DescendingRange.from_to_inclusive ">=..=" #:both/descending
  #:->sequence descending-range-from-to-inclusive->sequence)

(define (descending-range-from-to-inclusive->sequence r step)
  (define start (descending-range-from-to-inclusive-start r))
  (define end (descending-range-from-to-inclusive-end r))
  (define (cont? i)
    (i . >= . end))
  (range-sequence start step cont?))

(define (descending-range-from-to-inclusive->list r)
  (define start (descending-range-from-to-inclusive-start r))
  (define end (descending-range-from-to-inclusive-end r))
  (for/list ([i (in-inclusive-range start end -1)])
    i))

(define (descending-range-from-to-inclusive->treelist r)
  (define start (descending-range-from-to-inclusive-start r))
  (define end (descending-range-from-to-inclusive-end r))
  (for/treelist ([i (in-inclusive-range start end -1)])
    i))

(define-descending-range descending-range descending-range-from DescendingRange.from ">=.." #:left/descending
  #:->sequence descending-range-from->sequence)

(define (descending-range-from->sequence r step)
  (define start (descending-range-from-start r))
  (range-sequence start step #f))

(define-descending-range descending-list-range descending-range-from-exclusive-to DescendingRange.from_exclusive_to ">.." #:both-not-equal/descending
  #:->sequence descending-range-from-exclusive-to->sequence)

(define (descending-range-from-exclusive-to->sequence r step)
  (define start (sub1 (descending-range-from-exclusive-to-start r)))
  (define end (descending-range-from-exclusive-to-end r))
  (define (cont? i)
    (i . > . end))
  (range-sequence start step cont?))

(define (descending-range-from-exclusive-to->list r)
  (define start (sub1 (descending-range-from-exclusive-to-start r)))
  (define end (descending-range-from-exclusive-to-end r))
  (for/list ([i (in-range start end -1)])
    i))

(define (descending-range-from-exclusive-to->treelist r)
  (define start (sub1 (descending-range-from-exclusive-to-start r)))
  (define end (descending-range-from-exclusive-to-end r))
  (for/treelist ([i (in-range start end -1)])
    i))

(define-descending-range descending-list-range descending-range-from-exclusive-to-inclusive DescendingRange.from_exclusive_to_inclusive ">..=" #:both/descending
  #:->sequence descending-range-from-exclusive-to-inclusive->sequence)

(define (descending-range-from-exclusive-to-inclusive->sequence r step)
  (define start (sub1 (descending-range-from-exclusive-to-inclusive-start r)))
  (define end (descending-range-from-exclusive-to-inclusive-end r))
  (define (cont? i)
    (i . >= . end))
  (range-sequence start step cont?))

(define (descending-range-from-exclusive-to-inclusive->list r)
  (define start (sub1 (descending-range-from-exclusive-to-inclusive-start r)))
  (define end (descending-range-from-exclusive-to-inclusive-end r))
  (for/list ([i (in-inclusive-range start end -1)])
    i))

(define (descending-range-from-exclusive-to-inclusive->treelist r)
  (define start (sub1 (descending-range-from-exclusive-to-inclusive-start r)))
  (define end (descending-range-from-exclusive-to-inclusive-end r))
  (for/treelist ([i (in-inclusive-range start end -1)])
    i))

(define-descending-range descending-range descending-range-from-exclusive DescendingRange.from_exclusive ">.." #:left/descending
  #:->sequence descending-range-from-exclusive->sequence)

(define (descending-range-from-exclusive->sequence r step)
  (define start (sub1 (descending-range-from-exclusive-start r)))
  (range-sequence start step #f))

(define (range-sequence start step cont?)
  (define (inc i)
    (+ i step))
  (make-do-sequence
   (lambda ()
     (values values #f inc start cont? #f #f))))

(define (check-range who r)
  (unless (range? r)
    (raise-annotation-failure who r "Range")))

(define (check-sequence-range who r)
  (unless (sequence-range? r)
    (raise-annotation-failure who r "SequenceRange")))

(define (check-list-range who r)
  (unless (list-range? r)
    (raise-annotation-failure who r "ListRange")))

(define (check-descending-range who r)
  (unless (descending-range? r)
    (raise-annotation-failure who r "DescendingRange")))

(define (check-descending-list-range who r)
  (unless (descending-list-range? r)
    (raise-annotation-failure who r "DescendingListRange")))

(define/method (Range.start r)
  #:static-infos ((#%call-result #,(get-real-static-infos)))
  (check-range who r)
  (cond
    [(range-from-to? r) (range-from-to-start r)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive-start r)]
    [(range-from? r) (range-from-start r)]
    [(range-from-exclusive-to? r) (range-from-exclusive-to-start r)]
    [(range-from-exclusive-to-inclusive? r) (range-from-exclusive-to-inclusive-start r)]
    [(range-from-exclusive? r) (range-from-exclusive-start r)]
    [else -inf.0]))

(define/method (Range.end r)
  #:static-infos ((#%call-result #,(get-real-static-infos)))
  (check-range who r)
  (cond
    [(range-from-to? r) (range-from-to-end r)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive-end r)]
    [(range-from-exclusive-to? r) (range-from-exclusive-to-end r)]
    [(range-from-exclusive-to-inclusive? r) (range-from-exclusive-to-inclusive-end r)]
    [(range-to? r) (range-to-end r)]
    [(range-to-inclusive? r) (range-to-inclusive-end r)]
    [else +inf.0]))

(define/method (Range.includes_start r)
  (check-range who r)
  (or (range-from-to? r)
      (range-from-to-inclusive? r)
      (range-from? r)))

(define/method (Range.includes_end r)
  (check-range who r)
  (or (range-from-to-inclusive? r)
      (range-from-exclusive-to-inclusive? r)
      (range-to-inclusive? r)))

(define/method (Range.is_empty r)
  (check-range who r)
  (cond
    [(range-from-to? r)
     (eqv? (range-from-to-start r)
           (range-from-to-end r))]
    [(range-from-exclusive-to? r)
     (eqv? (add1 (range-from-exclusive-to-start r))
           (range-from-exclusive-to-end r))]
    [(range-from-exclusive-to-inclusive? r)
     (eqv? (range-from-exclusive-to-inclusive-start r)
           (range-from-exclusive-to-inclusive-end r))]
    [else #f]))

(define/method (Range.canonicalize r)
  #:static-infos ((#%call-result #,(get-range-static-infos)))
  (check-range who r)
  (cond
    [(range-from-to? r) r]
    [(range-from-to-inclusive? r)
     (unsafe-range-from-to (range-from-to-inclusive-start r)
                           (add1 (range-from-to-inclusive-end r)))]
    [(range-from? r) r]
    [(range-from-exclusive-to? r)
     ;; invariant for `<..` ensures that `start` cannot be equal to `end`
     ;; see `check-start-end/not-equal`
     (unsafe-range-from-to (add1 (range-from-exclusive-to-start r))
                           (range-from-exclusive-to-end r))]
    [(range-from-exclusive-to-inclusive? r)
     (unsafe-range-from-to (add1 (range-from-exclusive-to-inclusive-start r))
                           (add1 (range-from-exclusive-to-inclusive-end r)))]
    [(range-from-exclusive? r)
     (unsafe-range-from (add1 (range-from-exclusive-start r)))]
    [(range-to? r) r]
    [(range-to-inclusive? r)
     (unsafe-range-to (add1 (range-to-inclusive-end r)))]
    [else r]))

;; used by `substring` and alike, resembling `raise-range-error`
(define (range-canonical-start+end who type r in-val in-start in-end)
  (define-values (start end)
    (cond
      [(range-from-to? r)
       (values (range-from-to-start r)
               (range-from-to-end r))]
      [(range-from-to-inclusive? r)
       (values (range-from-to-inclusive-start r)
               (add1 (range-from-to-inclusive-end r)))]
      [(range-from? r)
       (values (range-from-start r) in-end)]
      [(range-from-exclusive-to? r)
       (values (add1 (range-from-exclusive-to-start r))
               (range-from-exclusive-to-end r))]
      [(range-from-exclusive-to-inclusive? r)
       (values (add1 (range-from-exclusive-to-inclusive-start r))
               (add1 (range-from-exclusive-to-inclusive-end r)))]
      [(range-from-exclusive? r)
       (values (add1 (range-from-exclusive-start r)) in-end)]
      [(range-to? r)
       (values in-start (range-to-end r))]
      [(range-to-inclusive? r)
       (values in-start (add1 (range-to-inclusive-end r)))]
      [(range-full? r)
       (values in-start in-end)]
      [else
       (raise-annotation-failure who r "Range")]))
  (unless (and (in-start . <= . start)
               (end . <= . in-end))
    (raise-arguments-error* who rhombus-realm
                            (string-append "derived range is invalid"
                                           ";\n derived range must be enclosed by valid range")
                            "given range" r
                            "derived range" (unsafe-range-from-to start end)
                            "valid range" (unsafe-range-from-to in-start in-end)
                            type in-val))
  (values start end))

;; NOTE The following implementation borrows from Rebellion, but does
;; so in a way that doesn't use cuts and therefore doesn't allocate.
(define (range-explode r)
  (cond
    [(range-from-to? r)
     (values (range-from-to-start r) #t
             (range-from-to-end r) #f)]
    [(range-from-to-inclusive? r)
     (values (range-from-to-inclusive-start r) #t
             (range-from-to-inclusive-end r) #t)]
    [(range-from? r)
     (values (range-from-start r) #t
             +inf.0 #f)]
    [(range-from-exclusive-to? r)
     (values (range-from-exclusive-to-start r) #f
             (range-from-exclusive-to-end r) #f)]
    [(range-from-exclusive-to-inclusive? r)
     (values (range-from-exclusive-to-inclusive-start r) #f
             (range-from-exclusive-to-inclusive-end r) #t)]
    [(range-from-exclusive? r)
     (values (range-from-exclusive-start r) #f
             +inf.0 #f)]
    [(range-to? r)
     (values -inf.0 #f
             (range-to-end r) #f)]
    [(range-to-inclusive? r)
     (values -inf.0 #f
             (range-to-inclusive-end r) #t)]
    [else
     (values -inf.0 #f
             +inf.0 #f)]))

(define (range-implode start in-start? end in-end?)
  (if (eqv? start -inf.0)
      (if (eqv? end +inf.0)
          (unsafe-range-full)
          (if in-end?
              (unsafe-range-to-inclusive end)
              (unsafe-range-to end)))
      (if (eqv? end +inf.0)
          (if in-start?
              (unsafe-range-from start)
              (unsafe-range-from-exclusive start))
          (if in-start?
              (if in-end?
                  (unsafe-range-from-to-inclusive start end)
                  (unsafe-range-from-to start end))
              (if in-end?
                  (unsafe-range-from-exclusive-to-inclusive start end)
                  (unsafe-range-from-exclusive-to start end))))))

;; `lower?`: `in-start?` or `(not in-end?)`
(define (bound<? point lower?
                 other-point other-lower?)
  (or (point . < . other-point)
      (and lower?
           (not other-lower?)
           (= point other-point))))

;; `upper?`: `(not in-start?)` or `in-end?`
(define (bound>? point upper?
                 other-point other-upper?)
  (or (point . > . other-point)
      (and upper?
           (not other-upper?)
           (= point other-point))))

(define (range-contains? r i)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (and (bound<? start in-start?
                i #f)
       (bound<? i #t
                end (not in-end?))))

(define/method (Range.contains r i)
  (check-range who r)
  (check-int who i)
  (range-contains? r i))

(define (range-encloses? r other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (and (not (bound>? start (not in-start?)
                     other-start (not other-in-start?)))
       (not (bound<? end (not in-end?)
                     other-end (not other-in-end?)))))

(define/method Range.encloses
  (case-lambda
    [() #t]
    [(r)
     (check-range who r)
     #t]
    [(r other-r)
     (check-range who r)
     (check-range who other-r)
     (range-encloses? r other-r)]
    [(r . other-rs)
     (check-range who r)
     (for ([other-r (in-list other-rs)])
       (check-range who other-r))
     (let loop ([r r] [other-rs other-rs])
       (cond
         [(null? other-rs) #t]
         [else (and (range-encloses? r (car other-rs))
                    (loop (car other-rs) (cdr other-rs)))]))]))

(define (range-connected?/internal start in-start? end in-end?
                                   other-start other-in-start? other-end other-in-end?)
  (and (not (bound>? start (not in-start?)
                     other-end other-in-end?))
       (not (bound>? other-start (not other-in-start?)
                     end in-end?))))

(define/method (Range.is_connected r other-r)
  (check-range who r)
  (check-range who other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (range-connected?/internal start in-start? end in-end?
                             other-start other-in-start? other-end other-in-end?))

(define/method (Range.overlaps r other-r)
  (check-range who r)
  (check-range who other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (and (bound<? start in-start?
                other-end (not other-in-end?))
       (bound>? end in-end?
                other-start (not other-in-start?))))

(define (range-span r other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (define-values (new-start new-in-start?)
    (if (bound>? start (not in-start?)
                 other-start (not other-in-start?))
        (values other-start other-in-start?)
        (values start in-start?)))
  (define-values (new-end new-in-end?)
    (if (bound<? end (not in-end?)
                 other-end (not other-in-end?))
        (values other-end other-in-end?)
        (values end in-end?)))
  (range-implode new-start new-in-start? new-end new-in-end?))

(define/method Range.span
  #:static-infos ((#%call-result #,(get-range-static-infos)))
  (case-lambda
    [(r)
     (check-range who r)
     r]
    [(r other-r)
     (check-range who r)
     (check-range who other-r)
     (range-span r other-r)]
    [(r . other-rs)
     (check-range who r)
     (for ([other-r (in-list other-rs)])
       (check-range who other-r))
     (for/fold ([r r])
               ([other-r (in-list other-rs)])
       (range-span r other-r))]))

(define/method (Range.gap r other-r)
  (check-range who r)
  (check-range who other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (cond
    [(not (bound<? start in-start?
                   other-end (not other-in-end?)))
     (range-implode other-end (not other-in-end?)
                    start (not in-start?))]
    [(not (bound>? end in-end?
                   other-start (not other-in-start?)))
     (range-implode end (not in-end?)
                    other-start (not other-in-start?))]
    [else #f]))

(define (range-intersect r other-r)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (define-values (other-start other-in-start? other-end other-in-end?)
    (range-explode other-r))
  (cond
    [(range-connected?/internal start in-start? end in-end?
                                other-start other-in-start? other-end other-in-end?)
     (define-values (new-start new-in-start?)
       (if (bound<? start in-start?
                    other-start other-in-start?)
           (values other-start other-in-start?)
           (values start in-start?)))
     (define-values (new-end new-in-end?)
       (if (bound>? end in-end?
                    other-end other-in-end?)
           (values other-end other-in-end?)
           (values end in-end?)))
     (range-implode new-start new-in-start? new-end new-in-end?)]
    [else #f]))

(define/method Range.intersect
  (case-lambda
    [()
     (unsafe-range-full)]
    [(r)
     (check-range who r)
     r]
    [(r other-r)
     (check-range who r)
     (check-range who other-r)
     (range-intersect r other-r)]
    [(r . other-rs)
     (check-range who r)
     (for ([other-r (in-list other-rs)])
       (check-range who other-r))
     (for/fold ([r r])
               ([other-r (in-list other-rs)])
       #:break (not r)
       (range-intersect r other-r))]))

(define (sequence-range->sequence r step)
  (cond
    [(range-from-to? r) (range-from-to->sequence r step)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive->sequence r step)]
    [(range-from? r) (range-from->sequence r step)]
    [(range-from-exclusive-to? r) (range-from-exclusive-to->sequence r step)]
    [(range-from-exclusive-to-inclusive? r) (range-from-exclusive-to-inclusive->sequence r step)]
    [else (range-from-exclusive->sequence r step)]))

(define-sequence-syntax SequenceRange.to_sequence/optimize
  (lambda () #'SequenceRange.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ range-expr/statinfo)]
       (define who 'SequenceRange.to_sequence)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (sequence-range-sequence/inline/optimize #'id who range-expr)]
      [_ #f])))

(define/method (SequenceRange.to_sequence r)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-sequence-range who r)
  (sequence-range->sequence r 1))

(define-sequence-syntax SequenceRange.step_by/optimize
  (lambda () #'values)
  (lambda (stx)
    (syntax-parse stx
      #:literals (SequenceRange.step_by/bounce)
      [[(id) (_ step-by-expr/statinfo)]
       #:do [(define step-by-expr (unwrap-static-infos #'step-by-expr/statinfo))]
       #:with (SequenceRange.step_by/bounce range-expr/statinfo step-expr/statinfo) step-by-expr
       (define who 'SequenceRange.step_by)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (define step-expr (unwrap-static-infos #'step-expr/statinfo))
       (sequence-range-sequence/inline/optimize #'id who range-expr
                                                #:step-expr step-expr)]
      [_ #f])))

(define/arity (SequenceRange.step_by/bounce r step)
  #:static-infos ((#%call-result ((#%sequence-constructor SequenceRange.step_by/optimize)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (SequenceRange.step_by r step))

(define/method #:direct-id SequenceRange.step_by/bounce (SequenceRange.step_by r step)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-sequence-range who r)
  (check-pos-int who step)
  (sequence-range->sequence r step))

(define (sequence-range-stream-op who r next?)
  (define (wrong)
    (raise-annotation-failure who r "SequenceRange && !satisying(Range.is_empty)"))
  (cond
    [(range-from-to? r)
     (define start (range-from-to-start r))
     (define end (range-from-to-end r))
     (if (start . < . end)
         (if next?
             (range-from-to (add1 start) end)
             start)
         (wrong))]
    [(range-from-to-inclusive? r)
     (define start (range-from-to-inclusive-start r))
     (define end (range-from-to-inclusive-end r))
     (if (start . <= . end)
         (if next?
             (if (start . = . end)
                 (range-from-to (add1 start) (add1 start))
                 (range-from-to-inclusive (add1 start) end))
             start)
         (wrong))]
    [(range-from? r)
     (define start (range-from-start r))
     (if next?
         (range-from (add1 start))
         start)]
    [(range-from-exclusive-to? r)
     (define start+1 (add1 (range-from-exclusive-to-start r)))
     (define end (range-from-exclusive-to-end r))
     (if (start+1 . < . end)
         (if next?
             (range-from-exclusive-to start+1 end)
             start+1)
         (wrong))]
    [(range-from-exclusive-to-inclusive? r)
     (define start (range-from-exclusive-to-inclusive-start r))
     (define end (range-from-exclusive-to-inclusive-end r))
     (if (start . < . end)
         (if next?
             (range-from-exclusive-to-inclusive (add1 start) end)
             (add1 start))
         (wrong))]
    [(range-from-exclusive? r)
     (define start+1 (add1 (range-from-exclusive-start r)))
     (if next?
         (range-from-exclusive start+1)
         start+1)]
    [else (wrong)]))

(define/method (SequenceRange.first r)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (define (wrong)
    (raise-annotation-failure who r "SequenceRange && !satisying(Range.is_empty)"))
  (sequence-range-stream-op who r #f))

(define/method (SequenceRange.rest r)
  #:static-infos ((#%call-result #,(get-sequence-range-static-infos)))
  (define (wrong)
    (raise-annotation-failure who r "SequenceRange && !satisying(Range.is_empty)"))
  (sequence-range-stream-op who r #t))

(define-for-syntax (range-explode/inline range-expr)
  (define-values (who-stx start-expr start-type end-expr end-type)
    (syntax-parse range-expr
      #:literals (quote
                  range-from-to/who
                  range-from-to-inclusive/who
                  range-from/who
                  range-from-exclusive-to/who
                  range-from-exclusive-to-inclusive/who
                  range-from-exclusive/who
                  range-to/who
                  range-to-inclusive/who
                  range-full/who)
      [(range-from-to/who 'who start-expr end-expr)
       (values #'who #'start-expr 'inclusive #'end-expr 'exclusive)]
      [(range-from-to-inclusive/who 'who start-expr end-expr)
       (values #'who #'start-expr 'inclusive #'end-expr 'inclusive)]
      [(range-from/who 'who start-expr)
       (values #'who #'start-expr 'inclusive #f 'infinite)]
      [(range-from-exclusive-to/who 'who start-expr end-expr)
       (values #'who #'start-expr 'exclusive #'end-expr 'exclusive)]
      [(range-from-exclusive-to-inclusive/who 'who start-expr end-expr)
       (values #'who #'start-expr 'exclusive #'end-expr 'inclusive)]
      [(range-from-exclusive/who 'who start-expr)
       (values #'who #'start-expr 'exclusive #f 'infinite)]
      [(range-to/who 'who end-expr)
       (values #'who #f 'infinite #'end-expr 'exclusive)]
      [(range-to-inclusive/who 'who end-expr)
       (values #'who #f 'infinite #'end-expr 'inclusive)]
      [(range-full/who 'who)
       (values #'who #f 'infinite #f 'infinite)]
      [_
       (values #f #f #f #f #f)]))
  (define check-start-end-stx
    (case start-type
      [(inclusive)
       (case end-type
         [(exclusive inclusive) #'check-start-end]
         [(infinite) #f]
         [else (error "cannot happen")])]
      [(exclusive)
       (case end-type
         [(exclusive) #'check-start-end/not-equal]
         [(inclusive) #'check-start-end]
         [(infinite) #f]
         [else (error "cannot happen")])]
      [(infinite #f) #f]
      [else (error "cannot happen")]))
  (values (and who-stx (syntax-e who-stx))
          (and start-expr (discard-static-infos start-expr))
          start-type
          (and end-expr (discard-static-infos end-expr))
          end-type
          #f ; rev?
          check-start-end-stx))

(define (sequence-range-normalize r)
  (cond
    [(range-from-to? r)
     (values (range-from-to-start r)
             (range-from-to-end r)
             <)]
    [(range-from-to-inclusive? r)
     (values (range-from-to-inclusive-start r)
             (range-from-to-inclusive-end r)
             <=)]
    [(range-from? r)
     (values (range-from-start r)
             #f
             #f)]
    [(range-from-exclusive-to? r)
     (values (add1 (range-from-exclusive-to-start r))
             (range-from-exclusive-to-end r)
             <)]
    [(range-from-exclusive-to-inclusive? r)
     (values (add1 (range-from-exclusive-to-inclusive-start r))
             (range-from-exclusive-to-inclusive-end r)
             <=)]
    [else
     (values (add1 (range-from-exclusive-start r))
             #f
             #f)]))

(define-for-syntax (sequence-range-sequence/inline/optimize id who range-expr
                                                            #:step-expr [step-expr #f])
  (do-range-sequence/inline/optimize id who range-expr
                                     #'check-sequence-range #'<? #'sequence-range-normalize
                                     #'add1 sub1
                                     #'check-pos-int 1 who step-expr
                                     #'unsafe-fx< #'<
                                     #'unsafe-fx<= #'<=
                                     (list range-explode/inline)))

(define (list-range->descending-range r)
  (cond
    [(range-from-to? r)
     (unsafe-descending-range-from-exclusive-to-inclusive (range-from-to-end r)
                                                          (range-from-to-start r))]
    [(range-from-to-inclusive? r)
     (unsafe-descending-range-from-to-inclusive (range-from-to-inclusive-end r)
                                                (range-from-to-inclusive-start r))]
    [(range-from-exclusive-to? r)
     (unsafe-descending-range-from-exclusive-to (range-from-exclusive-to-end r)
                                                (range-from-exclusive-to-start r))]
    [else
     (unsafe-descending-range-from-to (range-from-exclusive-to-inclusive-end r)
                                      (range-from-exclusive-to-inclusive-start r))]))

(define/arity (ListRange.descending/bounce r)
  #:static-infos ((#%call-result #,(get-descending-list-range-static-infos)))
  (ListRange.descending r))

(define/method #:direct-id ListRange.descending/bounce (ListRange.descending r)
  #:static-infos ((#%call-result #,(get-descending-list-range-static-infos)))
  (check-list-range who r)
  (list-range->descending-range r))

(define (list-range->list r)
  (cond
    [(range-from-to? r) (range-from-to->list r)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive->list r)]
    [(range-from-exclusive-to? r) (range-from-exclusive-to->list r)]
    [else (range-from-exclusive-to-inclusive->list r)]))

(define (list-range->treelist r)
  (cond
    [(range-from-to? r) (range-from-to->treelist r)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive->treelist r)]
    [(range-from-exclusive-to? r) (range-from-exclusive-to->treelist r)]
    [else (range-from-exclusive-to-inclusive->treelist r)]))

(define/method (ListRange.to_list r)
  #:static-infos ((#%call-result ((#%index-result #,(get-int-static-infos))
                                  #,@(indirect-get-treelist-static-infos))))
  (check-list-range who r)
  (list-range->treelist r))

(define (descending-range->sequence r step)
  (cond
    [(descending-range-from-to? r) (descending-range-from-to->sequence r step)]
    [(descending-range-from-to-inclusive? r) (descending-range-from-to-inclusive->sequence r step)]
    [(descending-range-from? r) (descending-range-from->sequence r step)]
    [(descending-range-from-exclusive-to? r) (descending-range-from-exclusive-to->sequence r step)]
    [(descending-range-from-exclusive-to-inclusive? r) (descending-range-from-exclusive-to-inclusive->sequence r step)]
    [else (descending-range-from-exclusive->sequence r step)]))

(define-sequence-syntax DescendingRange.to_sequence/optimize
  (lambda () #'DescendingRange.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ range-expr/statinfo)]
       (define who 'DescendingRange.to_sequence)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (descending-range-sequence/inline/optimize #'id who range-expr)]
      [_ #f])))

(define/method (DescendingRange.to_sequence r)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-descending-range who r)
  (descending-range->sequence r -1))

(define-sequence-syntax DescendingRange.step_by/optimize
  (lambda () #'values)
  (lambda (stx)
    (syntax-parse stx
      #:literals (DescendingRange.step_by/bounce)
      [[(id) (_ step-by-expr/statinfo)]
       #:do [(define step-by-expr (unwrap-static-infos #'step-by-expr/statinfo))]
       #:with (DescendingRange.step_by/bounce range-expr/statinfo step-expr/statinfo) step-by-expr
       (define who 'DescendingRange.step_by)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (define step-expr (unwrap-static-infos #'step-expr/statinfo))
       (descending-range-sequence/inline/optimize #'id who range-expr
                                                  #:step-expr step-expr)]
      [_ #f])))

(define/arity (DescendingRange.step_by/bounce r step)
  #:static-infos ((#%call-result ((#%sequence-constructor DescendingRange.step_by/optimize)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (DescendingRange.step_by r step))

(define/method #:direct-id DescendingRange.step_by/bounce (DescendingRange.step_by r step)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-descending-range who r)
  (check-neg-int who step)
  (descending-range->sequence r step))

(define-for-syntax (descending-range-explode/inline range-expr)
  (define-values (who-stx start-expr start-type end-expr end-type)
    (syntax-parse range-expr
      #:literals (quote
                  descending-range-from-to/who
                  descending-range-from-to-inclusive/who
                  descending-range-from/who
                  descending-range-from-exclusive-to/who
                  descending-range-from-exclusive-to-inclusive/who
                  descending-range-from-exclusive/who)
      [(descending-range-from-to/who 'who start-expr end-expr)
       (values #'who #'start-expr 'inclusive #'end-expr 'exclusive)]
      [(descending-range-from-to-inclusive/who 'who start-expr end-expr)
       (values #'who #'start-expr 'inclusive #'end-expr 'inclusive)]
      [(descending-range-from/who 'who start-expr)
       (values #'who #'start-expr 'inclusive #f 'infinite)]
      [(descending-range-from-exclusive-to/who 'who start-expr end-expr)
       (values #'who #'start-expr 'exclusive #'end-expr 'exclusive)]
      [(descending-range-from-exclusive-to-inclusive/who 'who start-expr end-expr)
       (values #'who #'start-expr 'exclusive #'end-expr 'inclusive)]
      [(descending-range-from-exclusive/who 'who start-expr)
       (values #'who #'start-expr 'exclusive #f 'infinite)]
      [_
       (values #f #f #f #f #f)]))
  (define check-start-end-stx
    (case start-type
      [(inclusive)
       (case end-type
         [(exclusive inclusive) #'check-start-end/descending]
         [(infinite) #f]
         [else (error "cannot happen")])]
      [(exclusive)
       (case end-type
         [(exclusive) #'check-start-end/not-equal/descending]
         [(inclusive) #'check-start-end/descending]
         [(infinite) #f]
         [else (error "cannot happen")])]
      [(#f) #f]
      [else (error "cannot happen")]))
  (values (and who-stx (syntax-e who-stx))
          (and start-expr (discard-static-infos start-expr))
          start-type
          (and end-expr (discard-static-infos end-expr))
          end-type
          #f ; rev?
          check-start-end-stx))

(define-for-syntax (range-explode/inline/rev range-expr)
  (define inner-range-expr
    (syntax-parse range-expr
      #:literals (ListRange.descending/bounce)
      [(ListRange.descending/bounce r) (unwrap-static-infos #'r)]
      [_ #f]))
  (define-values (who-stx start-expr start-type end-expr end-type)
    (if inner-range-expr
        (syntax-parse inner-range-expr
          #:literals (quote
                      range-from-to/who
                      range-from-to-inclusive/who
                      range-from-exclusive-to/who
                      range-from-exclusive-to-inclusive/who)
          [(range-from-to/who 'who start-expr end-expr)
           (values #'who #'start-expr 'inclusive #'end-expr 'exclusive)]
          [(range-from-to-inclusive/who 'who start-expr end-expr)
           (values #'who #'start-expr 'inclusive #'end-expr 'inclusive)]
          [(range-from-exclusive-to/who 'who start-expr end-expr)
           (values #'who #'start-expr 'exclusive #'end-expr 'exclusive)]
          [(range-from-exclusive-to-inclusive/who 'who start-expr end-expr)
           (values #'who #'start-expr 'exclusive #'end-expr 'inclusive)]
          [_
           (values #f #f #f #f #f)])
        (values #f #f #f #f #f)))
  (define check-start-end-stx
    (case start-type
      [(inclusive)
       (case end-type
         [(exclusive inclusive) #'check-start-end]
         [else (error "cannot happen")])]
      [(exclusive)
       (case end-type
         [(exclusive) #'check-start-end/not-equal]
         [(inclusive) #'check-start-end]
         [else (error "cannot happen")])]
      [(#f) #f]
      [else (error "cannot happen")]))
  (values (and who-stx (syntax-e who-stx))
          (and end-expr (discard-static-infos end-expr))
          end-type
          (and start-expr (discard-static-infos start-expr))
          start-type
          #t ; rev?
          check-start-end-stx))

(define (descending-range-normalize r)
  (cond
    [(descending-range-from-to? r)
     (values (descending-range-from-to-start r)
             (descending-range-from-to-end r)
             >)]
    [(descending-range-from-to-inclusive? r)
     (values (descending-range-from-to-inclusive-start r)
             (descending-range-from-to-inclusive-end r)
             >=)]
    [(descending-range-from? r)
     (values (descending-range-from-start r)
             #f
             #f)]
    [(descending-range-from-exclusive-to? r)
     (values (sub1 (descending-range-from-exclusive-to-start r))
             (descending-range-from-exclusive-to-end r)
             >)]
    [(descending-range-from-exclusive-to-inclusive? r)
     (values (sub1 (descending-range-from-exclusive-to-inclusive-start r))
             (descending-range-from-exclusive-to-inclusive-end r)
             >=)]
    [else
     (values (sub1 (descending-range-from-exclusive-start r))
             #f
             #f)]))

(define-for-syntax (descending-range-sequence/inline/optimize id who range-expr
                                                              #:step-expr [step-expr #f])
  (do-range-sequence/inline/optimize id who range-expr
                                     #'check-descending-range #'>? #'descending-range-normalize
                                     #'sub1 add1
                                     #'check-neg-int -1 who step-expr
                                     #'unsafe-fx> #'>
                                     #'unsafe-fx>= #'>=
                                     (list descending-range-explode/inline
                                           range-explode/inline/rev)))

(define (descending-list-range->list r)
  (cond
    [(descending-range-from-to? r) (descending-range-from-to->list r)]
    [(descending-range-from-to-inclusive? r) (descending-range-from-to-inclusive->list r)]
    [(descending-range-from-exclusive-to? r) (descending-range-from-exclusive-to->list r)]
    [else (descending-range-from-exclusive-to-inclusive->list r)]))

(define (descending-list-range->treelist r)
  (cond
    [(descending-range-from-to? r) (descending-range-from-to->treelist r)]
    [(descending-range-from-to-inclusive? r) (descending-range-from-to-inclusive->treelist r)]
    [(descending-range-from-exclusive-to? r) (descending-range-from-exclusive-to->treelist r)]
    [else (descending-range-from-exclusive-to-inclusive->treelist r)]))

(define/method (DescendingListRange.to_list r)
  #:static-infos ((#%call-result ((#%index-result #,(get-int-static-infos))
                                  #,@(indirect-get-treelist-static-infos))))
  (check-descending-list-range who r)
  (descending-list-range->treelist r))

(define (descending-range-stream-op who r next?)
  (define (wrong)
    (raise-annotation-failure who r "DescendingRange && !satisying(DescendingRange.is_empty)"))
  (cond
    [(descending-range-from-to? r)
     (define start (descending-range-from-to-start r))
     (define end (descending-range-from-to-end r))
     (if (start . > . end)
         (if next?
             (descending-range-from-to (sub1 start) end)
             start)
         (wrong))]
    [(descending-range-from-to-inclusive? r)
     (define start (descending-range-from-to-inclusive-start r))
     (define end (descending-range-from-to-inclusive-end r))
     (if (start . >= . end)
         (if next?
             (if (start . = . end)
                 (descending-range-from-exclusive-to-inclusive (sub1 start) (sub1 start))
                 (descending-range-from-to-inclusive (sub1 start) end))
             start)
         (wrong))]
    [(descending-range-from-exclusive-to? r)
     (define start-1 (sub1 (descending-range-from-exclusive-to-start r)))
     (define end (descending-range-from-exclusive-to-end r))
     (if (start-1 . > . end)
         (if next?
             (descending-range-from-exclusive-to start-1 end)
             start-1)
         (wrong))]
    [(descending-range-from-exclusive-to-inclusive? r)
     (define start (descending-range-from-exclusive-to-inclusive-start r))
     (define end (descending-range-from-exclusive-to-inclusive-end r))
     (if (start . > . end)
         (if next?
             (descending-range-from-exclusive-to-inclusive (sub1 start) end)
             (sub1 start))
         (wrong))]
    [else (wrong)]))

(define/method (DescendingRange.first r)
  (descending-range-stream-op who r #f))

(define/method (DescendingRange.rest r)
  (descending-range-stream-op who r #t))

(define/method (DescendingRange.is_empty r)
  (cond
    [(descending-range-from-to? r)
     (= (descending-range-from-to-start r)
        (descending-range-from-to-end r))]
    [(descending-range-from-to-inclusive? r)
     #f]
    [(descending-range-from-exclusive-to? r)
     (= (sub1 (descending-range-from-exclusive-to-start r))
        (descending-range-from-exclusive-to-end r))]
    [(descending-range-from-exclusive-to-inclusive? r)
     (= (descending-range-from-exclusive-to-inclusive-start r)
        (descending-range-from-exclusive-to-inclusive-end r))]
    [else
     (raise-annotation-failure who r "DescendingRange")]))

(define-for-syntax (do-range-sequence/inline/optimize id who range-expr
                                                      check-sequence-range-stx <?-id sequence-range-normalize-stx
                                                      add1-stx do-sub1
                                                      check-step-stx default-step step-who step-expr
                                                      unsafe-fx<-stx <-stx
                                                      unsafe-fx<=-stx <=-stx
                                                      range-explode/inlines)
  (define (try-inline range-explode/inline)
    (define-values (range-who start-expr start-type end-expr end-type rev? check-start-end-stx)
      (range-explode/inline range-expr))
    (and (or (eq? start-type 'inclusive)
             (eq? start-type 'exclusive))
         (do-range-sequence/inline id range-who start-expr start-type end-expr end-type rev?
                                   check-start-end-stx add1-stx do-sub1
                                   check-step-stx default-step step-who step-expr
                                   unsafe-fx<-stx <-stx
                                   unsafe-fx<=-stx <=-stx)))
  (or (ormap try-inline range-explode/inlines)
      (do-range-sequence/optimize id who range-expr
                                  check-sequence-range-stx <?-id sequence-range-normalize-stx
                                  check-step-stx default-step step-who step-expr)))

(define-for-syntax (do-range-sequence/inline id range-who start-expr start-type end-expr end-type rev?
                                             check-start-end-stx add1-stx do-sub1
                                             check-step-stx default-step step-who step-expr
                                             unsafe-fx<-stx <-stx
                                             unsafe-fx<=-stx <=-stx)
  (define (maybe-unwrap-fixnum stx)
    (syntax-parse stx
      #:literals (quote)
      [(quote datum-stx)
       (define datum (syntax-e #'datum-stx))
       (and (fixnum-for-every-system? datum)
            datum)]
      [_ #f]))
  (define use-unsafe? (and (maybe-unwrap-fixnum start-expr)
                           (let* ([end (and end-expr
                                            (maybe-unwrap-fixnum end-expr))]
                                  [step (and end
                                             (if step-expr
                                                 (maybe-unwrap-fixnum step-expr)
                                                 default-step))])
                             ;; conservatively check that the biggest
                             ;; ending integer must be fixnum
                             (and step
                                  (fixnum-for-every-system?
                                   (+ (case end-type
                                        [(exclusive) (do-sub1 end)]
                                        [(inclusive) end]
                                        [else (error "cannot happen")])
                                      step))))))
  (define +-stx (if use-unsafe? #'unsafe-fx+ #'+))
  #`[(#,id) (:do-in
             #,(let ([start-bind #`[(start) #,start-expr]]
                     [maybe-end-bind (if end-expr
                                         (list #`[(end) #,end-expr])
                                         '())]
                     [maybe-step-bind (if step-expr
                                          (list #`[(step) #,step-expr])
                                          '())])
                 (if (not rev?)
                     #`(#,start-bind
                        #,@maybe-end-bind
                        #,@maybe-step-bind)
                     #`(#,@maybe-end-bind
                        #,start-bind
                        #,@maybe-step-bind)))
             (unless (variable-reference-from-unsafe? (#%variable-reference))
               #,@(let ([start-check (list #`(check-int '#,range-who start))]
                        [maybe-end-check (if end-expr
                                             (list #`(check-int '#,range-who end))
                                             '())]
                        [maybe-start-end-check (if end-expr
                                                   (list (if (not rev?)
                                                             #`(#,check-start-end-stx '#,range-who start end)
                                                             #`(#,check-start-end-stx '#,range-who end start)))
                                                   '())]
                        [maybe-step-check (if step-expr
                                              (list #`(#,check-step-stx '#,step-who step))
                                              '())])
                    (if (not rev?)
                        (append start-check
                                maybe-end-check
                                maybe-start-end-check
                                maybe-step-check)
                        (append maybe-end-check
                                start-check
                                maybe-start-end-check
                                maybe-step-check))))
             ([pos #,(case start-type
                       [(exclusive) #`(#,add1-stx start)]
                       [(inclusive) #'start]
                       [else (error "cannot happen")])])
             #,(if end-expr
                   (let ([<-stx (case end-type
                                  [(exclusive) (if use-unsafe? unsafe-fx<-stx <-stx)]
                                  [(inclusive) (if use-unsafe? unsafe-fx<=-stx <=-stx)]
                                  [else (error "cannot happen")])])
                     #`(pos . #,<-stx . end))
                   #'#t)
             ([(#,id) pos])
             #t
             #t
             ((#,+-stx pos #,(if step-expr #'step #`'#,default-step))))])

(define-for-syntax (do-range-sequence/optimize id who range-expr
                                               check-sequence-range-stx <?-id sequence-range-normalize-stx
                                               check-step-stx default-step step-who step-expr)
  #`[(#,id) (:do-in
             ([(start end #,<?-id) (let ([rge #,range-expr])
                                     (unless (variable-reference-from-unsafe? (#%variable-reference))
                                       (#,check-sequence-range-stx '#,who rge))
                                     (#,sequence-range-normalize-stx rge))]
              #,@(if step-expr
                     (list #`[(step) (let ([step #,step-expr])
                                       (unless (variable-reference-from-unsafe? (#%variable-reference))
                                         (#,check-step-stx '#,step-who step))
                                       step)])
                     '()))
             (void)
             ([pos start])
             (if end (pos . #,<?-id . end) #t)
             ([(#,id) pos])
             #t
             #t
             ((+ pos #,(if step-expr #'step #`'#,default-step))))])

(void (set-range->list! list-range->list list-range->treelist))
(void (set-descending-range->list! descending-list-range->list descending-list-range->treelist))

(begin-for-syntax
  (void (install-range #'range? #'range-contains? range-explode/inline)))
