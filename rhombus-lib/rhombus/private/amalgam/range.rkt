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
                  unsafe-fx<=)
         "expression.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         "static-info.rkt"
         "parse.rkt"
         (prefix-in rhombus-a: "arithmetic.rkt")
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "treelist.rkt"
         (submod "list.rkt" for-listable)
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
         "order.rkt"
         "order-primitive.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Range)
         (for-spaces (rhombus/annot)
                     SequenceRange
                     ListRange)
         (for-spaces (#f
                      rhombus/repet
                      rhombus/bind)
                     ..
                     ..=
                     <..<
                     <..=))

(module+ for-container
  (provide range?
           Range.contains))

(define-primitive-class Range range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%contains Range.contains))
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
   [full Range.full]
   [to_sequence Range.to_sequence]
   [step_by Range.step_by]
   [to_list Range.to_list])
  #:properties
  ()
  #:methods
  (start
   end
   includes_start
   includes_end
   contains
   encloses
   is_connected
   overlaps
   span
   gap
   intersect))

(define-static-info-getter get-sequence-range-static-infos/sequence
  (#%sequence-constructor Range.to_sequence/optimize)
  (#%sequence-element #,(get-int-static-infos))
  #,@(get-range-static-infos))

(define-primitive-class SequenceRange sequence-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-sequence-range-static-infos/sequence)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f range
  #:fields ()
  #:namespace-fields
  (#:no-methods)
  #:properties
  ()
  #:methods
  ([to_sequence Range.to_sequence]
   [step_by Range.step_by]))

(define-primitive-class ListRange list-range
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-sequence-range-static-infos/sequence)
  #:existing
  #:just-annot #:no-primitive
  #:parent #f sequence-range
  #:fields ()
  #:namespace-fields
  (#:no-methods)
  #:properties
  ()
  #:methods
  ([to_list Range.to_list]))

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
        #`(range-full))
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
        #`(range-from-exclusive/who '<..<
                                    #,(discard-static-infos left)))
       (get-range-static-infos))]
     [(left right self-stx)
      (wrap-static-info*
       (relocate+reraw
        (respan (datum->syntax #f (list left self-stx right)))
        #`(range-from-exclusive-to/who '<..<
                                       #,(discard-static-infos left)
                                       #,(discard-static-infos right)))
       (get-range-static-infos))])
   'none))

(define-syntax <..<
  <..-expr-infix)

(define-repetition-syntax <..<
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
      (get-range-static-infos)))
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

(define-binding-syntax <..<
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
        #:with (~var rhs (:infix-op+binding+tail #'<..<)) #`(#,group-tag . more)
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

(define (check-int who i)
  (unless (exact-integer? i)
    (raise-annotation-failure who i "Int")))

(define (check-pos-int who i)
  (unless (exact-positive-integer? i)
    (raise-annotation-failure who i "PosInt")))

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

(define (step->inc step)
  (if step
      (lambda (i) (+ i step))
      add1))

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
                                           (~parse check-start-end #'check-start-end/not-equal)))
        (~optional (~seq #:->sequence ->sequence
                         (~optional (~seq #:->list ->list)))))
     #:with range-method-table (datum->syntax
                                #'range
                                (string->symbol
                                 (format "~a-method-table" (syntax->datum #'range))))
     #:with unsafe-name (datum->syntax
                         #'name
                         (string->symbol
                          (format "unsafe-~a" (syntax->datum #'name))))
     #:attr name/who (and (or (attribute start)
                              (attribute end))
                          (datum->syntax
                           #'name
                           (string->symbol
                            (format "~a/who" (syntax->datum #'name)))))
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
          (~? (~@ #:property prop:sequence (lambda (r) (->sequence r))))
          (~? (~@ #:property prop:Listable (vector (lambda (r) (->list r)))))))
     (syntax-local-lift-module-end-declaration
      #'(define-primitive-class Name name
          #:existing
          #:just-binding #:no-primitive
          #:parent #f range
          #:fields
          ((~? [(start) #,(get-int-static-infos)])
           (~? [(end) #,(get-int-static-infos)]))
          #:properties
          ()
          #:methods
          ()))
     (if (attribute name/who)
         #'(begin
             (define (name/who who (~? start) (~? end))
               (~? (check-int who start))
               (~? (check-int who end))
               (~? (check-start-end who start end))
               (unsafe-name (~? start) (~? end)))
             (define name
               (let ([Name (lambda ((~? start) (~? end))
                             (name/who 'Name (~? start) (~? end)))])
                 Name)))
         #'(define name
             (let ([Name (lambda ()
                           (unsafe-name))])
               Name)))]))

(struct range () #:authentic)
(struct sequence-range range () #:authentic)
(struct list-range sequence-range () #:authentic)

(define-range list-range range-from-to Range.from_to ".." #:both
  #:->sequence range-from-to->sequence
  #:->list range-from-to->list)

(define (range-from-to->sequence r [step #f])
  (define start (range-from-to-start r))
  (define end (range-from-to-end r))
  (define (cont? i)
    (i . < . end))
  (range-sequence start (step->inc step) cont?))

(define (range-from-to->list r)
  (define start (range-from-to-start r))
  (define end (range-from-to-end r))
  (for/treelist ([i (in-range start end)])
    i))

(define-range list-range range-from-to-inclusive Range.from_to_inclusive "..=" #:both
  #:->sequence range-from-to-inclusive->sequence
  #:->list range-from-to-inclusive->list)

(define (range-from-to-inclusive->sequence r [step #f])
  (define start (range-from-to-inclusive-start r))
  (define end (range-from-to-inclusive-end r))
  (define (cont? i)
    (i . <= . end))
  (range-sequence start (step->inc step) cont?))

(define (range-from-to-inclusive->list r)
  (define start (range-from-to-inclusive-start r))
  (define end (range-from-to-inclusive-end r))
  (for/treelist ([i (in-inclusive-range start end)])
    i))

(define-range sequence-range range-from Range.from ".." #:left
  #:->sequence range-from->sequence)

(define (range-from->sequence r [step #f])
  (define start (range-from-start r))
  (range-sequence start (step->inc step) #f))

(define-range range range-from-exclusive-to Range.from_exclusive_to "<..<" #:both-not-equal)

(define-range range range-from-exclusive-to-inclusive Range.from_exclusive_to_inclusive "<..=" #:both)

(define-range range range-from-exclusive Range.from_exclusive "<..<" #:left)

(define-range range range-to Range.to ".." #:right)

(define-range range range-to-inclusive Range.to_inclusive "..=" #:right)

(define-range range range-full Range.full ".." #:none)

(define (range-sequence start inc cont?)
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

(define/method (Range.contains r i)
  (check-range who r)
  (check-int who i)
  (define-values (start in-start? end in-end?)
    (range-explode r))
  (and (bound<? start in-start?
                i #f)
       (bound<? i #t
                end (not in-end?))))

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

(define-for-syntax (sequence-range-normalize/inline range-expr)
  (syntax-parse range-expr
    #:literals (quote range-from-to/who range-from-to-inclusive/who range-from/who)
    [(range-from-to/who 'who start-expr end-expr)
     (values (syntax-e #'who)
             (unwrap-static-infos #'start-expr) (unwrap-static-infos #'end-expr)
             'exclusive)]
    [(range-from-to-inclusive/who 'who start-expr end-expr)
     (values (syntax-e #'who)
             (unwrap-static-infos #'start-expr) (unwrap-static-infos #'end-expr)
             'inclusive)]
    [(range-from/who 'who start-expr)
     (values (syntax-e #'who)
             (unwrap-static-infos #'start-expr) #f
             'infinite)]
    [_
     (values #f
             #f #f
             #f)]))

(define-sequence-syntax Range.to_sequence/optimize
  (lambda () #'Range.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ range-expr/statinfo)]
       (define who 'Range.to_sequence)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (define-values (range-who start-expr end-expr type)
         (sequence-range-normalize/inline range-expr))
       (if range-who
           (range-sequence/inline #'id range-who start-expr end-expr type)
           (range-sequence/optimize #'id who range-expr))]
      [_ #f])))

(define/method (Range.to_sequence r)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-sequence-range who r)
  (cond
    [(range-from-to? r) (range-from-to->sequence r)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive->sequence r)]
    [else (range-from->sequence r)]))

(define-sequence-syntax Range.step_by/optimize
  (lambda () #'Range.step_by)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ range-expr/statinfo step-expr/statinfo)]
       (define who 'Range.step_by)
       (define range-expr (unwrap-static-infos #'range-expr/statinfo))
       (define step-who who)
       (define step-expr (unwrap-static-infos #'step-expr/statinfo))
       (define-values (range-who start-expr end-expr type)
         (sequence-range-normalize/inline range-expr))
       (if range-who
           (range-sequence/inline #'id range-who start-expr end-expr type
                                  #:step-who step-who
                                  #:step-expr step-expr)
           (range-sequence/optimize #'id who range-expr
                                    #:step-who step-who
                                    #:step-expr step-expr))]
      [_ #f])))

(define-static-info-syntax Range.step_by/optimize
  (#%call-result ((#%sequence-constructor #t)
                  (#%sequence-element #,(get-int-static-infos)))))

(define/method #:direct-id Range.step_by/optimize (Range.step_by r step)
  #:static-infos ((#%call-result ((#%sequence-constructor #t)
                                  (#%sequence-element #,(get-int-static-infos)))))
  (check-sequence-range who r)
  (check-pos-int who step)
  (cond
    [(range-from-to? r) (range-from-to->sequence r step)]
    [(range-from-to-inclusive? r) (range-from-to-inclusive->sequence r step)]
    [else (range-from->sequence r step)]))

(define-for-syntax (range-sequence/inline id range-who start-expr end-expr type
                                          #:step-who [step-who #f]
                                          #:step-expr [step-expr #f])
  (define (maybe-unwrap-fixnum stx)
    (syntax-parse stx
      #:literals (quote)
      [(quote num)
       (define datum (syntax-e #'num))
       (and (fixnum-for-every-system? datum)
            datum)]
      [_ #f]))
  (define use-unsafe? (and (maybe-unwrap-fixnum start-expr)
                           (let* ([end (and end-expr
                                            (maybe-unwrap-fixnum end-expr))]
                                  [step (and end
                                             (if step-expr
                                                 (maybe-unwrap-fixnum step-expr)
                                                 1))])
                             ;; conservatively check that the biggest
                             ;; ending integer must be fixnum
                             (and step
                                  (fixnum-for-every-system?
                                   (+ (case type
                                        [(exclusive) (sub1 end)]
                                        [(inclusive) end]
                                        [else (error "cannot happen")])
                                      step))))))
  #`[(#,id) (:do-in
             ([(start) #,start-expr]
              #,@(if end-expr
                     (list #`[(end) #,end-expr])
                     '())
              #,@(if step-expr
                     (list #`[(step) #,step-expr])
                     '()))
             (unless (variable-reference-from-unsafe? (#%variable-reference))
               (check-int '#,range-who start)
               #,@(if end-expr
                      (list #`(check-int '#,range-who end)
                            #`(check-start-end '#,range-who start end))
                      '())
               #,@(if step-expr
                      (list #`(check-pos-int '#,step-who step))
                      '()))
             ([pos start])
             #,(let ([<-stx (case type
                              [(exclusive) (if use-unsafe? #'unsafe-fx< #'<)]
                              [(inclusive) (if use-unsafe? #'unsafe-fx<= #'<=)]
                              [(infinite) #f]
                              [else (error "cannot happen")])])
                 (if <-stx #`(pos . #,<-stx . end) #'#t))
             ([(#,id) pos])
             #t
             #t
             ((#,(if use-unsafe? #'unsafe-fx+ #'+)
               pos
               #,(if step-expr #'step #''1))))])

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
    [else
     (values (range-from-start r)
             #f
             #f)]))

;; detect wrapper that hides an otherwise immediate sequence range
(define-syntax (sequence-range/not-inlinable stx)
  (syntax-parse stx
    [(_ range-expr)
     (define expanded
       (local-expand #'range-expr
                     'expression
                     (list #'range-from-to/who
                           #'range-from-to-inclusive/who
                           #'range-from/who)))
     (syntax-parse expanded
       #:literals (range-from-to/who range-from-to-inclusive/who range-from/who)
       [(~or* (range-from-to/who _ _ _)
              (range-from-to-inclusive/who _ _ _)
              (range-from/who _ _))
        (raise-syntax-error #f
                            "should not get here;\n sequence range is inlinable"
                            stx
                            #'range-expr
                            (list expanded))]
       [_
        (void)])
     expanded]))

(define-for-syntax (range-sequence/optimize id who range-expr
                                            #:step-who [step-who #f]
                                            #:step-expr [step-expr #f])
  #`[(#,id) (:do-in
             ([(start end <?) (let ([range (sequence-range/not-inlinable #,range-expr)])
                                (unless (variable-reference-from-unsafe? (#%variable-reference))
                                  (check-sequence-range '#,who range))
                                (sequence-range-normalize range))]
              #,@(if step-expr
                     (list #`[(step) (let ([step #,step-expr])
                                       (unless (variable-reference-from-unsafe? (#%variable-reference))
                                         (check-pos-int '#,step-who step))
                                       step)])
                     '()))
             (void)
             ([pos start])
             (if end (pos . <? . end) #t)
             ([(#,id) pos])
             #t
             #t
             ((+
               pos
               #,(if step-expr #'step #''1))))])

(define/method (Range.to_list r)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-list-range who r)
  (cond
    [(range-from-to? r) (range-from-to->list r)]
    [else (range-from-to-inclusive->list r)]))
