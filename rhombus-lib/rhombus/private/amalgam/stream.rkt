#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annot-context.rkt")
         (only-in racket/private/for
                  stream?
                  stream-first
                  stream-rest
                  stream-empty?
                  empty-stream
                  in-stream)
         (only-in racket/private/stream-cons
                  stream-cons)
         "provide.rkt"
         "class-primitive.rkt"
         "dot-parse.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "index-key.rkt"
         "sequence-constructor-key.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt"
         "define-arity.rkt"
         "realm.rkt"
         "rhombus-primitive.rkt"
         "static-info.rkt"
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         "binding.rkt"
         "composite.rkt"
         "literal.rkt"
         "parse.rkt"
         "parens.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_]))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Stream))

(module+ for-builtin
  (provide stream-method-table))

(module+ for-static-info
  (provide (for-syntax get-stream-static-infos)))

(module+ for-index
  (provide Stream.get))

(define-primitive-class Stream stream
  #:lift-declaration
  #:instance-static-info ((#%sequence-constructor Stream.to_sequence/optimize)
                          (#%index-get Stream.get))
  #:existing
  #:transparent
  #:fields
  ()
  #:namespace-fields
  ([expect_of Stream.expect_of]
   [empty Stream.empty]
   [cons Stream.cons])
  #:properties
  ([first Stream.first extract-result-statinfo]
   [rest Stream.rest extract-rest-statinfo])
  #:methods
  (is_empty
   get))

(define-for-syntax (extract-result-statinfo lhs-si)
  (or (static-info-lookup lhs-si #'#%sequence-element)
      (extract-index-uniform-result
       (static-info-lookup lhs-si #'#%index-result))
      #'()))

(define-for-syntax (extract-rest-statinfo arg-si
                                          #:static-infos [statinfos (get-stream-static-infos)])
  (define r-si (extract-result-statinfo arg-si))
  (if (not (static-infos-empty? r-si))
      #`((#%sequence-element #,r-si)
         #,@statinfos)
      statinfos))

(define-syntax (stream-of-static-infos data static-infoss)
  #`((#%sequence-element #,(if (= (length static-infoss) 1)
                               (car static-infoss)
                               #`((#%values #,static-infoss))))))

(define-syntax (stream-build-convert arg-id build-convert-stxs kws data)
  arg-id)

(define-annotation-constructor (StreamAgain Stream.expect_of)
  ()
  #'stream? #,(get-stream-static-infos)
  'any ;; allow any number of annotations in `expect_of`
  #f
  (lambda (predicate-stxs) #`(lambda (arg) #t))
  #'stream-of-static-infos #f
  #'stream-build-convert #'())

(define Stream.empty
  empty-stream)

(define-static-info-syntax Stream.empty
  #,@(get-stream-static-infos))

(define-syntax (select-elem data deps)
  (define mode (syntax-e data))
  (define args (annotation-dependencies-args deps))
  (define lst-i 0)
  (define arg-si (or (and (< lst-i (length args))
                          (list-ref args lst-i))
                     #'()))
  (case mode
    [(first) (extract-result-statinfo arg-si)]
    [(rest) (extract-rest-statinfo arg-si)]
    [(seq) (extract-rest-statinfo arg-si #:static-infos #'())]))

(define/method (Stream.first s)
  #:primitive (stream-first)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem first)))))
  (stream-first s))

(define/method (Stream.rest s)
  #:primitive (stream-rest)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem rest)))))
  (stream-rest s))

(define/method (Stream.get st i)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem first)))))
  (unless (stream? st) (raise-annotation-failure who st "Stream"))
  (unless (exact-nonnegative-integer? i)
    (raise-annotation-failure who i "Nat"))
  (let loop ([n i] [s st])
    (cond
     [(stream-empty? s)
      (raise-arguments-error* who
                              rhombus-realm
                              "stream ended before index"
                              "index" i
                              ;; Why `"stream" st` is omitted: see `racket/stream`
                              #;"stream" #;st)]
     [(zero? n)
      (stream-first s)]
     [else
      (loop (sub1 n) (stream-rest s))])))

(define/method (Stream.is_empty s)
  #:primitive (stream-empty?)
  (stream-empty? s))

(define (nonempty-stream? v)
  (and (stream? v)
       (not (stream-empty? v))))

(begin-for-syntax
  (define-syntax-class (:maybe-eager-expression stx)
    #:attributes (parsed [eager 1])
    #:datum-literals (group)
    (pattern (group (~and #:eager kw))
             #:do [(raise-syntax-error #f
                                       "missing expression after keyword"
                                       stx
                                       #'kw)]
             #:attr parsed #'#f
             #:attr [eager 1] null)
    (pattern (group #:eager . tail)
             #:with ::expression #'(group . tail)
             #:attr [eager 1] (list #'#:eager))
    (pattern ::expression
             #:attr [eager 1] null)))

(define-syntax Stream.cons
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens (~var a (:maybe-eager-expression stx)) (~var d (:maybe-eager-expression stx))) . tail)
        (define e (wrap-static-info*
                   #'(stream-cons a.eager ... a.parsed d.eager ... d.parsed)
                   (get-stream-static-infos)))
        (define si (static-infos-or (extract-static-infos #'a.parsed)
                                    (extract-result-statinfo
                                     (extract-static-infos #'d.parsed))))
        (values (if (static-infos-empty? si)
                    e
                    (wrap-static-info e #'#%sequence-element si))
                #'tail)]))))

(define-binding-syntax Stream.cons
  (binding-transformer
   (lambda (tail)
     (define (underscore? g)
       (syntax-parse g
         #:datum-literals (group)
         [(_ us:identifier)
          (free-identifier=? (in-binding-space #'rhombus-_) (in-binding-space #'us))]
         [_ #f]))
     (composite-binding-transformer tail
                                    "Stream && !satisfying(Stream.is_empty)"
                                    #'nonempty-stream?
                                    #:static-infos (get-stream-static-infos)
                                    #:stream-element-info? #t
                                    (syntax-parse tail
                                      [(_ (_::parens g1 g2) . _)
                                       (list (if (underscore? #'g1) #'void #'stream-first)
                                             (if (underscore? #'g2) #'void #'stream-rest))]
                                      [_
                                       (list #'stream-first #'stream-rest)])
                                    (list #'() (get-stream-static-infos))))))

(define-binding-syntax Stream.empty
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'stream-empty-infoer #'()) #'tail)]))))

(define-syntax (stream-empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "Stream.empty"
                   #'empty
                   (static-infos-and (get-stream-static-infos) #'up-static-infos)
                   #'()
                   #'empty-oncer
                   #'stream-empty-matcher
                   #'()
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'datum)]))

(define-syntax (stream-empty-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (and (stream? arg-id) (stream-empty? arg-id))
           success
           fail)]))

(define-sequence-syntax Stream.to_sequence/optimize
  (lambda () #'Stream.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ stm-expr)]
       #`[(id) (in-stream #,(discard-static-infos #'stm-expr))]]
      [_ #f])))

(define/method (Stream.to_sequence stm)
  #:primitive (in-stream)
  #:static-infos ((#%call-result ((#%dependent-result (select-elem seq)))))
  (in-stream stm))

(void (set-primitive-contract! '(and/c stream? (not/c stream-empty?)) "Stream && !satisfying(Stream.is_empty)"))
(void (set-primitive-who! 'stream-cons 'Stream.cons))
