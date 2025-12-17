#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/flonum
         "name-root.rkt"
         "static-info.rkt"
         "realm.rkt"
         "annotation-failure.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "function-arity-key.rkt"
         "call-result-key.rkt"
         (submod "arithmetic.rkt" static-infos)
         "flonum-key.rkt"
         (submod "function-parse.rkt" for-call)
         (submod "function-parse.rkt" for-non-special)
         "is-static.rkt"
         "parse.rkt"
         "expression.rkt"
         "repetition.rkt"
         "parens.rkt"
         (only-in "implicit.rkt" #%call)
         "rhombus-primitive.rkt"
         "reducer.rkt")

(provide (for-space rhombus/namespace
                    math))

(define-name-root math
  #:fields
  (pi
   [abs abs/fl]
   [min min/fl]
   [max max/fl]
   [floor floor/fl]
   [ceiling ceiling/fl]
   [round round/fl]
   [truncate truncate/fl]
   sqrt
   log
   exp
   [expt expt/fl]
   [cos cos/fl]
   [sin sin/fl]
   [tan tan/fl]
   acos asin atan
   [random rhombus-random]
   numerator
   denominator
   gcd lcm
   magnitude angle
   [real_part real-part]
   [imag_part imag-part]
   [exact inexact->exact]
   [inexact exact->inexact]
   equal
   less less_or_equal
   greater greater_or_equal
   sum product))

(define pi (atan 0 -1))

(define-syntax (define-fl stx)
  (syntax-parse stx
    [(_ (id arg ...) op flop)
     #`(define-syntaxes (id #,(in-repetition-space #'id))
         (let ([m
                (lambda (stx repet?)
                  (syntax-parse stx
                    [(head (~and args (p-tag::parens arg ...)) . tail)
                     #:when (free-identifier=? #'#%call (datum->syntax #'p-tag '#%call))
                     #:with (_ _::non-special (... ...)) #'args
                     (subst-op #'head #'args #'flop #'op repet? #'tail)]
                    [(head . tail)
                     (values (subst-op-head #'head #'op repet?)
                             #'tail)]))])
           (values (expression-transformer
                    (lambda (stx) (m stx #f)))
                   (repetition-transformer
                    (lambda (stx) (m stx #t))))))]))

(define-for-syntax (subst-op head args-in flop op repet? tail)
  (define-values (proc new-tail to-anon-function?)
    (cond
      [(not repet?)
       (define-values (tag args)
         (syntax-parse args-in
           [(tag e::expression ...)
            (values #'tag (syntax->list #'(e.parsed ...)))]))
       (define flonum? (andmap flonum-statinfo? args))
       (parse-function-call (if flonum?
                                flop
                                op)
                            null
                            (list* head
                                   (cons tag (map (lambda (e) #`(group (parsed #:rhombus/expr #,e))) args))
                                   tail)
                            #:static? (is-static-context? tag)
                            #:result-static-infos (and flonum?
                                                       (get-flonum-static-infos)))]
      [else
       (define-values (tag args)
         (syntax-parse args-in
           [(tag r::repetition ...)
            (values #'tag (syntax->list #'(r.parsed ...)))]))
       (define flonum? (for/and ([arg (in-list args)])
                         (syntax-parse arg
                           [r::repetition-info
                            (define v (static-info-lookup #'r.element-static-infos #'#%flonum))
                            (and v (syntax-e v))])))
       (parse-function-call (identifier-repetition-use
                             (if flonum?
                                 flop
                                 op))
                            null
                            (list* head
                                   (cons tag (map (lambda (r) #`(group (parsed #:rhombus/repet #,r))) args))
                                   tail)
                            #:repetition? #t
                            #:static? (is-static-context? tag)
                            #:result-static-infos (and flonum?
                                                       (get-flonum-static-infos)))]))
  (values proc new-tail))

(define-for-syntax (subst-op-head head op repet?)
  (datum->syntax op (syntax-e op) head head))

(define-fl (abs/fl v) abs flabs)
(define-fl (min/fl a b ...) min flmin)
(define-fl (max/fl a b ...) max flmax)
(define-fl (floor/fl v) floor flfloor)
(define-fl (ceiling/fl v) ceiling flceiling)
(define-fl (round/fl v) round flround)
(define-fl (truncate/fl v) truncate fltruncate)
(define-fl (expt/fl v a) expt flexpt)
(define-fl (cos/fl v) cos flcos)
(define-fl (sin/fl v) sin flsin)
(define-fl (tan/fl v) tan fltan)

(define-static-info-syntaxes (abs
                              floor ceiling round truncate
                              sqrt
                              exp
                              cos sin tan
                              acos asin
                              numerator
                              denominator
                              magnitude angle
                              real-part imag-part
                              inexact->exact exact->inexact)
  (#%call-result #,(get-number-static-infos))
  (#%function-arity 2)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntaxes (expt)
  (#%call-result #,(get-number-static-infos))
  (#%function-arity 4)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntaxes (min max)
  (#%call-result #,(get-number-static-infos))
  (#%function-arity -2)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntaxes (log atan)
  (#%call-result #,(get-number-static-infos))
  (#%function-arity 6)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntaxes (gcd lcm)
  (#%call-result #,(get-number-static-infos))
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

(define (check-posint who n)
  (unless (exact-positive-integer? n)
    (raise-annotation-failure who n "PosInt")))

(define (check-int who n)
  (unless (exact-integer? n)
    (raise-annotation-failure who n "Int")))

(define/arity #:name random rhombus-random
  #:static-infos ((#%call-result #,(get-number-static-infos)))
  (case-lambda
    [() (random)]
    [(n)
     (check-posint who n)
     (if (n . < . (arithmetic-shift 1 31))
         (random n)
         (let rejection-loop ()
           (define maybe-result
             (let ([m (- n 1)])
               (let loop ([r 0] [len (integer-length m)] [shift 0])
                 (if (len . < . 32)
                     (+ r (arithmetic-shift (random (add1 (arithmetic-shift m (- shift)))) shift))
                     (loop (+ r (arithmetic-shift (random #x80000000) shift))
                           (- len 31)
                           (+ shift 31))))))
           (if (maybe-result . < . n)
               maybe-result
               (rejection-loop))))]
    [(start end)
     (check-int who start)
     (check-int who end)
     (unless (start . < . end)
       (raise-arguments-error* who rhombus-realm
                               "start index is not less than end index"
                               "start index" start
                               "end index" end))
     (+ (rhombus-random (- end start)) start)]))

(define-syntax (define-nary stx)
  (syntax-parse stx
    [(_ ok? ok-str 0-value
        #:static-infos static-infos
        [name op] ...)
     #'(begin
         (define/arity name
           #:static-infos static-infos
           (case-lambda
             [() (0-value op)]
             [(a)
              (unless (ok? a) (raise-annotation-failure who a ok-str))
              (op a)]
             [(a b)
              (unless (ok? a) (raise-annotation-failure who a ok-str))
              (unless (ok? b) (raise-annotation-failure who b ok-str))
              (op a b)]
             [ns
              (for ([a (in-list ns)])
                (unless (ok? a) (raise-annotation-failure who a ok-str)))
              (apply op ns)]))
         ...)]))

(define-nary
  number? "Number" (lambda (op) #t)
  #:static-infos ()
  [equal =])

(define-nary
  real? "Real" (lambda (op) #t)
  #:static-infos ()
  [less <]
  [less_or_equal <=]
  [greater >]
  [greater_or_equal >=])

(define-nary
  number? "Number" (lambda (op) (op))
  #:static-infos ((#%call-result #,(get-number-static-infos)))
  [sum +]
  [product *])

(define-for-syntax (make-number-reducer init op)
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(self . tail)
        (values (reducer
                 #'build-result
                 #`([result #,init])
                 #f
                 #'build-accum
                 #f
                 #f
                 #'build-accum-result
                 (get-number-static-infos)
                 #`(result #,op self next-result))
                #'tail)]))))

(define-reducer-syntax sum
  (make-number-reducer #'0 #'+))

(define-reducer-syntax product
  (make-number-reducer #'1 #'*))

(define-syntax (build-result stx)
  (syntax-parse stx
    [(_ _ e) #'e]))

(define-syntax (build-accum stx)
  (syntax-parse stx
    [(_ (result op self next-result) e)
     #'(define next-result
         (let ([v e])
           (if (number? v)
               (op result v)
               (raise-number-error 'self v))))]))

(define-syntax (build-accum-result stx)
  (syntax-parse stx
    [(_ (_ _ _ next-result)) #'next-result]))

(define (raise-number-error who v)
  (raise-annotation-failure who v "Number"))

(void (set-primitive-who! 'flabs 'abs))
(void (set-primitive-who! 'flmin 'min))
(void (set-primitive-who! 'flmax 'max))
(void (set-primitive-who! 'flfloor 'floor))
(void (set-primitive-who! 'flceiling 'ceiling))
(void (set-primitive-who! 'flround 'round))
(void (set-primitive-who! 'fltruncate 'truncate))
(void (set-primitive-who! 'flsin 'sin))
(void (set-primitive-who! 'flcos 'cos))
(void (set-primitive-who! 'fltan 'tan))

(void (set-primitive-who! 'fl< '.<))
(void (set-primitive-who! 'fl<= '.<=))
(void (set-primitive-who! 'fl= '.=))
(void (set-primitive-who! 'fl> '.>))
(void (set-primitive-who! 'fl>= '.>=))

(void (set-primitive-who! 'fx< '.<))
(void (set-primitive-who! 'fx<= '.<=))
(void (set-primitive-who! 'fx= '.=))
(void (set-primitive-who! 'fx> '.>))
(void (set-primitive-who! 'fx>= '.>=))

;; defined in `rhombus/flonum`, but renamed here
(void (set-primitive-who! 'flsqrt 'sqrt))
(void (set-primitive-who! 'fllog 'log))
(void (set-primitive-who! 'flexp 'exp))
(void (set-primitive-who! 'flasin 'asin))
(void (set-primitive-who! 'flacos 'acos))
(void (set-primitive-who! 'flatan 'atan))
(void (set-primitive-who! 'flsingle 'to_single))
(void (set-primitive-who! 'flbit-field 'bit_field))
(void (set-primitive-who! '->fl 'from_real))
(void (set-primitive-who! 'fl->exact-integer 'to_int))

;; defined in `rhombus/fixnum`, but renamed here
(void (set-primitive-who! 'fx+ '+))
(void (set-primitive-who! 'fx- '-))
(void (set-primitive-who! 'fx* '*))
(void (set-primitive-who! 'fxdiv 'div))
(void (set-primitive-who! 'fxmod 'mod))
(void (set-primitive-who! 'fxrem 'rem))
(void (set-primitive-who! 'fxabs 'abs))
(void (set-primitive-who! 'fxmin 'min))
(void (set-primitive-who! 'fxmax 'max))
(void (set-primitive-who! 'fxlshift '<<))
(void (set-primitive-who! 'fxrshift '>>))
(void (set-primitive-who! 'fxand 'and))
(void (set-primitive-who! 'fxior 'or))
(void (set-primitive-who! 'fxxor 'xor))
(void (set-primitive-who! 'fxnot 'not))
(void (set-primitive-who! 'fxpopcount 'popcount))
(void (set-primitive-who! 'fxpopcount16 'popcount16))
(void (set-primitive-who! 'fxpopcount32 'popcount32))
(void (set-primitive-who! 'fx+/wraparound '|wraparound.(+)|))
(void (set-primitive-who! 'fx-/wraparound '|wraparound.(-)|))
(void (set-primitive-who! 'fx*/wraparound '|wraparound.(*)|))
(void (set-primitive-who! 'fxlshift/wraparound '|wraparound.bits.(<<)|))
(void (set-primitive-who! 'fx->fl 'from_flonum))
(void (set-primitive-who! 'fl->fx 'to_flonum))

(void (set-primitive-contract! '16-bit-fixnum? "Fixnum && Int.in(0 ..= 65535)"))
(void (set-primitive-contract! '32-bit-fixnum? "Fixnum && Int.in(0 ..= 4294967295)"))
