#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/operator
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         (only-in "arithmetic.rkt" .< .<= .= .>= .>)
         (submod "arithmetic.rkt" precedence)
         (submod "map.rkt" for-append)
         "append-key.rkt"
         "compare-key.rkt"
         "call-result-key.rkt"
         "static-info.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         (only-in "class-method-result.rkt" method-result)
         "is-static.rkt"
         "number.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     Comparable)
         (for-spaces (#f
                      rhombus/repet)
                     (rename-out
                      [rhombus< <]
                      [rhombus<= <=]
                      [rhombus>= >=]
                      [rhombus> >])
                     compares_equal
                     compares_unequal))

(define-values (prop:Comparable Comparable? Comparable-ref)
  (make-struct-type-property 'Comparable))

(define-annotation-syntax Comparable
  (identifier-annotation comparable? ((#%compare ((< general<)
                                                  (<= general<=)
                                                  (= general=)
                                                  (!= general!=)
                                                  (>= general>=)
                                                  (> general>))))))

(define (comparable? v)
  (or (real? v)
      (string? v)
      (bytes? v)
      (symbol? v)
      (keyword? v)
      (Comparable? v)))

(define-class-desc-syntax Comparable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&compare_to #&less #&less_or_equal #&compares_equal #&compares_unequal #&greater_or_equal #&greater)
                     #'#(#:abstract
                         Comparable.less
                         Comparable.less_or_equal
                         Comparable.compares_equal
                         Comparable.compares_unequal
                         Comparable.greater_or_equal
                         Comparable.greater)
                     (hasheq 'compare_to 0
                             'less 1
                             'less_or_equal 2
                             'compares_equal 3
                             'compares_unequal 4
                             'greater_or_equal 5
                             'greater 6)
                     (hasheq 'compare_to #'compare-to-result
                             'less #'boolean-result
                             'less_or_equal #'boolean-result
                             'compares_equal #'boolean-result
                             'compares_unequal #'boolean-result
                             'greater_or_equal #'boolean-result
                             'greater #'boolean-result)
                     '()
                     #f
                     #'()
                     '(compare veneer)
                     ;; --------------------
                     #'Comparable
                     #'Comparable
                     #'prop:Comparable
                     #'prop:Comparable
                     #'Comparable-ref
                     #t
                     #f
                     null))))

(define-syntax compare-to-result
  (method-result #'exact-integer? #t 1 "Int" (get-int-static-infos) 4))

(define-syntax boolean-result
  (method-result #'boolean? #t 1 "Boolean" #'() 4))

(define-for-syntax (parse-compare op form1 form2 self-stx form1-in
                                  static?
                                  comparable-static-info
                                  k)
  (define-values (direct-compare1/maybe-boxed direct-compare2/maybe-boxed)
    (comparable-static-info #'#%compare))
  (define checked1? (and direct-compare1/maybe-boxed
                         (box? (syntax-e direct-compare1/maybe-boxed))))
  (define checked2? (and direct-compare2/maybe-boxed
                         (box? (syntax-e direct-compare2/maybe-boxed))))
  (define checked? (or checked1? checked2?))
  (define direct-compare1 (if checked1?
                              (unbox (syntax-e direct-compare1/maybe-boxed))
                              direct-compare1/maybe-boxed))
  (define direct-compare2 (if checked2?
                              (unbox (syntax-e direct-compare2/maybe-boxed))
                              direct-compare2/maybe-boxed))
  (define (get-compare-id direct-compare)
    (cond
      [(and (syntax? direct-compare)
            (eq? '#:method (syntax-e direct-compare)))
       (values (case op
                 [(<) #'method<]
                 [(<=) #'method<=]
                 [(=) #'method=]
                 [(!=) #'method!=]
                 [(>=) #'method>=]
                 [(>) #'method>]
                 [else (error "unrecognized op" op)])
               #t)]
      [(identifier? direct-compare)
       (values direct-compare #f)]
      [else
       (define id
         (for/or ([pr (in-list (or (and direct-compare (syntax->list direct-compare)) null))])
           (syntax-parse pr
             [(c-op id)
              #:when (eq? op (syntax-e #'c-op))
              #'id]
             [_ #f])))
       (cond
         [(identifier? id)
          (values id #t)]
         [(and (syntax? id)
               (box? (syntax-e id))
               (identifier? (unbox (syntax-e id))))
          (values (unbox (syntax-e id)) #f)]
         [else (values (case op
                         [(<) #'general<]
                         [(<=) #'general<=]
                         [(=) #'general=]
                         [(!=) #'general!=]
                         [(>=) #'general>=]
                         [(>) #'general>]
                         [else (error "unrecognized op" op)])
                       #t)])]))
  (define-values (compare1-id bool1?) (get-compare-id direct-compare1))
  (define-values (compare2-id bool2?) (get-compare-id direct-compare2))
  (define-values (generic-id generic-bool2) (get-compare-id #f))
  (define-values (compare-id bool?)
    (cond
      [(and static? (not direct-compare1) (not direct-compare2))
       (raise-syntax-error #f
                           (string-append "specialization not known" statically-str)
                           self-stx
                           #f
                           (list form1-in
                                 form2))]
      [(not direct-compare1) (values compare2-id bool2?)]
      [(not direct-compare2) (values compare1-id bool1?)]
      [(free-identifier=? compare1-id compare2-id) (values compare2-id bool2?)]
      [(free-identifier=? compare1-id generic-id) (values compare2-id bool2?)]
      [(free-identifier=? compare2-id generic-id) (values compare1-id bool1?)]
      [(not static?) (get-compare-id #f)]
      [else
       (raise-syntax-error #f
                           (string-append "incompatible specializations from arguments" statically-str)
                           self-stx
                           #f
                           (list form1-in
                                 form2))]))
  (k compare-id bool?
     (not checked?)
     form1 form2))

(define (!= a b) (not (= a b)))

(define-for-syntax (build-compare compare-id op bool? direct? form1 form2 orig-stxes)
  (relocate+reraw
   (respan (datum->syntax #f orig-stxes))
   (datum->syntax (quote-syntax here)
                  (let ([e (if direct?
                               (list compare-id form1 form2)
                               `(,#'let ([a1 ,form1]
                                         [a2 ,form2])
                                        (,#'check-comparable ',(case op
                                                                 [(=) 'compares_equal]
                                                                 [(!=) 'compares_unequal]
                                                                 [else op])
                                                             ,(case op
                                                                [(<) 1]
                                                                [(<=) 2]
                                                                [(=) 3]
                                                                [(!=) 4]
                                                                [(>=) 5]
                                                                [(>) 6])
                                                             a1
                                                             a2)
                                        (,compare-id a1 a2)))])
                    (if bool?
                        e
                        `(,op ,e 0))))))

(define-for-syntax (make-comp-expression op)
  (lambda (form1-in form2 self-stx)
    (define static? (is-static-context? self-stx))
    (define form1 (rhombus-local-expand form1-in))
    (parse-compare
     op form1 form2 self-stx form1-in
     static?
     (lambda (key) (values (syntax-local-static-info form1 key)
                           (syntax-local-static-info form2 key)))
     (lambda (compare-id bool? direct? form1 form2)
       (build-compare compare-id op bool? direct? form1 form2
                      (list form1-in self-stx form2))))))

(define-for-syntax (make-comp-repetition op)
  (lambda (form1 form2 self-stx)
    (define static? (is-static-context? self-stx))
    (syntax-parse form1
      [form1-info::repetition-info
       (syntax-parse form2
         [form2-info::repetition-info
          (build-compound-repetition
           self-stx
           (list form1 form2)
           (lambda (form1 form2)
             (parse-compare
              op form1 form2 self-stx form1
              static?
              (lambda (key)
                (values (repetition-static-info-lookup #'form1-info.element-static-infos key)
                        (repetition-static-info-lookup #'form2-info.element-static-infos key)))
              (lambda (compare-id bool? direct? form1 form2)
                (values
                 (build-compare compare-id op bool? direct? form1 form2
                                (list form1 self-stx form2))
                 #'())))))])])))

(define-for-syntax (repet-comparison-precedences)
  (for/list ([p (in-list (comparison-precedences))])
    (if (identifier? (car p))
        (cons (in-repetition-space (car p)) (cdr p))
        p)))

(define-syntax-rule (define-compare-op def-op op .op)
  (begin
    (define-syntax def-op
      (expression-infix-operator
       comparison-precedences
       'automatic
       (make-comp-expression 'op)
       'left))
    (define-repetition-syntax def-op
      (repetition-infix-operator
       repet-comparison-precedences
       'automatic
       (make-comp-repetition 'op)
       'left))))

(define-compare-op rhombus< < .<)
(define-compare-op rhombus<= <= .<=)
(define-compare-op compares_equal = .=)
(define-compare-op compares_unequal != .!=)
(define-compare-op rhombus>= >= .>=)
(define-compare-op rhombus> > .>)

;; checking for the same `compare` method relies on the fact that `class`
;; will generate a new procedure each time that `compare` is overridden
(define (same-compare? a b)
  (eq? a b))

(define compare-who/method 'Comparable.compare_to)

(define (raise-mismatch what op v1 v2
                        #:method [method 'compare_to]
                        #:both-compare? [both-compare? #f])
  (define other-what (if both-compare?
                         (string-append "other " what)
                         "other value"))
  (raise-arguments-error*
   op rhombus-realm
   (string-append "cannot compare "
                  (case (string-ref what 0)
                    [(#\a #\i) "an "]
                    [else "a "])
                  what " and " other-what
                  (if both-compare?
                      (string-append ";\n two "
                                     what
                                     "s must share the same `"
                                     (symbol->string method)
                                     "` implementation")
                      ""))
   what v1
   other-what v2))

(define-syntax-rule (define-general general-op method-op
                      op wrap vtable-index
                      num-op
                      char-op
                      string-op
                      bytes-op
                      symbol-op
                      keyword-op)
  (begin
    (define (general-op v1 v2)
      (cond
        [(number? v1)
         (unless (number? v2)
           (raise-mismatch "number" 'op v1 v2))
         (wrap (num-op v1 v2))]
        [(char? v1)
         (unless (char? v2)
           (raise-mismatch "character" 'op v1 v2))
         (wrap (char-op v1 v2))]
        [(string? v1)
         (unless (string? v2)
           (raise-mismatch "string" 'op v1 v2))
         (wrap (string-op v1 v2))]
        [(bytes? v1)
         (unless (bytes? v2)
           (raise-mismatch "byte string" 'op v1 v2))
         (wrap (bytes-op v1 v2))]
        [(symbol? v1)
         (unless (symbol? v2)
           (raise-mismatch "symbol" 'op v1 v2))
         (wrap (symbol-op v1 v2))]
        [(keyword? v1)
         (unless (keyword? v2)
           (raise-mismatch "keyword" 'op v1 v2))
         (wrap (keyword-op v1 v2))]
        [else (method-op v1 v2)]))
    (define (method-op v1 v2)
      (define vt1 (Comparable-ref v1 #f))
      (unless vt1
        (raise-argument-error* compare-who/method rhombus-realm "Comparable" v1))
      (define app1 (vector-ref vt1 vtable-index))
      (define vt2 (Comparable-ref v2 #f))
      (unless (and vt2
                   (same-compare? (vector-ref vt1 0) (vector-ref vt2 0))
                   (same-compare? (vector-ref vt1 vtable-index) (vector-ref vt2 vtable-index)))
        (raise-mismatch "comparable object" 'op v1 v2
                        #:method (if (same-compare? (vector-ref vt1 0) (vector-ref vt2 0))
                                     'op
                                     'compare_to)
                        #:both-compare? (and vt2 #t)))
      (app1 v1 v2))))

(define-general general< method<
  < values 1
  <
  char<?
  string<?
  bytes<?
  symbol<?
  keyword<?)

(define-general general<= method<=
  <= values 2
  <=
  char<=?
  string<=?
  (lambda (a b) (not (bytes>? a b)))
  (lambda (a b) (or (eq? a b) (symbol<? a b)))
  (lambda (a b) (or (eq? a b) (keyword<? a b))))

(define-general general= method=
  compares_equal values 3
  =
  char=?
  string=?
  bytes=?
  eq?
  eq?)

(define-general general!= method!=
  compares_unequal not 4
  =
  char=?
  string=?
  bytes=?
  eq?
  eq?)

(define-general general>= method>=
  >= values 5
  >=
  char>=?
  string>=?
  (lambda (a b) (not (bytes<? a b)))
  (lambda (a b) (or (eq? a b) (symbol<? b a)))
  (lambda (a b) (or (eq? a b) (keyword<? b a))))

(define-general general> method>
  >= values 6
  >
  char>?
  string>?
  bytes>?
  (lambda (a b) (symbol<? b a))
  (lambda (a b) (keyword<? b a)))

(define (Comparable.less a b)
  (< ((vector-ref (Comparable-ref a #f) 0) a b) 0))
(define (Comparable.less_or_equal a b)
  (<= ((vector-ref (Comparable-ref a #f) 0) a b) 0))
(define (Comparable.compares_equal a b)
  (= ((vector-ref (Comparable-ref a #f) 0) a b) 0))
(define (Comparable.compares_unequal a b)
  (not (= ((vector-ref (Comparable-ref a #f) 0) a b) 0)))
(define (Comparable.greater_or_equal a b)
  (>= ((vector-ref (Comparable-ref a #f) 0) a b) 0))
(define (Comparable.greater a b)
  (> ((vector-ref (Comparable-ref a #f) 0) a b) 0))

(define (check-comparable op vtable-index a1 a2)
  (define vt1 (Comparable-ref a1 #f))
  (unless vt1
    (raise-arguments-error*
     op rhombus-realm
     "checked `compare` must be applied to an comparable object"
     "value" a1))
  (define vt2 (Comparable-ref a2 #f))
  (unless (and vt2
               (same-compare? (vector-ref vt1 0) (vector-ref vt2 0))
               (same-compare? (vector-ref vt1 vtable-index) (vector-ref vt2 vtable-index)))
    (raise-mismatch "comparable object" op a1 a2
                    #:method (if (same-compare? (vector-ref vt1 0) (vector-ref vt2 0))
                                 op
                                 'compare_to)
                    #:both-compare? (and vt2 #t))))
