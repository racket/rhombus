#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         "../version-case.rkt"
         "treelist.rkt"
         (submod "treelist.rkt" unsafe)
         "mutable-treelist.rkt"
         "to-list.rkt"
         "provide.rkt"
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "reducer.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "sequence-constructor-key.rkt"
         "list-bounds-key.rkt"
         "maybe-key.rkt"
         "contains-key.rkt"
         "op-literal.rkt"
         "literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         "repetition.rkt"
         "compound-repetition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "simple-call.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rest-bind.rkt"
         "try-rest-bind.rkt"
         "number.rkt"
         (submod "comparable.rkt" for-builtin)
         "list-last.rkt"
         "maybe-list-tail.rkt"
         (submod "range.rkt" for-substring)
         (submod "range.rkt" for-info))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/repet
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer)
                     List
                     PairList)
         (for-spaces (rhombus/namespace
                      #f
                      rhombus/repet
                      rhombus/annot)
                     MutableList)
         (for-spaces (rhombus/namespace
                      rhombus/annot)
                     NonemptyList
                     NonemptyPairList))

(module+ for-binding
  (provide (for-syntax parse-list-binding
                       parse-list-expression
                       parse-list-repetition)))

(module+ for-builtin
  (provide treelist-method-table
           list-method-table
           mutable-treelist-method-table))

(module+ for-compound-repetition
  (provide (for-syntax get-list-static-infos
                       get-treelist-static-infos)))

(module+ for-listable
  (provide prop:Listable Listable? Listable-ref
           treelist? listable?
           to-treelist to-list
           (for-syntax get-treelist-static-infos)))

(define-for-syntax (extract-result-statinfo lhs-si)
  (or (static-info-lookup lhs-si #'#%index-result)
      #'()))

(define-for-syntax (add-result-statinfo lhs-si base-si)
  (define maybe-index-result
    (static-info-lookup lhs-si #'#%index-result))
  (if maybe-index-result
      #`((#%index-result #,maybe-index-result)
         #,@base-si)
      base-si))

(define-primitive-class List treelist
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get List.get)
                          (#%contains List.contains)
                          (#%append List.append)
                          (#%sequence-constructor List.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons List.cons]
   [empty empty-treelist]
   [iota List.iota]
   [repet List.repet]
   [of List.of]
   [later_of List.later_of])
  #:properties
  ([first List.first extract-result-statinfo]
   [last List.last extract-result-statinfo]
   [rest List.rest
         (lambda (lhs-si)
           (add-result-statinfo lhs-si (get-treelist-static-infos)))])
  #:methods
  (length
   get
   set
   add
   insert
   delete
   reverse
   append
   take
   take_last
   drop
   drop_last
   sublist
   contains
   index
   find
   find_index
   remove
   map
   for_each
   filter
   partition
   sort
   to_list
   to_sequence
   copy))

(define-primitive-class PairList list
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get PairList.get)
                          (#%contains PairList.contains)
                          (#%append PairList.append)
                          (#%sequence-constructor PairList.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons PairList.cons]
   [empty null]
   [iota PairList.iota]
   [repet PairList.repet]
   [of PairList.of])
  #:properties
  ([first PairList.first extract-result-statinfo]
   [last PairList.last extract-result-statinfo]
   [rest PairList.rest
         (lambda (lhs-si)
           (add-result-statinfo lhs-si (get-list-static-infos)))])
  #:methods
  (length
   get
   reverse
   append
   take
   take_last
   drop
   drop_last
   contains
   index
   find
   find_index
   remove
   map
   for_each
   filter
   partition
   sort
   to_list
   to_sequence))

(define-primitive-class MutableList mutable-treelist
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get MutableList.get)
                          (#%index-set MutableList.set)
                          (#%contains MutableList.contains)
                          (#%sequence-constructor MutableList.to_sequence/optimize))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons MutableList.cons]
   [now_of MutableList.now_of]
   [later_of MutableList.later_of])
  #:properties
  ()
  #:methods
  (length
   get
   set
   add
   insert
   delete
   reverse
   append
   take
   take_last
   drop
   drop_last
   sublist
   contains
   index
   find
   find_index
   remove
   map
   for_each
   filter
   sort
   to_list
   snapshot
   to_sequence
   copy))

(define-name-root NonemptyList
  #:fields
  ([of NonemptyList.of]))

(define-name-root NonemptyPairList
  #:fields
  ([of NonemptyPairList.of]))

(define-binding-syntax List.cons
  (binding-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id (tag::parens elem lst) . new-tail)
        (composite-binding-transformer #'(form-id (tag elem) . new-tail)
                                       #:rest-arg #`(#,group-tag rest-bind #,(get-treelist-static-infos)
                                                     #:annot-prefix? #f
                                                     lst)
                                       "List.cons" #'nonempty-treelist? (list #'(lambda (l) (treelist-ref l 0))) (list #'())
                                       #:static-infos (get-treelist-static-infos)
                                       #:index-result-info? #t
                                       #:rest-accessor #'(lambda (l) (treelist-drop l 1))
                                       #:rest-to-repetition #'in-treelist
                                       #:rest-repetition? #f)]))))

(define-binding-syntax PairList.cons
  (binding-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id (tag::parens elem lst) . new-tail)
        (composite-binding-transformer #'(form-id (tag elem) . new-tail)
                                       #:rest-arg #`(#,group-tag rest-bind #,(get-list-static-infos)
                                                     #:annot-prefix? #f
                                                     lst)
                                       "PairList.cons" #'nonempty-list? (list #'car) (list #'())
                                       #:static-infos (get-list-static-infos)
                                       #:index-result-info? #t
                                       #:rest-accessor #'cdr
                                       #:rest-to-repetition #'in-list
                                       #:rest-repetition? #f)]))))

(define (check-treelist who l)
  (unless (treelist? l)
    (raise-annotation-failure who l "List")))

(define (check-list who l)
  (unless (list? l)
    (raise-annotation-failure who l "PairList")))

(define (check-mutable-treelist who l)
  (unless (mutable-treelist? l)
    (raise-annotation-failure who l "MutableList")))

(define/arity (List.cons a d)
  #:primitive (treelist-cons)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-cons d a))

(define/arity (PairList.cons a d)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who d)
  (cons a d))

(define/method (List.add d a)
  #:primitive (treelist-add)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-add d a))

(define/method (List.insert d pos a)
  #:primitive (treelist-insert)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-insert d pos a))

(define/method (List.delete d pos)
  #:primitive (treelist-delete)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-delete d pos))

(define/arity (MutableList.cons a d)
  #:primitive (mutable-treelist-cons!)
  (mutable-treelist-cons! d a))

(define/method (MutableList.add d a)
  #:primitive (mutable-treelist-add!)
  (mutable-treelist-add! d a))

(define/method (MutableList.insert d pos a)
  #:primitive (mutable-treelist-insert!)
  (mutable-treelist-insert! d pos a))

(define/method (MutableList.delete d pos)
  #:primitive (mutable-treelist-delete!)
  (mutable-treelist-delete! d pos))

(define (check-nonempty-list who l)
  (unless (nonempty-list? l)
    (raise-annotation-failure who l "NonemptyPairList")))

(define (check-nonempty-treelist who l)
  (unless (nonempty-treelist? l)
    (raise-annotation-failure who l "NonemptyList")))

(define/arity (List.first l)
  #:primitive (treelist-first)
  (check-nonempty-treelist who l)
  (treelist-first l))

(define/arity (List.last l)
  #:primitive (treelist-last)
  (check-nonempty-treelist who l)
  (treelist-last l))

(define/arity (List.rest l)
  #:primitive (treelist-rest)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-nonempty-treelist who l)
  (treelist-rest l))

(define/arity (PairList.first l)
  (check-nonempty-list who l)
  (car l))

(define/arity (PairList.last l)
  (check-nonempty-list who l)
  (list-last l))

(define/arity (PairList.rest l)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-nonempty-list who l)
  (cdr l))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-annotation-failure who n "NonnegInt")))

(define/arity (List.iota n)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-nonneg-int who n)
  (for/treelist ([i (in-range n)])
    i))

(define/arity (PairList.iota n)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-nonneg-int who n)
  (for/list ([i (in-range n)])
    i))

(define/method (List.length l)
  #:primitive (treelist-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (treelist-length l))

(define/method (PairList.length l)
  #:primitive (length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (length l))

(define/method (MutableList.length l)
  #:primitive (mutable-treelist-length)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (mutable-treelist-length l))

(define/method (List.reverse l)
  #:primitive (treelist-reverse)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-reverse l))

(define/method (PairList.reverse l)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who l)
  (reverse l))

(define/method (MutableList.reverse l)
  #:primitive (mutable-treelist-reverse!)
  (mutable-treelist-reverse! l))

;; used to define `List` and `PairList` further below:
(define-for-syntax (make-constructor proc-stx build-form get-static-infos wrap-static-infos
                                     #:repetition? [repetition? #f]
                                     #:rep-for-form rep-for-form
                                     #:rep-solo-for-form [rep-solo-for-form rep-for-form])
  ;; special cases optimize for `...` and `&`; letting it expand
  ;; instead to `(apply list ....)` is not so bad, but we can
  ;; avoid a `list?` check in `apply`, and we can expose more static
  ;; information this way
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (group)
      [(form-id (tag::parens _ ... _ (group _::...-expr)) . tail)
       #:when (if repetition?
                  (normal-call-repetition? #'tag)
                  (normal-call? #'tag))
       (parse-*list-form stx build-form (get-static-infos) wrap-static-infos
                         #:rep-for-form rep-for-form
                         #:rep-solo-for-form rep-solo-for-form
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(form-id (tag::parens _ ... (group _::&-expr _ ...)) . tail)
       #:when (if repetition?
                  (normal-call-repetition? #'tag)
                  (normal-call? #'tag))
       (parse-*list-form stx build-form (get-static-infos) wrap-static-infos
                         #:rep-for-form rep-for-form
                         #:rep-solo-for-form rep-solo-for-form
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(form-id (tag::brackets _ ...) . tail)
       (parse-*list-form stx build-form (get-static-infos) wrap-static-infos
                         #:rep-for-form rep-for-form
                         #:rep-solo-for-form rep-solo-for-form
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(form-id . tail)
       (values (if repetition?
                   (identifier-repetition-use proc-stx)
                   (relocate-id #'form-id proc-stx))
               #'tail)])))

(define-for-syntax (make-treelist-rest-selector args-n post-args-n)
  (if (= 0 args-n)
      (if (= 0 post-args-n)
          #'values
          #`(lambda (v) (treelist-drop-right v '#,post-args-n)))
      (if (= 0 post-args-n)
          #`(lambda (v) (treelist-drop v '#,args-n))
          #`(lambda (v) (treelist-drop-right (treelist-drop v '#,args-n) '#,post-args-n)))))

(define-for-syntax (make-list-rest-selector args-n zero-post-args-n)
  (if (= 0 args-n)
      #'values
      #'cdr))

(define-for-syntax (make-binding generate-binding make-rest-selector get-static-infos
                                 mid-splice-allowed? rest-to-repetition bounds-key)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (_::parens arg ...) . tail)
        (parse-*list-binding stx generate-binding make-rest-selector (get-static-infos)
                             mid-splice-allowed? rest-to-repetition bounds-key)]
       [(form-id (_::brackets arg ...) . tail)
        (parse-*list-binding stx generate-binding make-rest-selector (get-static-infos)
                             mid-splice-allowed? rest-to-repetition bounds-key)]))))

(define-for-syntax (parse-list-binding stx)
  (parse-*list-binding stx generate-treelist-binding make-treelist-rest-selector (get-treelist-static-infos)
                       #t #'in-treelist #'#%treelist-bounds))

(define-for-syntax (make-list-annotation-make-predicate in-form-stx)
  (lambda (predicate-stxs)
    #`(let ([pred #,(car predicate-stxs)])
        (lambda (arg)
          (for/and ([e (#,in-form-stx arg)])
            (pred e))))))

(define-annotation-constructor (List List.of)
  ()
  #'treelist? #,(get-treelist-static-infos)
  1
  #f
  (make-list-annotation-make-predicate #'in-treelist)
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'treelist-build-convert #'())

(define-annotation-constructor (List/again List.later_of)
  ()
  #'treelist? #,(get-treelist-static-infos)
  1
  #f
  (lambda (predicate-stxes annot-strs)
    (define (make-reelementer what)
      #`(lambda (lst idx v state)
          (unless (pred v)
            (raise-reelementer-error 'List '#,what idx v '#,(car annot-strs)))
          (values v state)))
    #`(let ([pred #,(car predicate-stxes)])
        (lambda (lst)       
          (chaperone-treelist lst
                              #:state #f
                              #:ref (lambda (lst idx v state)
                                      (unless (pred v)
                                        (raise-reelementer-error 'List "current" idx v '#,(car annot-strs)))
                                      v)
                              #:set #,(make-reelementer "new")
                              #:insert #,(make-reelementer "new")
                              #:append (lambda (lst lst2 state)
                                         (values (check-elements 'List #f pred lst2 '#,(car annot-strs))
                                                 state))
                              #:prepend (lambda (lst2 lst state)
                                          (values (check-elements 'List #f pred lst2 '#,(car annot-strs))
                                                  state))
                              #:delete (lambda (lst idx state) state)
                              #:take (lambda (lst n state) state)
                              #:drop (lambda (lst n state) state)))))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  "converter annotation not supported for elements;\n checking needs a predicate annotation for the list content"
  #'()
  #:parse-of parse-annotation-of/chaperone)

(define-annotation-constructor (PairList PairList.of)
  ()
  #'list? #,(get-list-static-infos)
  1
  #f
  (make-list-annotation-make-predicate #'in-list)
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'list-build-convert #'())

(define-annotation-constructor (MutableList MutableList.now_of)
  ()
  #'mutable-treelist? #,(get-mutable-treelist-static-infos)
  1
  #f
  (make-list-annotation-make-predicate #'in-mutable-treelist)
  (lambda (static-infoss)
    ;; no static info, since mutable and content is checked only initially
    #'())
  "converter annotation not supported for elements;\n immediate checking needs a predicate annotation for the mutable list content"
  #'())

(define-annotation-constructor (MutableList/again MutableList.later_of)
  ()
  #'mutable-treelist? #,(get-mutable-treelist-static-infos)
  1
  #f
  (lambda (predicate-stxes annot-strs)
    (define (make-reelementer what)
      #`(lambda (mlst idx v)
          (unless (pred v)
            (raise-reelementer-error 'MutableList '#,what idx v '#,(car annot-strs)))
          v))
    #`(let ([pred #,(car predicate-stxes)])
        (lambda (mlst)        
          (chaperone-mutable-treelist mlst
                                      #:ref #,(make-reelementer "current")
                                      #:set #,(make-reelementer "new")
                                      #:insert #,(make-reelementer "new")
                                      #:append (lambda (mlst lst)
                                                 (check-elements 'MutableList #f pred lst '#,(car annot-strs)))))))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'mutable-list-build-convert #'()
  #:parse-of parse-annotation-of/chaperone)

(define (check-elements who cvt? pred/cvt lst annot-str)
  (cond
    [cvt?
     (for/treelist ([v (in-treelist lst)])
       (pred/cvt
        v
        (lambda (v) v)
        (lambda ()
          (raise-binding-failure who "appended element" v annot-str))))]
    [else
     (for ([v (in-treelist lst)])
       (unless (pred/cvt v)
         (raise-binding-failure who "appended element" v annot-str)))
     lst]))

(define (raise-reelementer-error who what idx v annot-str)
  (raise-binding-failure who
                         (string-append what " element") v annot-str
                         "position" (unquoted-printing-string (number->string idx))))

(define-syntax (treelist-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst empty-treelist])
              ([v (in-treelist #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (treelist-add lst v))
       (lambda () #f))))

(define-syntax (list-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst '()] #:result (and lst (reverse lst)))
              ([v (in-list #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (cons v lst))
       (lambda () #f))))

(define-syntax (mutable-list-build-convert arg-id build-convert-stxs kws data)
  (with-syntax ([[(annot-str . _) _] data])
    (define (make-reelementer what)
      #`(lambda (mlst idx val)
          (cvt
           val
           (lambda (v) v)
           (lambda ()
             (raise-reelementer-error 'MutableList '#,what idx val 'annot-str)))))
    #`(let ([cvt #,(car build-convert-stxs)])
        (impersonate-mutable-treelist #,arg-id
                                      #:ref #,(make-reelementer "current")
                                      #:set #,(make-reelementer "new")
                                      #:insert #,(make-reelementer "new")
                                      #:append (lambda (mlst lst)
                                                 (check-elements 'MutableList #t cvt lst 'annot-str))))))

(define-static-info-syntax empty-treelist
  #:getter get-treelist-static-infos)

(define-static-info-syntax null
  #:getter get-list-static-infos)

(define-binding-syntax empty-treelist
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'treelist-empty-infoer #'()) #'tail)]))))

(define-binding-syntax null
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-infoer #'()) #'tail)]))))

(define-syntax (treelist-empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "List.empty"
                   #'empty
                   (static-infos-and (get-treelist-static-infos) #'up-static-infos)
                   #'()
                   #'empty-oncer
                   #'treelist-empty-matcher
                   #'()
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'datum)]))

(define-syntax (empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "PairList.empty"
                   #'empty
                   (static-infos-and (get-list-static-infos) #'up-static-infos)
                   #'()
                   #'empty-oncer
                   #'empty-matcher
                   #'()
                   #'literal-commit-nothing
                   #'literal-bind-nothing
                   #'datum)]))

(define-syntax (treelist-empty-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (treelist-empty? arg-id)
           success
           fail)]))

(define-syntax (empty-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (null? arg-id)
           success
           fail)]))

(define (nonempty-treelist? l)
  (and (treelist? l) ((treelist-length l) . > . 0)))

(define (nonempty-list? l)
  (and (pair? l) (list? l)))

(define-annotation-constructor (NonemptyList NonemptyList.of)
  ()
  #'nonempty-treelist? ((#%treelist-bounds (group 1 #f))
                        #,@(get-treelist-static-infos))
  1
  #f
  (make-list-annotation-make-predicate #'in-treelist)
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'treelist-build-convert #'())

(define-annotation-constructor (NonemptyPairList NonemptyPairList.of)
  ()
  #'nonempty-list? ((#%list-bounds (group 1 #f))
                    #,@(get-list-static-infos))
  1
  #f
  (make-list-annotation-make-predicate #'in-list)
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'list-build-convert #'())

(define-reducer-syntax List
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer/no-break #'build-from-root
                                  #'([root unsafe-empty-root]
                                     [size 0]
                                     [height 0])
                                  #'build-treelist-accum
                                  (get-treelist-static-infos)
                                  #'(root size height))
                #'tail)]))))

(define-syntax (build-treelist-accum stx)
  (syntax-parse stx
    [(_ (root size height) e) #'(unsafe-root-add root size height e)]))

(define-syntax (build-from-root stx)
  (syntax-parse stx
    [(_ _ e) #'(let-values ([(root size height) e])
                 (if (eqv? size 0)
                     empty-treelist
                     (unsafe-treelist root size height)))]))

(define-reducer-syntax PairList
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer/no-break #'build-reverse
                                  #'([accum null])
                                  #'build-accum
                                  (get-list-static-infos)
                                  #'accum)
                #'tail)]))))

(define-syntax (build-reverse stx)
  (syntax-parse stx
    [(_ accum e) #'(reverse e)]))

(define-syntax (build-identity stx)
  (syntax-parse stx
    [(_ accum e) #'e]))

(define-syntax (build-accum stx)
  (syntax-parse stx
    [(_ accum e) #'(cons e accum)]))

(define-for-syntax (make-repet g-to-for-clause-stx)
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and args (_::parens g)) . tail)
        (values (make-repetition-info #'(form-id args)
                                      #`(([(repet) #,(g-to-for-clause-stx #'g)]))
                                      #'repet
                                      #'()
                                      0)
                #'tail)]))))

(define-repetition-syntax List.repet
  (make-repet (lambda (g)
                #`(in-treelist (let ([l (rhombus-expression #,g)])
                                 (unless (variable-reference-from-unsafe? (#%variable-reference))
                                   (check-treelist 'List.repet l))
                                 l)))))

(define-repetition-syntax PairList.repet
  (make-repet (lambda (g)
                #`(in-list (let ([l (rhombus-expression #,g)])
                             (unless (variable-reference-from-unsafe? (#%variable-reference))
                               (check-list 'PairList.repet l))
                             l)))))

(define (check-function-of-arity n who proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc n))
    (raise-annotation-failure who
                              proc
                              (string-append "Function.of_arity("
                                             (number->string n)
                                             ")"))))

(define/method (List.map lst proc)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-treelist who lst)
  (check-function-of-arity 1 who proc)
  (for/treelist ([e (in-treelist lst)])
    (proc e)))

(define/method (List.for_each lst proc)
  (check-treelist who lst)
  (check-function-of-arity 1 who proc)
  (for ([e (in-treelist lst)])
    (proc e)))

(define/method (List.filter lst
                            #:keep [keep (lambda (x) #t)]
                            #:skip [skip (lambda (x) #f)])
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-treelist who lst)
  (check-function-of-arity 1 who keep)
  (check-function-of-arity 1 who skip)
  (for/treelist ([e (in-treelist lst)]
                 #:when (keep e)
                 #:unless (skip e))
    e))

(define/method (List.partition lst pred)
  #:static-infos ((#%call-result ((#%values (#,(get-treelist-static-infos)
                                             #,(get-treelist-static-infos))))))
  (check-treelist who lst)
  (check-function-of-arity 1 who pred)
  (for/fold ([a empty-treelist] [b empty-treelist]) ([e (in-treelist lst)])
    (if (pred e)
        (values (treelist-add a e) b)
        (values a (treelist-add b e)))))

(define/method (PairList.map lst proc)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (map proc lst))

(define/method (PairList.for_each lst proc)
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (for-each proc lst))

(define/method (PairList.filter lst
                                #:keep [keep (lambda (x) #t)]
                                #:skip [skip (lambda (x) #f)])
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who lst)
  (check-function-of-arity 1 who keep)
  (check-function-of-arity 1 who skip)
  (for/list ([e (in-list lst)]
             #:when (keep e)
             #:unless (skip e))
    e))

(define/method (PairList.partition lst pred)
  #:static-infos ((#%call-result ((#%values (#,(get-list-static-infos)
                                             #,(get-list-static-infos))))))
  (check-list who lst)
  (check-function-of-arity 1 who pred)
  (for/fold ([a '()] [b '()] #:result (values (reverse a) (reverse b)))
            ([e (in-list lst)])
    (if (pred e)
        (values (cons e a) b)
        (values a (cons e b)))))

(define/method (MutableList.map lst proc)
  #:primitive (mutable-treelist-map!)
  (mutable-treelist-map! lst proc))

(define/method (MutableList.for_each lst proc)
  #:primitive (mutable-treelist-for-each)
  (mutable-treelist-for-each lst proc))

(define/method (MutableList.filter lst
                                   #:keep [keep (lambda (x) #t)]
                                   #:skip [skip (lambda (x) #f)])
  (check-mutable-treelist who lst)
  (check-function-of-arity 1 who keep)
  (check-function-of-arity 1 who skip)
  (for/fold ([delta 0]) ([e (in-treelist (mutable-treelist-snapshot lst))]
                         [i (in-naturals)])
    (cond
      [(and (keep e)
            (not (skip e)))
       delta]
      [else
       (mutable-treelist-delete! lst (- i delta))
       (add1 delta)]))
  (void))

(define/method (List.sort lst [less-than? general<])
  #:primitive (treelist-sort)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-sort lst less-than?))

(define/method (PairList.sort lst [less-than? general<])
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who lst)
  (check-function-of-arity 2 who less-than?)
  (sort lst less-than?))

(define/method (MutableList.sort lst [less-than? general<])
  #:primitive (mutable-treelist-sort!)
  (mutable-treelist-sort! lst less-than?))

(define/method (List.to_list lst)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-treelist who lst)
  lst)

(define/method (PairList.to_list lst)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-list who lst)
  (list->treelist lst))

(define/method (MutableList.to_list lst)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-mutable-treelist who lst)
  (mutable-treelist-snapshot lst))

(define/method (MutableList.snapshot lst)
  #:primitive (mutable-treelist-snapshot)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (mutable-treelist-snapshot lst))

(define-sequence-syntax PairList.to_sequence/optimize
  (lambda () #'PairList.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ lst-expr)]
       #`[(id) (in-list #,(discard-static-infos #'lst-expr))]]
      [_ #f])))

(define/method (PairList.to_sequence lst)
  #:primitive (in-list)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-list lst))

(define-sequence-syntax List.to_sequence/optimize
  (lambda () #'List.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ lst-expr)]
       #`[(id) (in-treelist #,(discard-static-infos #'lst-expr))]]
      [_ #f])))

(define/method (List.to_sequence lst)
  #:primitive (in-treelist)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-treelist lst))

(define/method (List.get l n)
  #:primitive (treelist-ref)
  (treelist-ref l n))

(define/method (List.set l n v)
  #:primitive (treelist-set)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-set l n v))

(define/method (List.copy lst)
  #:primitive (treelist-copy)
  #:static-infos ((#%call-result #,(get-mutable-treelist-static-infos)))
  (treelist-copy lst))

(define/method (MutableList.copy lst)
  #:primitive (mutable-treelist-copy)
  #:static-infos ((#%call-result #,(get-mutable-treelist-static-infos)))
  (mutable-treelist-copy lst))

(define-sequence-syntax MutableList.to_sequence/optimize
  (lambda () #'MutableList.to_sequence)
  (lambda (stx)
    (syntax-parse stx
      [[(id) (_ lst-expr)]
       #`[(id) (in-mutable-treelist #,(discard-static-infos #'lst-expr))]]
      [_ #f])))

(define/method (MutableList.to_sequence lst)
  #:primitive (in-mutable-treelist)
  #:static-infos ((#%call-result ((#%sequence-constructor #t))))
  (in-mutable-treelist lst))

(define/method (MutableList.get l n)
  #:primitive (mutable-treelist-ref)
  (mutable-treelist-ref l n))

(define/method (MutableList.set l n v)
  #:primitive (mutable-treelist-set!)
  (mutable-treelist-set! l n v))

(define/method List.append
  #:primitive (treelist-append)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (case-lambda
    [() empty-treelist]
    [(a) (treelist-append a)]
    [(a b) (treelist-append a b)]
    [(a b c) (treelist-append a b c)]
    [as (apply treelist-append as)]))

;; only check that the *last* argument is list here, since `append` checks the rest
(define/method PairList.append
  #:primitive (append)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (case-lambda
    [() null]
    [(a)
     (check-list who a)
     a]
    [(a b)
     (unless (list? b)
       (check-list who a)
       (raise-annotation-failure who b "PairList"))
     (append a b)]
    [ls
     (let ([ln (list-last ls)])
       (unless (list? ln)
         (for ([l (in-list ls)])
           (check-list who l))
         (raise-annotation-failure who ln "PairList")))
     (apply append ls)]))

(define/method (MutableList.append a b)
  #:primitive (mutable-treelist-append!)
  (mutable-treelist-append! a b))

;; primitive doesn't check for listness
(define/method (PairList.get l n)
  #:primitive (list-ref)
  (check-list who l)
  (list-ref l n))

(define/method (List.take l n)
  #:primitive (treelist-take)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-take l n))

(define/method (List.take_last l n)
  #:primitive (treelist-take-right)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-take-right l n))

(define/method (List.drop l n)
  #:primitive (treelist-drop)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-drop l n))

(define/method (List.drop_last l n)
  #:primitive (treelist-drop-right)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (treelist-drop-right l n))

(define (treelist-sublist/range who lst r)
  (check-treelist who lst)
  (define-values (start end)
    (range-canonical-start+end who "list" r lst 0 (treelist-length lst)))
  (treelist-sublist lst start end))

(define/method List.sublist
  #:primitive (treelist-sublist)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (case-lambda
    [(lst r) (treelist-sublist/range who lst r)]
    [(lst start end) (treelist-sublist lst start end)]))

(define (raise-list-count who what l len n)
  (raise-arguments-error* who rhombus-realm
                          (string-append "list is shorter than the number of elements to "
                                         what)
                          "list length" (unquoted-printing-string (number->string len))
                          (string-append "number to " what)
                          (unquoted-printing-string (number->string n))))

(define/method (PairList.take orig-l orig-n)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who orig-l)
  (check-nonneg-int who orig-n)
  (let loop ([l orig-l] [n orig-n])
    (cond
      [(zero? n) null]
      ;; too many to take
      [(null? l) (raise-list-count who "take" orig-l (- orig-n n) orig-n)]
      [else (cons (car l) (loop (cdr l) (sub1 n)))])))

(define/method (PairList.take_last l n)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who l)
  (check-nonneg-int who n)
  (define len (length l))
  (when (n . > . len)
    (raise-list-count who "take" l len n))
  (list-tail l (- len n)))

(define/method (PairList.drop orig-l orig-n)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who orig-l)
  (check-nonneg-int who orig-n)
  (let loop ([l orig-l] [n orig-n])
    (cond
      [(zero? n) l]
      ;; too many to drop
      [(null? l) (raise-list-count who "drop" orig-l (- orig-n n) orig-n)]
      [else (loop (cdr l) (sub1 n))])))

(define/method (PairList.drop_last l n)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who l)
  (check-nonneg-int who n)
  (define len (length l))
  (when (n . > . len)
    (raise-list-count who "drop" l len n))
  (for/list ([a (in-list l)]
             [i (in-range 0 (- len n))])
    a))

(define/method (MutableList.take l n)
  #:primitive (mutable-treelist-take!)
  (mutable-treelist-take! l n))

(define/method (MutableList.take_last l n)
  #:primitive (mutable-treelist-take-right!)
  (mutable-treelist-take-right! l n))

(define/method (MutableList.drop l n)
  #:primitive (mutable-treelist-drop!)
  (mutable-treelist-drop! l n))

(define/method (MutableList.drop_last l n)
  #:primitive (mutable-treelist-drop-right!)
  (mutable-treelist-drop-right! l n))

(define (mutable-treelist-sublist!/range who lst r)
  (check-mutable-treelist who lst)
  (define-values (start end)
    (range-canonical-start+end who "mutable list" r lst 0 (mutable-treelist-length lst)))
  (mutable-treelist-sublist! lst start end))

(define/method MutableList.sublist
  #:primitive (mutable-treelist-sublist!)
  (case-lambda
    [(lst r) (mutable-treelist-sublist!/range who lst r)]
    [(lst start end) (mutable-treelist-sublist! lst start end)]))

(define/method (List.remove l v)
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (check-treelist who l)
  (define len (treelist-length l))
  (let loop ([i 0])
    (cond
      [(= i len) l]
      [(equal-always? v (treelist-ref l i))
       (treelist-delete l i)]
      [else (loop (+ i 1))])))

(define/method (PairList.remove l v)
  #:static-infos ((#%call-result #,(get-list-static-infos)))
  (check-list who l)
  (remove v l equal-always?))

(define/method (MutableList.remove l v)
  (check-mutable-treelist who l)
  (for ([vi (in-mutable-treelist l)]
        [i (in-naturals)])
    #:break (cond
              [(equal-always? v vi)
               (mutable-treelist-delete! l i)
               #t]
              [else #f])
    (void)))

(define/method (List.contains l v [eql equal-always?])
  #:primitive (treelist-member?)
  (treelist-member? l v eql))

(define/method (PairList.contains l v [eql equal-always?])
  (check-list who l)
  (check-function-of-arity 2 who eql)
  (and (member v l eql) #t))

(define/method (MutableList.contains l v [eql equal-always?])
  #:primitive (mutable-treelist-member?)
  (mutable-treelist-member? l v eql))

(define/method (List.index l v [eql equal-always?])
  #:primitive (treelist-index-of)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (meta-if-version-at-least
   "8.15.0.6"
   (treelist-index-of l v eql)
   (begin
     (check-treelist who l)
     (check-function-of-arity 2 who eql)
     (for/or ([vi (in-treelist l)]
              [i (in-naturals)])
       (and (eql v vi) i)))))

(define/method (PairList.index l v [eql equal-always?])
  (check-list who l)
  (check-function-of-arity 2 who eql)
  (for/or ([vi (in-list l)]
           [i (in-naturals)])
    (and (eql v vi) i)))

(define/method (MutableList.index l v [eql equal-always?])
  (check-mutable-treelist who l)
  (check-function-of-arity 2 who eql)
  (for/or ([vi (in-mutable-treelist l)]
            [i (in-naturals)])
    (and (eql v vi) i)))

(define/method (List.find l pred)
  #:primitive (treelist-find)
  (treelist-find l pred))

(define/method (PairList.find l pred)
  (check-list who l)
  (check-function-of-arity 1 who pred)
  (findf pred l))

(define/method (MutableList.find l pred)
  #:primitive (mutable-treelist-find)
  (mutable-treelist-find l pred))

(define/method (List.find_index l pred)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (check-treelist who l)
  (check-function-of-arity 1 who pred)
  (for/or ([v (in-treelist l)]
           [i (in-naturals)])
    (and (pred v) i)))

(define/method (PairList.find_index l pred)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (check-list who l)
  (check-function-of-arity 1 who pred)
  (for/or ([v (in-list l)]
           [i (in-naturals)])
    (and (pred v) i)))

(define/method (MutableList.find_index l pred)
  #:static-infos ((#%call-result ((#%maybe #,(get-int-static-infos)))))
  (check-mutable-treelist who l)
  (check-function-of-arity 1 who pred)
  (for/or ([v (in-mutable-treelist l)]
           [i (in-naturals)])
    (and (pred v) i)))

(define-for-syntax (wrap-treelist-static-info expr)
  (wrap-static-info* expr (get-treelist-static-infos)))

(define-for-syntax (wrap-list-static-info expr)
  (wrap-static-info* expr (get-list-static-infos)))

(define-for-syntax (wrap-mutable-treelist-static-info expr)
  (wrap-static-info* expr (get-mutable-treelist-static-infos)))

;; parses a list pattern that has already been checked for use with a
;; suitable `parens` or `brackets` form
(define-for-syntax (parse-*list-binding stx generate-binding make-rest-selector static-infos
                                        mid-splice-allowed? rest-to-repetition bounds-key)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (_ arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (let loop ([args args] [accum null])
       (cond
         [(null? args)
          (generate-binding #'form-id (reverse accum) null #'tail)]
         [else
          (define arg (car args))
          (define (rest-simple?)
            (and (or mid-splice-allowed?
                     (null? (cdr args)))
                 (for/and ([arg (in-list (cdr args))])
                   (syntax-parse arg
                     [(group _::&-bind . _) #f]
                     [(group _::...-bind) #f]
                     [(group _::...-bind #:nonempty) #f]
                     [(group _::...-bind #:once) #f]
                     [_ #t]))))
          (syntax-parse arg
            [(group op::&-bind rest-arg ...)
             (cond
               [(rest-simple?)
                (generate-binding #'form-id (reverse accum) (cdr args) #'tail
                                  #`(#,group-tag rest-bind #,static-infos
                                     (#,group-tag rest-arg ...))
                                  make-rest-selector
                                  #f #f)]
               [(and (pair? (cdr args))
                     (syntax-parse (cadr args)
                       [(group _::...-bind #:nonempty) '(1 #f)]
                       [(group _::...-bind #:once) '(0 1)]
                       [(group _::...-bind) '(0 #f)]
                       [_ #f]))
                => (lambda (rep-min+max)
                     (generate-binding #'form-id (reverse accum) null #'tail
                                       #`(#,group-tag try-rest-bind #,static-infos form-id
                                          #:splice-repetition #,@rep-min+max #,mid-splice-allowed? #,rest-to-repetition #,bounds-key
                                          (#,group-tag rest-arg ...)
                                          #,@(cddr args))
                                       make-rest-selector
                                       #f #f))]
               [else
                (generate-binding #'form-id (reverse accum) null #'tail
                                  #`(#,group-tag try-rest-bind #,static-infos form-id
                                     #:splice 0 #f #,mid-splice-allowed? #,rest-to-repetition #,bounds-key
                                     (#,group-tag rest-arg ...)
                                     #,@(cdr args))
                                  make-rest-selector
                                  #f #f)])]
            [(group op::...-bind (~or (~seq)
                                      (~seq (~and nonempty #:nonempty))
                                      (~seq (~and once #:once))))
             #:when (pair? accum)
             (define rep-min (if (attribute nonempty) 1 0))
             (define rep-max (if (attribute once) 1 #f))
             (cond
               [(rest-simple?)
                (generate-binding #'form-id (reverse (cdr accum)) (cdr args) #'tail (car accum)
                                  make-rest-selector
                                  rep-min rep-max)]
               [else
                (generate-binding #'form-id (reverse (cdr accum)) null #'tail
                                  #`(#,group-tag try-rest-bind #,static-infos form-id
                                     #:repetition #,rep-min #,rep-max #,mid-splice-allowed? #,rest-to-repetition #,bounds-key
                                     #,(car accum)
                                     #,@(cdr args))
                                  make-rest-selector
                                  #f #f)])]
            [_ (loop (cdr args) (cons arg accum))])]))]))

(define-for-syntax (generate-treelist-binding form-id args after-args tail [rest-arg #f] [make-rest-selector #f]
                                              [rest-repetition/min 0] [rest-repetition/max #f])
  (define pre-len (length args))
  (define post-len (length after-args))
  (define len (+ pre-len post-len))
  (define pred #`(lambda (min-len max-len)
                   (lambda (v)
                     (and (treelist? v)
                          ;; constant propoagation and folding should pick one case:
                          (cond
                            [(not max-len)
                             (>= (treelist-length v) min-len)]
                            [(eqv? min-len max-len)
                             (= (treelist-length v) min-len)]
                            [else
                             (<= min-len (treelist-length v) max-len)])))))
  (composite-binding-transformer #`(#,form-id (parens . #,args) . #,tail)
                                 #:rest-arg rest-arg
                                 #:post-args after-args
                                 "List"
                                 pred
                                 (for/list ([i (in-range len)])
                                   (if (i . < . pre-len)
                                       #`(lambda (tl) (treelist-ref tl #,i))
                                       #`(lambda (tl) (treelist-ref tl (+ (treelist-length tl) #,(- i len))))))
                                 (for/list ([i (in-range len)])
                                   #'())
                                 #:index-result-info? #t
                                 #:rest-accessor (and make-rest-selector (make-rest-selector pre-len post-len))
                                 #:rest-repetition? (and rest-repetition/min #t)
                                 #:rest-repetition-min (or rest-repetition/min 0)
                                 #:rest-repetition-max rest-repetition/max
                                 #:rest-to-repetition #'in-treelist
                                 #:bounds-key #'#%treelist-bounds
                                 #:static-infos (get-treelist-static-infos)))

(define-for-syntax (generate-list-binding form-id args after-args tail [rest-arg #f] [make-rest-selector #f]
                                          [rest-repetition/min 0] [rest-repetition/max #f])
  (define len (length args))
  (define pred #`(lambda (min-len max-len)
                   (lambda (v)
                     (and (list? v)
                          #,(let ([check #`(maybe-list-tail v min-len)])
                              ;; constant propoagation and folding should pick one case:
                              #`(cond
                                  [(not max-len)
                                   (and #,check #t)]
                                  [(eqv? min-len max-len)
                                   (null? #,check)]
                                  [else
                                   (let ([tail #,check])
                                     (and tail (maybe-list-tail tail (- max-len min-len)) #t))]))))))
  (composite-binding-transformer  #`(#,form-id (parens . #,args) . #,tail)
                                  #:rest-arg rest-arg
                                  "PairList"
                                  pred
                                  #:steppers (if (null? args)
                                                 null
                                                 (cons #'values
                                                       (for/list ([arg (in-list (cdr args))])
                                                         #'cdr)))
                                  (for/list ([arg (in-list args)])
                                    #'car)
                                  (for/list ([arg (in-list args)])
                                    #'())
                                  #:index-result-info? #t
                                  #:rest-accessor (and make-rest-selector (make-rest-selector len 0))
                                  #:rest-repetition? (and rest-repetition/min #t)
                                  #:rest-repetition-min (or rest-repetition/min 0)
                                  #:rest-repetition-max rest-repetition/max
                                  #:rest-to-repetition #'in-list
                                  #:bounds-key #'#%list-bounds
                                  #:static-infos (get-list-static-infos)))

(define-binding-syntax List (make-binding generate-treelist-binding make-treelist-rest-selector get-treelist-static-infos
                                          #t #'in-treelist #'#%treelist-bounds))
(define-binding-syntax PairList (make-binding generate-list-binding make-list-rest-selector get-list-static-infos
                                              #f #'in-list #'#%list-bounds))

(begin-for-syntax
  (struct list-rest (syntax))
  (struct list-rest-splice list-rest ())
  (struct list-rest-rep list-rest ()))

(define-for-syntax (parse-*list-form stx
                                     build-form
                                     static-infos
                                     wrap-static-info
                                     #:rep-for-form rep-for-form
                                     #:rep-solo-for-form rep-solo-for-form
                                     #:repetition? repetition?
                                     #:span-form-name? span-form-name?)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (~and args (tag arg ...)) . tail)
     ;; a list of syntax, (list-rest-splice syntax), or (list-rest-rep syntax):
     (define solo? (= 2 (length (syntax->list #'(arg ...)))))
     (define content
       (let loop ([gs-stx #'(arg ...)])
         (syntax-parse gs-stx
           #:datum-literals (group)
           [() '()]
           [(rep-arg (group _::...-expr) . gs)
            (define-values (new-gs extras) (consume-extra-ellipses #'gs))
            (define e (syntax-parse #'rep-arg
                        [rep::repetition
                         (define the-rep (flatten-repetition #'rep.parsed extras))
                         (if repetition?
                             the-rep
                             (render-repetition (if solo?
                                                    rep-solo-for-form
                                                    rep-for-form)
                                                the-rep))]))
            (cons (list-rest-rep e)
                  (loop new-gs))]
           [((~or* (~and (group _::&-expr rand ...+)
                         (~parse g #`(#,group-tag rand ...))
                         (~bind [splice? #t]))
                   g)
             . gs)
            (define e (if repetition?
                          (syntax-parse #'g
                            [rep::repetition #'rep.parsed])
                          (syntax-parse #'g
                            [e::expression #'e.parsed])))
            (cons (if (attribute splice?)
                      (list-rest-splice e)
                      e)
                  (loop #'gs))])))
     (define src-span (if span-form-name?
                          (respan (datum->syntax #f (list #'form-id #'args)))
                          (maybe-respan #'args)))
     (define (tag-props stx) (datum->syntax stx (syntax-e stx) stx #'tag))
     (values
      (relocate-wrapped
       src-span
       (cond
         [(and (pair? content) (null? (cdr content))
               (list-rest-rep? (car content)))
          ;; special case, especially to expose static info on rest elements
          (define seq (list-rest-syntax (car content)))
          (cond
            [repetition? (consume-repetition seq rep-solo-for-form static-infos)]
            [else (wrap-static-info seq)])]
         [(not repetition?)
          (wrap-static-info
           (tag-props
            (build-form content)))]
         [else
          (build-compound-repetition
           stx
           content
           #:sequence-for-form rep-for-form
           #:is-sequence? list-rest-rep?
           #:extract (lambda (e) (if (list-rest? e) (list-rest-syntax e) e))
           (lambda new-content
             (let ([content (for/list ([e (in-list content)]
                                       [new-e (in-list new-content)])
                              (cond
                                [(list-rest-splice? e) (list-rest-splice new-e)]
                                [(list-rest-rep? e) (list-rest-rep new-e)]
                                [else new-e]))])
               (values (tag-props (build-form content))
                       static-infos))))]))
      #'tail)]))

(define-for-syntax (build-*list-form content *list-stx empty-*list-stx *list-append-stx
                                     ensure-*list-stx)
  ;; group content into a list of list-generating groups,
  ;; and those lists will be appended
  (define (gather group groups)
    (if (null? group)
        groups
        (cons #`(#,*list-stx #,@group) groups)))
  (define groups
    (for/foldr ([group '()]
                [groups '()]
                #:result (gather group groups))
               ([one (in-list content)])
      (cond
        [(list-rest-splice? one)
         (values '()
                 (cons #`(#,ensure-*list-stx #,(list-rest-syntax one))
                       (gather group groups)))]
        [(list-rest-rep? one)
         (values '()
                 (cons (list-rest-syntax one)
                       (gather group groups)))]
        [else (values (cons one group) groups)])))
  (cond
    [(null? groups) empty-*list-stx]
    [(null? (cdr groups)) (car groups)]
    [else #`(#,*list-append-stx #,@groups)]))

(define-for-syntax (build-treelist-form content)
  (build-*list-form content #'treelist #'empty-treelist #'treelist-append
                    #'ensure-treelist))

(define-for-syntax (build-list-form content)
  (build-*list-form content #'list #'null #'append
                    #'ensure-list))

(define-for-syntax (build-mutable-treelist-form content)
  #`(treelist-copy
     #,(build-*list-form content #'treelist #'empty-treelist #'treelist-append
                         #'ensure-treelist)))

(define-syntax List
  (expression-transformer
   (make-constructor #'treelist build-treelist-form get-treelist-static-infos wrap-treelist-static-info
                     #:rep-for-form #'for/treelist)))
(define-syntax PairList
  (expression-transformer
   (make-constructor #'list build-list-form get-list-static-infos wrap-list-static-info
                     #:rep-for-form #'for/list)))
(define-syntax MutableList
  (expression-transformer
   (make-constructor #'mutable-treelist build-mutable-treelist-form get-mutable-treelist-static-infos wrap-mutable-treelist-static-info
                     #:rep-for-form #'for/treelist
                     #:rep-solo-for-form #'for/mutable-treelist)))

(define-repetition-syntax List
  (repetition-transformer
   (make-constructor #:repetition? #t
                     #'treelist build-treelist-form get-treelist-static-infos wrap-treelist-static-info
                     #:rep-for-form #'for/treelist)))
(define-repetition-syntax PairList
  (repetition-transformer
   (make-constructor #:repetition? #t
                     #'list build-list-form get-list-static-infos wrap-list-static-info
                     #:rep-for-form #'for/list)))
(define-repetition-syntax MutableList
  (repetition-transformer
   (make-constructor #:repetition? #t
                     #'mutable-treelist build-mutable-treelist-form get-mutable-treelist-static-infos wrap-mutable-treelist-static-info
                     #:rep-for-form #'for/mutable-treelist)))

(define-for-syntax (parse-list-expression stx)
  (parse-*list-form stx build-treelist-form (get-treelist-static-infos) wrap-treelist-static-info
                    #:rep-for-form #'for/treelist
                    #:rep-solo-for-form #'for/treelist
                    #:repetition? #f
                    #:span-form-name? #f))

(define-for-syntax (parse-list-repetition stx)
  (parse-*list-form stx build-treelist-form (get-treelist-static-infos) wrap-treelist-static-info
                    #:rep-for-form #'for/treelist
                    #:rep-solo-for-form #'for/treelist
                    #:repetition? #t
                    #:span-form-name? #f))

(define (ensure-treelist v)
  (or (to-treelist #f v)
      (raise-arguments-error* 'List rhombus-realm
                              "not a listable for splicing into a list"
                              "value" v)))

(define (ensure-list v)
  (or (to-list #f v)
      (raise-arguments-error* 'PairList rhombus-realm
                              "not a listable for splicing into a pair list"
                              "value" v)))

(begin-for-syntax
  (install-get-treelist-static-infos! get-treelist-static-infos))
