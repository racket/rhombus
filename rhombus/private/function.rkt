#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre
                     "srcloc.rkt"
                     "consistent.rkt")
         racket/unsafe/undefined
         "provide.rkt"
         (submod "function-parse.rkt" for-build)
         "parens.rkt"
         "expression.rkt"
         "expression+definition.rkt"
         "annotation.rkt"
         "definition.rkt"
         "entry-point.rkt"
         "parse.rkt"
         (submod "match.rkt" for-match-id)
         "name-root.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         "dotted-sequence-parse.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-parse.rkt"
         "realm.rkt")

(provide (for-spaces (#f
                      rhombus/entry_point)
                     fun)
         (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Function))

(module+ for-method
  (provide fun/read-only-property))

(module+ for-builtin
  (provide function-method-table))

(define-name-root Function
  #:fields
  (map
   of_arity
   pass))

(define function-method-table
  (hash 'map (lambda (f) (lambda lists (apply map f lists)))))

(define-for-syntax function-static-infos
  #'((#%dot-provider function-instance)))

(define-for-syntax (wrap-function-static-info expr)
  (wrap-static-info* expr function-static-infos))

(define-annotation-syntax Function (identifier-annotation #'procedure? function-static-infos))

(define-annotation-syntax of_arity
  (annotation-prefix-operator
   (annot-quote of_arity)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens g ...+) . tail)
        (with-syntax ([(kw ...) (for/list ([g (in-list (syntax->list #'(g ...)))]
                                           #:do [(define kw
                                                   (syntax-parse g
                                                     [(_ kw:keyword) #'kw]
                                                     [_ #f]))]
                                           #:when kw)
                                  kw)]
                      [(g ...) (for/list ([g (in-list (syntax->list #'(g ...)))]
                                          #:unless (syntax-parse g
                                                     [(_ _:keyword) #t]
                                                     [_ #f]))
                                 g)])
          (with-syntax ([(kw-ok? kw-check) (let ([kws (syntax->list #'(kw ...))])
                                             (cond
                                               [(null? kws) #'(#f #t)]
                                               [else
                                                (for/fold ([ht #hasheq()]) ([kw-stx (in-list kws)])
                                                  (define kw (syntax-e kw-stx))
                                                  (when (hash-ref ht kw #f)
                                                    (raise-syntax-error #f "duplicate keyword" kw #f))
                                                  (hash-set ht kw #t))
                                                #`(#t (accepts-keywords? v '#,(sort (map syntax-e kws) keyword<?)))]))]
                        [(n ...) (generate-temporaries #'(g ...))])
            (values (annotation-predicate-form
                     #`(let ([n (check-nonneg-integer 'Function.of_arity (rhombus-expression g))]
                             ...)
                         (lambda (v)
                           (and (procedure? v)
                                (procedure-arity-includes? v n kw-ok?)
                                ...
                                kw-check)))
                     function-static-infos)
                    #'tail)))]))))

(define (check-nonneg-integer who v)
  (unless (exact-nonnegative-integer? v)
    (raise-argument-error* who rhombus-realm "NonnegInt" v))
  v)

(define (accepts-keywords? proc kws)
  (define-values (req allow) (procedure-keywords proc))
  (define (kw-subset big small)
    (let loop ([big big] [small small])
      (cond
        [(null? small) #t]
        [(null? big) #f]
        [(eq? (car small) (car big)) (loop (cdr big) (cdr small))]
        [(keyword<? (car small) (car big)) #f]
        [else (loop (cdr big) small)])))
  (and (or (not allow)
           (kw-subset allow kws))
       (kw-subset kws req)))

(define-syntax function-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(map) (nary #'map -2 #'map)]
        [else #f])))))

(begin-for-syntax  
  (define (get-local-name who)
    (or (syntax-local-name) who)))

(define-syntax fun
  (make-expression+definition-transformer
   (expression-transformer
    (lambda (stx)
      (parse-anonymous-function stx no-adjustments #f)))
   (definition-transformer
     (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group block alts parens)
        ;; alts case
        [(form-id (alts-tag::alts
                   (block (group name-seq::dotted-identifier-sequence (_::parens arg::kw-binding ... rest::maybe-arg-rest)
                                 ret::ret-annotation
                                 (~and rhs (_::block body ...))))
                   ...+))
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (define-values (proc arity)
           (build-case-function no-adjustments
                                the-name #'#f
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                #'(ret.converter ...)
                                #'(rhs ...)
                                #'form-id #'alts-tag))
         (maybe-add-function-result-definition
          the-name (syntax->list #'(ret.static-infos ...)) function-static-infos arity
          (build-definitions/maybe-extension #f the-name (car (syntax->list #'(name.extends ...)))
                                             proc))]
        ;; `match` containing alts case --- almost the same, but with a declared name and maybe return annotation
        [(form-id main-name-seq::dotted-identifier-sequence main-ret::ret-annotation
                  (_::block
                   ~!
                   (group _::match
                          (alts-tag::alts
                           (block (group name-seq::dotted-identifier-sequence (_::parens arg::kw-binding ... rest::maybe-arg-rest)
                                         ret::ret-annotation
                                         (~and rhs (_::block body ...))))
                           ...+))))
         #:with main-name::dotted-identifier #'main-name-seq
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name #'main-name.name)
         (check-consistent stx (cons the-name names) "name" #:has-main? #t)
         (define-values (proc arity)
           (build-case-function no-adjustments
                                the-name #'main-ret.converter
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                #'(ret.converter ...)
                                #'(rhs ...)
                                #'form-id #'alts-tag))
         (maybe-add-function-result-definition
          the-name (list #'main-ret.static-infos) function-static-infos arity
          (build-definitions/maybe-extension #f the-name (car (syntax->list #'(name.extends ...)))
                                             proc))]
        ;; single-alterative case
        [(form-id name-seq::dotted-identifier-sequence (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest)
                  ret::ret-annotation
                  (~and rhs (_::block body ...)))
         #:with name::dotted-identifier #'name-seq
         (define-values (proc arity)
           (build-function no-adjustments
                           #'name.name
                           #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                           #'rest.arg #'rest.parsed
                           #'rest.kwarg #'rest.kwparsed
                           #'ret.converter
                           #'rhs
                           #'form-id #'parens-tag))
         (maybe-add-function-result-definition
          #'name.name (list #'ret.static-infos) function-static-infos arity
          (build-definitions/maybe-extension #f #'name.name #'name.extends
                                             proc))]
        ;; definition form didn't match, so try parsing as a `fun` expression:
        [(_ (~or (_::parens _ ...)
                 (_::alts (block (group (parens _ ...) . _)) ...+)
                 (~seq _ ... (_::block (group _::match (_::alts . _)))))
            . _)
         (syntax-parse #`(group . #,stx)
           [e::expression
            (list #'(#%expression e.parsed))])])))))

(define-entry-point-syntax fun
  (entry-point-transformer
   ;; parse function:
   (lambda (stx adjustments)
     (define-values (term tail) (parse-anonymous-function stx adjustments #t))
     (syntax-parse tail
       [() term]
       [_ (raise-syntax-error #f
                              "unexpected extra terms"
                              tail)]))
   ;; extract arity:
   (lambda (stx)
     (parse-anonymous-function-arity stx))))

(define-syntax fun/read-only-property
  (entry-point-transformer
   ;; parse function:
   (lambda (stx adjustments)
     (define-values (term tail) (parse-anonymous-function stx adjustments #t))
     term)
   ;; extract arity:
   (lambda (stx)
     1)))
  
(define-for-syntax (parse-anonymous-function stx [adjustments no-adjustments] [for-entry? #f])
  (syntax-parse stx
    #:datum-literals (group block alts)
    ;; alts case
    [(form-id (alts-tag::alts
               (block (group (_::parens arg::kw-binding ... rest::maybe-arg-rest) ret::ret-annotation
                             (~and rhs (_::block body ...))))
               ...+)
              . tail)
     (define-values (proc arity)
       (build-case-function adjustments
                            (get-local-name #'form-id) #'#f
                            #'((arg.kw ...) ...)
                            #'((arg ...) ...) #'((arg.parsed ...) ...)
                            #'(rest.arg ...) #'(rest.parsed ...)
                            #'(rest.kwarg ...) #'(rest.kwparsed ...)
                            #'(ret.converter ...)
                            #'(rhs ...)
                            #'form-id #'alts-tag))
     (values (if arity
                 (wrap-static-info proc #'#%function-arity arity)
                 proc)
             #'tail)]
    ;; `match` containing alts case --- almost the same, but with maybe a declared return annotation
    [(form-id main-ret::ret-annotation
              (_::block
               ~!
               (group _::match
                      (alts-tag::alts
                       (block (group (_::parens arg::kw-binding ... rest::maybe-arg-rest) ret::ret-annotation
                                     (~and rhs (_::block body ...))))
                       ...+)
                      . tail)))
     (define-values (proc arity)
       (build-case-function adjustments
                            (get-local-name #'form-id) #'main-ret.converter
                            #'((arg.kw ...) ...)
                            #'((arg ...) ...) #'((arg.parsed ...) ...)
                            #'(rest.arg ...) #'(rest.parsed ...)
                            #'(rest.kwarg ...) #'(rest.kwparsed ...)
                            #'(ret.converter ...)
                            #'(rhs ...)
                            #'form-id #'alts-tag))
     (values (wrap-function-static-info
              (if arity
                  (wrap-static-info proc #'#%function-arity arity)
                  proc))
             #'tail)]
    ;; single-alterative case
    [(form-id (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest) ret::ret-annotation
              (~and rhs (_::block body ...))
              . tail)
     (define-values (fun arity)
       (build-function adjustments
                       (get-local-name #'form-id)
                       #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                       #'rest.arg #'rest.parsed
                       #'rest.kwarg #'rest.kwparsed
                       #'ret.converter
                       #'rhs
                       #'form-id #'parens-tag))
     (values (let* ([fun (if (pair? (syntax-e #'ret.static-infos))
                             (wrap-static-info fun #'#%call-result #'ret.static-infos)
                             fun)]
                    [fun (if arity
                             (wrap-static-info fun #'#%function-arity arity)
                             fun)])
               (wrap-function-static-info fun))
             #'tail)]))


(define pass
  (make-keyword-procedure
   (let ([pass (lambda (kws kw-args . args)
                 (void))])
     pass)))
