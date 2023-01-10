#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         syntax/parse/pre
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "definition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "parsed.rkt"
         "pack.rkt"
         (only-in "def+let.rkt" def)
         (rename-in "ellipsis.rkt"
                    [... rhombus...]))

(provide (rename-out [rhombus-syntax syntax]))

(define-simple-name-root rhombus-syntax
  class)

;; should this be in "definition.rkt"?
(define-syntax parsed-defn
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ (_ d)) #'(d)]))))

(begin-for-syntax
  (define-syntax-class :attribute-lhs
    #:datum-literals (brackets group op)
    #:literals (rhombus...)
    (pattern id:identifier
             #:attr depth #'0)
    (pattern (brackets (group a::attribute-lhs) (group (op rhombus...)))
             #:attr id #'a.id
             #:attr depth #`#,(+ 1 (syntax-e #'a.depth)))))

(define-for-syntax (generate-pattern-and-attributes orig-stx stx kind splicing?)
  (define (generate pat body)
    (define in-quotes
      (cond
        [(eq? kind 'multi)
         (syntax-parse pat
           [(_ g ...) #'(multi g ...)])]
        [(eq? kind 'block)
         (syntax-parse pat
           #:datum-literals (block)
           [(_ (~and g (group (block . _)))) #'g]
           [_ (raise-syntax-error #f
                                  "not a block pattern"
                                  orig-stx
                                  pat)])]
        [else
         (syntax-parse pat
           [(_ g) #'g]
           [(_) (raise-syntax-error #f
                                    "no groups in pattern"
                                    orig-stx
                                    pat)]
           [_ (raise-syntax-error #f
                                  "multiple groups in pattern"
                                  orig-stx
                                  pat)])]))
    (define-values (p idrs sidrs vars can-be-empty?)
      (convert-pattern #:splice? (not (eq? kind 'group))
                       #:splice-pattern (and (not (eq? kind 'group))
                                             (not splicing?)
                                             (lambda (ps)
                                               (cond
                                                 [(eq? kind 'multi)
                                                  #`(_ #,@ps)]
                                                 [(= 1 (length ps))
                                                  (car ps)]
                                                 [else
                                                  (raise-syntax-error #f
                                                                      "not a single-term pattern"
                                                                      orig-stx
                                                                      pat)])))
                       in-quotes))
    (define-values (pattern-body explicit-attrs)
      (for/fold ([body-forms null]
                 [attrs null]
                 #:result
                 (values
                  (reverse body-forms)
                  (reverse attrs)))
                ([g (in-list (syntax->list body))])
        (syntax-parse g
          #:datum-literals (group block)
          [(group #:attr attr::attribute-lhs (block in-block ...))
           (values
            (cons #`(group def attr.id (block in-block ...)) body-forms)
            (cons (pattern-variable #'attr.id #'attr.id (syntax-e #'attr.depth) (quote-syntax unpack-element*)) attrs))]
          [other
           (values (cons #'other body-forms) attrs)])))
    (define all-attrs (append vars explicit-attrs))
    (with-syntax ([((attr ...) ...)
                   (map (lambda (var)
                          #`(#:attr
                             (#,(pattern-variable-id var) #,(pattern-variable-depth var))
                             (#,(pattern-variable-unpack*-id var)
                              (quote-syntax dots)
                              #,(pattern-variable-val-id var)
                              #,(pattern-variable-depth var))))
                        all-attrs)]
                  [(body-form ...) pattern-body]
                  [([val-id val-rhs] ...) idrs]
                  [([stx-id stx-rhs] ...) sidrs])
      (values #`(pattern #,p #:do [(define val-id val-rhs)
                                   ...
                                   (define-syntax stx-id stx-rhs)
                                   ...
                                   (define-values #,(map pattern-variable-val-id explicit-attrs)
                                     (rhombus-body
                                      body-form ...
                                      (group (parsed (values #,@(map pattern-variable-val-id explicit-attrs))))))]
                         attr ... ...)
              (map (lambda (attr)
                     (cons (syntax-e (pattern-variable-id attr))
                           (pattern-variable-depth attr)))
                   all-attrs))))
  (syntax-parse stx
    #:datum-literals (alts group quotes block)
    [(block (group (~and pat (quotes . _))))
     (generate #'pat #'())]
    [(block (group (~and pat (quotes . _))
                   (block body ...)))
     (generate #'pat #'(body ...))]))

(define-for-syntax (generate-syntax-class stx class-name kind-kw-stx alts description)
  (define-values (kind splicing?)
    (let ([kind (string->symbol (keyword->string (syntax-e kind-kw-stx)))])
      (cond
        [(eq? kind 'sequence) (values 'term #t)]
        [else (values kind #f)])))
  (define-values (patterns attributes)
    (for/lists (patterns attributess
                         #:result (values patterns (intersect-attributes stx attributess)))
        ([alt-stx (in-list alts)])
      (generate-pattern-and-attributes stx alt-stx kind splicing?)))
  (define define-class (if splicing?
                           #'define-splicing-syntax-class
                           #'define-syntax-class))
  (list
   #`(#,define-class #,class-name
      #:description #,(if description #`(rhombus-body #,@description) #f)
      #:datum-literals (block group quotes)
      #,@patterns)
   #`(define-syntax #,(in-syntax-class-space class-name)
       (rhombus-syntax-class '#,kind #'#,class-name '#,attributes #,splicing?))))

(define-for-syntax (intersect-attributes stx attributess)
  (cond
    [(null? attributess) '()]
    [(null? (cdr attributess)) (car attributess)]
    [else
     ;; start with initial set
     (define ht0
       (for/hasheq ([name+depth (in-list (car attributess))])
         (values (car name+depth) (cdr name+depth))))
     ;; intersect by pruning set
     (define ht
       (for/fold ([ht0 ht0]) ([attributes (in-list (cdr attributess))])
         (for/fold ([ht #hasheq()]) ([name+depth (in-list attributes)])
           (if (hash-ref ht0 (car name+depth) #f)
               (hash-set ht (car name+depth) (cdr name+depth))
               ht))))
     ;; check consistent depths
     (for* ([attributes (in-list attributess)]
            [name+depth (in-list attributes)])
       (unless (eqv? (cdr name+depth) (hash-ref ht (car name+depth) (cdr name+depth)))
         (raise-syntax-error #f
                             "attribute at different repetition depths in different clauses"
                             stx
                             (car name+depth))))
     ;; convert back to list of pairs
     (for/list ([(sym depth) (in-hash ht)])
       (cons sym depth))]))

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (alts group quotes block)
        ;; Classname and patterns shorthand
        [(form-id class-name (alts alt ...))
         (generate-syntax-class stx #'class-name #'#:sequence (syntax->list #'(alt ...)) #f)]
        ;; Specify patterns with "pattern"
        [(form-id class-name
                  (block
                   (~alt (~optional (group #:description (block class-desc ...)))
                         (~optional (group (~and kind-kw (~or* #:term
                                                               #:sequence
                                                               #:group
                                                               #:multi
                                                               #:block)))
                                    #:defaults ([kind-kw #'#:sequence])))
                   ...
                   (group #:pattern (alts alt ...))))
         (generate-syntax-class stx #'class-name #'kind-kw (syntax->list #'(alt ...)) (attribute class-desc))]
        [_
         (raise-syntax-error #f "expected alternatives" stx)]))))
