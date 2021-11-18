#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt"
                     "srcloc.rkt")
         syntax/parse
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "tail.rkt"
         "syntax-list.rkt"
         "empty-group.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_])
         ;; because `expr.macro` uses the result of `convert-pattern`
         ;; as a compile-time pattern, for example:
         (for-syntax "tail.rkt"
                     "syntax-list.rkt"
                     "empty-group.rkt"))

(provide |'|
         $
         (rename-out [rhombus... ...])
         ......)

(module+ convert
  (provide (for-syntax convert-pattern
                       convert-template)))

(begin-for-syntax
  (define-syntax-class list-repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))
    (pattern ((~datum block) ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))))
  (define-splicing-syntax-class tail-repetition
    #:literals ($ ......)
    #:datum-literals (op)
    (pattern (~seq (op $) e (op (~and name ......))))
    (pattern (~seq ((~datum group) (op $) e)
                   ((~datum group) (op (~and name ......)))))
    (pattern (~seq ((~datum block) ((~datum group) (op $) e))
                   ((~datum block) ((~datum group) (op (~and name ......))))))))

(define-for-syntax (convert-syntax e make-datum make-literal handle-escape deepen-escape handle-tail-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-empty-group)
  (let convert ([e e] [empty-ok? #f] [depth 0])
    (syntax-parse e
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum braces) (~datum block)))
        (~and g ((~datum group) . _)))
       ;; Special case: for a single group with (), [], {}, or block, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs can-be-empty?) (convert #'g #t depth))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs)
           (values (quasisyntax/loc e (#,(make-datum #'tag) #,p))
                   new-idrs 
                   #f))]
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum braces) (~datum block) (~datum alts) (~datum group)))
        g ...)
       (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [ps '()] [can-be-empty? #t] [tail #f] [depth depth])
         (define (simple gs a-depth)
           (syntax-parse gs
             [(g . gs)
              (define-values (p new-ids nested-can-be-empty?) (convert #'g #f a-depth))
              (loop #'gs new-ids (append (or pend-idrs '()) idrs) (cons p ps) (and can-be-empty? (not pend-idrs)) #f depth)]))
         (define (simple2 gs a-depth)
           (syntax-parse gs
             [(g0 g1 . gs)
              (define-values (p0 new-ids0 nested-can-be-empty?0) (convert #'g0 #f a-depth))
              (define-values (p1 new-ids1 nested-can-be-empty?1) (convert #'g1 #f a-depth))
              (loop #'gs (append new-ids0 new-ids1) (append (or pend-idrs '()) idrs) (list* p1 p0 ps) (and can-be-empty? (not pend-idrs)) #f depth)]))
         (syntax-parse gs
           [()
            (let ([ps (let ([ps (reverse ps)])
                        (if tail
                            (append ps tail)
                            ps))]
                  [idrs (append (or pend-idrs '()) idrs)]
                  [can-be-empty? (and can-be-empty? (not pend-idrs))])
              (cond
                [(and can-be-empty? (eq? (syntax-e #'tag) 'alts))
                 (handle-maybe-empty-alts #'tag ps idrs)]
                [(and can-be-empty? (eq? (syntax-e #'tag) 'group) (not empty-ok?))
                 (handle-maybe-empty-group #'tag ps idrs)]
                [else
                 (values (quasisyntax/loc e (#,(make-datum #'tag) . #,ps))
                         idrs
                         can-be-empty?)]))]
           [(op:list-repetition . gs)
            #:when (zero? depth)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                    (deepen-escape idr)))
            (loop #'gs #f (append new-pend-idrs idrs) (cons (quote-syntax ...) ps) can-be-empty? #f depth)]
           [(op:tail-repetition . gs)
            #:when (zero? depth)
            (unless (null? (syntax-e #'gs))
              (raise-syntax-error #f
                                  "misplaced tail repetition"
                                  #'op.name))
            (define-values (id new-idrs) (handle-tail-escape #'op.name #'op.e e))
            (loop #'() #f (append new-idrs (or pend-idrs '()) idrs) ps (and can-be-empty? (not pend-idrs)) id depth)]
           [(((~datum op) (~and (~literal $) $-id)) esc . n-gs)
            (cond
              [(zero? depth)
               (define-values (id new-idrs) (handle-escape #'$-id #'esc e))
               (loop #'n-gs new-idrs (append (or pend-idrs '()) idrs) (cons id ps) (and can-be-empty? (not pend-idrs)) #f depth)]
              [else
               (simple2 gs (sub1 depth))])]
           [(((~datum op) (~literal |'|)) unesc . _)
            (simple2 gs (add1 depth))]
           [(g . _)
            (simple gs depth)]))]
      [((~and tag (~datum op)) op-name)
       (values (quasisyntax/loc e (#,(make-datum #'tag) #,(make-literal #'op-name))) null #f)]
      [id:identifier
       (values (make-literal #'id) null #f)]
      [_
       (values e null #f)])))

(define-for-syntax (convert-pattern e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-literal
                  (lambda (d)
                    #`(~literal #,d))
                  ;; handle-escape:
                  (lambda ($-id e in-e)
                    (if (identifier? e)
                        (if (free-identifier=? e #'rhombus-_)
                            (values #'_ null)
                            (values e (list #`[#,e (pack-list* (syntax #,e) 0)])))
                        (raise-syntax-error #f
                                            (format "expected an identifier after ~a"
                                                    (syntax-e $-id))
                                            in-e
                                            e)))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id (_ (_ stx) depth))
                       #`(id (pack-list* (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))]))
                  ;; handle-tail-escape:
                  (lambda (name e in-e)
                    (if (identifier? e)
                        (values e (list #`[#,e (pack-list* (pack-tail (syntax #,e)) 0)]))
                        (raise-syntax-error #f
                                            (format "expected an identifier for use with ~a"
                                                    (syntax-e name))
                                            in-e
                                            e)))
                  ;; handle-maybe-empty-sole-group
                  (lambda (tag pat idrs)
                    ;; `pat` matches a `group` form that's supposed to be under `tag`,
                    ;; but if `pat` match `(group)`, then allow an overall match to `(tag)`
                    (values #`(~or* ((~datum #,tag) #,pat)
                                    (~and ((~datum #,tag))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,(syntax-parse pat
                                                   [(_ . tail) #'tail]))))
                            idrs
                            #f))
                  ;; handle-maybe-empty-alts
                  (lambda (tag ps idrs)
                    ;; if `(tag . ps)` would match `(alts)`, then let it match `(block)`
                    (values #`(~or* ((~datum #,tag) . #,ps)
                                    (~and ((~datum block))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,ps)))
                            idrs
                            #t))
                  ;; handle-maybe-empty-alts
                  (lambda (tag ps idrs)
                    ;; the `(tag . ps)` could match `(group)`, but it just never will,
                    ;; because that won't be an input
                    (values #`((~datum #,tag) . #,ps) idrs #t))))


(define-for-syntax (convert-template e
                                     #:check-escape [check-escape (lambda (e) (void))]
                                     #:rhombus-expression [rhombus-expression #'rhombus-expression])
  (define-values (template idrs can-be-empty?)
    (convert-syntax e
                    ;; make-datum
                    (lambda (d) d)
                    ;; make-literal
                    (lambda (d) d)
                    ;; handle-escape:
                    (lambda ($-id e in-e)
                      (check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-list* (quote-syntax #,$-id) (#,rhombus-expression (group #,e)) 0)])))
                    ;; deepen-escape
                    (lambda (idr)
                      (syntax-parse idr
                        [(id-pat ((~literal unpack-list*) q e depth))
                         #`[(id-pat (... ...)) (unpack-list* q e #,(add1 (syntax-e #'depth)))]]
                        [(id-pat (converter depth (qs t) . args))
                         #`[(id-pat (... ...)) (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...)))) . args]]))
                    ;; handle-tail-escape:
                    (lambda (name e in-e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-list* #f (unpack-tail (#,rhombus-expression (group #,e)) '#,name) 0)])))
                    ;; handle-maybe-empty-sole-group
                    (lambda (tag template idrs)
                      ;; if `template` generates `(group)`, then instead of `(tag (group))`,
                      ;; produce `(tag)`
                      (define id (car (generate-temporaries '(group))))
                      (values #`(#,tag #,id (... ...))
                              (cons #`[(#,id (... ...))
                                       (convert-empty-group 0 (#,(quote-syntax quasisyntax) #,template))]
                                    idrs)
                              #f))
                    ;; handle-maybe-empty-alts
                    (lambda (tag ts idrs)
                      ;; if `(tag . ts)` generates `(alts)`, then produce `(block)` instead
                      (define id (car (generate-temporaries '(alts))))
                      (values id
                              (cons #`[#,id (convert-empty-alts 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              #f))
                    ;; handle-maybe-empty-group
                    (lambda (tag ts idrs)
                      ;; if `(tag . ts)` generates `(group)`, then error
                      (define id (car (generate-temporaries '(group))))
                      (values id
                              (cons #`[#,id (error-empty-group 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              #f))))
  (define (wrap-bindings idrs body)
    (cond
      [(null? idrs) body]
      [else
       (wrap-bindings (cdr idrs)
                      (syntax-parse (car idrs)
                        [(id-pat e)
                         #`(with-syntax ([id-pat e])
                             #,body)]))]))
  (wrap-bindings idrs #`(#,(quote-syntax quasisyntax) #,template)))

(define-for-syntax (call-with-quoted-expression stx k)
  (define (classify-follower next-stx)
    (syntax-parse next-stx
      #:datum-literals (op parens brackets braces)
      [(op name)
       (values #'name "subsequent operator")]
      [(parens . _)
       (values (datum->syntax next-stx '#%call) "subsequent parentheses")]
      [(brackets . _)
       (values (datum->syntax next-stx '#%ref) "subsequent brackets")]
      [(braces . _)
       (values (datum->syntax next-stx '#%comp) "subsequent braces")]
      [(braces . _)
       (values (datum->syntax next-stx '#%juxtapose) "subsequent expression")]
      [else #f]))
  (syntax-parse stx
    #:datum-literals (op)
    [((op q) e)
     (values (k #'e) #'())]
    [((op q) (op name) . _)
     (raise-syntax-error #f
                         "need parentheses to quote an operator"
                         #'q
                         #'name)]
    [((op q) e next . _)
     #:do [(define-values (next-op what) (classify-follower #'next))
           (define prec (if next-op
                            (expression-relative-precedence 'prefix #'q 'infix next-op)
                            'stronger))]
     #:when (not (eq? prec 'stronger))
     (raise-syntax-error #f
                         (format "ambiguous quoting due to ~a; add parentheses"
                                 what)
                         #'q
                         #'next)]
    [((op q) e . tail)
     (values (k #'e)
             #'tail)]))

(define-syntax |'|
  (make-expression+binding-prefix-operator
   (quote-syntax |'|)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (call-with-quoted-expression stx convert-template))
   ;; pattern
   (lambda (stx)
     (call-with-quoted-expression
      stx
      (lambda (e)
        (define-values (pattern idrs can-be-empty?) (convert-pattern e))
        (with-syntax ([((id id-ref) ...) idrs])
          (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
            (binding-form
             #'syntax-infoer
             #`(#,pattern
                (tmp-id ...)
                (id ...)
                (id-ref ...))))))))))

(define-syntax (syntax-infoer stx)
  (syntax-parse stx
    [(_ static-infos (pattern tmp-ids (id ...) id-refs))
     (binding-info #'syntax
                   #'()
                   #'((id) ...)
                   #'syntax-matcher
                   #'syntax-binder
                   #'(pattern tmp-ids (id ...) id-refs))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ arg-id (pattern (tmp-id ...) (id ...) (id-ref ...)) IF success fail)
     #'(IF (syntax? arg-id)
           (begin
             (define-values (match? tmp-id ...)
               (syntax-parse arg-id
                 [pattern (values #t id-ref ...)]
                 [_ (values #f 'id ...)]))
             (IF match?
                 success
                 fail))
           fail)]))

(define-syntax (syntax-binder stx)
  (syntax-parse stx
    [(_ arg-id (pattern (tmp-id ...) (id ...) (id-ref ...)))
     #'(begin
         (define id tmp-id) ...)]))

(define-syntax $
  (expression-prefix-operator
   (quote-syntax $)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of a pattern or template"
                            #'op)]))))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a pattern"
                            #'op.name)]))))

(define-syntax ......
  (expression-transformer
   #'......
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of a pattern"
                            #'op.name)]))))
