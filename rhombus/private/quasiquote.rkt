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
         (only-in "underscore.rkt"
                  [_ rhombus-_])
         ;; because `expression_macro` uses the result of `convert-syntax`
         ;; as a compile-time pattern:
         (for-syntax "tail.rkt"))

(provide ?
         ¿
         (rename-out [¿ ??]
                     [rhombus... ...]))

(module+ convert
  (provide (for-syntax convert-pattern)))

(begin-for-syntax
  (define-syntax-class repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))
    (pattern ((~datum block) ((~datum group) ((~datum op) (~and name (~literal rhombus...))))))))

(define-for-syntax (convert-syntax e make-datum make-literal handle-escape deepen-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-empty-group)
  (let convert ([e e] [empty-ok? #f])
    (syntax-parse e
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum block)))
        (~and g ((~datum group) . _)))
       ;; Special case: for a single group with (), [], or {}, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs can-be-empty?) (convert #'g #t))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs)
           (values (quasisyntax/loc e (#,(make-datum #'tag) #,p))
                   new-idrs 
                   #f))]
      [((~and tag (~or (~datum parens) (~datum brackets) (~datum block) (~datum alts) (~datum group)))
        g ...)
       (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [ps '()] [can-be-empty? #t])
         (syntax-parse gs
           [()
            (let ([ps (reverse ps)]
                  [idrs (append (or pend-idrs '()) idrs)])
              (cond
                [(and can-be-empty? (eq? (syntax-e #'tag) 'alts))
                 (handle-maybe-empty-alts #'tag ps idrs)]
                [(and can-be-empty? (eq? (syntax-e #'tag) 'group) (not empty-ok?))
                 (handle-maybe-empty-group #'tag ps idrs)]
                [else
                 (values (quasisyntax/loc e (#,(make-datum #'tag) . #,ps))
                         idrs
                         can-be-empty?)]))]
           [(op:repetition . gs)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                    (deepen-escape idr)))
            (loop #'gs #f (append new-pend-idrs idrs) (cons (quote-syntax ...) ps) can-be-empty?)]
           [(((~datum op) (~and (~literal ¿) ¿-id)) esc . gs)
            (define-values (id new-idrs) (handle-escape #'¿-id #'esc e))
            (loop #'gs new-idrs (append (or pend-idrs '()) idrs) (cons id ps) (and can-be-empty? (not pend-idrs)))]
           [(g . gs)
            (define-values (p new-ids nested-can-be-empty?) (convert #'g #f))
            (loop #'gs new-ids (append (or pend-idrs '()) idrs) (cons p ps) (and can-be-empty? (not pend-idrs)))]))]
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
                  (lambda (¿-id e in-e)
                    (if (identifier? e)
                        (if (free-identifier=? e #'rhombus-_)
                            (values #'_ null)
                            (values e (list #`[#,e (pack-tail* (syntax #,e) 0)])))
                        (raise-syntax-error #f
                                            (format "expected an identifier after ~a"
                                                    (syntax-e #'¿-id))
                                            in-e
                                            e)))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id (_ (_ stx) depth))
                       #`(id (pack-tail* (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))]))
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


(define-for-syntax (convert-template e)
  (convert-syntax e
                  ;; make-datum
                  (lambda (d) d)
                  ;; make-literal
                  (lambda (d) d)
                  ;; handle-escape:
                  (lambda (¿-id e in-e)
                    (define id (car (generate-temporaries (list e))))
                    (values id (list #`[#,id (unpack-tail* (rhombus-expression (group #,e)) 0)])))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id-pat ((~literal unpack-tail*) e depth))
                       #`[(id-pat (... ...)) (unpack-tail* e #,(add1 (syntax-e #'depth)))]]
                      [(id-pat (converter depth (qs t) . args))
                       #`[(id-pat (... ...)) (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...)))) . args]]))
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

(define (convert-empty-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (if (or (null? u)
             (and (syntax? u) (null? (syntax-e u))))
         null
         (list l))]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-group (sub1 at-depth) g))]))

(define (convert-empty-alts at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (cond
       [(or (null? u)
            (and (syntax? u) (null? (syntax-e u))))
        (define a (car (syntax-e l)))
        (list (datum->syntax a 'block a a))]
       [else l])]
    [else (for/list ([g (in-list (syntax->list l))])
            (convert-empty-alts (sub1 at-depth) g))]))

(define (error-empty-group at-depth l)
  (cond
    [(zero? at-depth)
     (define u (cdr (syntax-e l)))
     (when (or (null? u)
               (and (syntax? u) (null? (syntax-e u))))
       (error '? "generated an empty group"))
     l]
    [else (for/list ([g (in-list (syntax->list l))])
            (error-empty-group (sub1 at-depth) g))]))

(require (for-syntax racket/pretty))

(define-syntax ?
  (make-expression+binding-prefix-operator
   (quote-syntax ?)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (template idrs can-be-empty?) (convert-template #'e))
        (define (wrap-bindings idrs body)
          (cond
            [(null? idrs) body]
            [else
             (wrap-bindings
              (cdr idrs)
              (syntax-parse (car idrs)
                [(id-pat e)
                 #`(with-syntax ([id-pat e])
                     #,body)]))]))
        (values (wrap-bindings idrs #`(#,(quote-syntax quasisyntax) #,template))
                #'tail)]))
   ;; pattern
   (lambda (stx)
     (syntax-parse stx
       [(op e . tail)
        (define-values (pattern idrs can-be-empty?) (convert-pattern #'e))
        (with-syntax ([((id id-ref) ...) idrs])
          (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
            (values
             (binding-form
              #'syntax
              #'()
              #'((id) ...)
              #'syntax-matcher
              #'syntax-binder
              #`(#,pattern
                 (tmp-id ...)
                 (id ...)
                 (id-ref ...)))
             #'tail)))]))))

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

(define-syntax ¿
  (expression-prefix-operator
   (quote-syntax ¿)
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(op . _)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op)]))))

(define-syntax rhombus...
  (expression-transformer
   #'rhombus...
   (lambda (stx)
     (syntax-parse stx
       [(op::operator . tail)
        (raise-syntax-error #f
                            "misuse outside of ?"
                            #'op.name)]))))
