#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/print
                     "operator-parse.rkt"
                     "srcloc.rkt"
                     "expression.rkt")
         (for-meta 2 racket/base syntax/parse "expression.rkt")
         syntax/parse
         "parse.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "pack.rkt"
         "empty-group.rkt"
         "syntax-class.rkt"
         "operator-parse.rkt"
         (submod "syntax-class.rkt" for-quasiquote)
         (only-in "underscore.rkt"
                  [_ rhombus-_])
         (only-in "annotation.rkt"
                  ::))

(provide #%quote
         syntax_term
         $
         (rename-out [rhombus... ...]))

(module+ convert
  (provide (for-syntax convert-pattern
                       convert-template)))

(begin-for-syntax
  (define-syntax-class list-repetition
    (pattern ((~datum op) (~and name (~literal rhombus...))))
    (pattern ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))
    (pattern ((~datum block) ((~datum group) ((~datum op) (~and name (~literal rhombus...)))))))
  (define-splicing-syntax-class tail-repetition
    #:literals ($ rhombus...)
    #:datum-literals (op)
    (pattern (~seq (op $) e (op (~and name rhombus...)))))
  (define-splicing-syntax-class block-tail-repetition
    #:literals ($ rhombus...)
    #:datum-literals (op)
    (pattern (~seq ((~datum group) (op $) e)
                   ((~datum group) (op (~and name rhombus...)))))))

(define-for-syntax (make-pattern-variable-syntax temp-id attributes)
  (expression-prefix-operator
   #'pattern-var
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       [(var-id (op |.|) attr)
        (values
         (hash-ref
          attributes
          (syntax->datum #'attr)
          (lambda ()
            (raise-syntax-error #f
                                (format
                                 (string-append "attribute not found\n"
                                                "  pattern: ~a\n"
                                                "  attribute: ~a")
                                 (syntax-e #'var-id)
                                 (syntax-e #'attr))
                                stx)))
         #'())]
       [(var-id)
        (values temp-id #'())]))))

(define-for-syntax (convert-syntax e make-datum make-literal
                                   handle-escape handle-group-escape handle-multi-escape
                                   deepen-escape
                                   handle-tail-escape handle-block-tail-escape
                                   handle-maybe-empty-sole-group
                                   handle-maybe-empty-alts handle-maybe-empty-group
                                   #:tail-any-escape? [tail-any-escape? #f]
                                   #:as-tail? [as-tail? #f]
                                   #:splice? [splice? #f])
  (let convert ([e e] [empty-ok? splice?] [depth 0] [as-tail? as-tail?])
    (syntax-parse e
      #:datum-literals (parens brackets braces block quotes multi group alts)
      [(group
        (op (~and (~literal $) $-id)) esc)
       #:when (and (zero? depth) (not as-tail?))
       ;; Special case: a group whose content is an escape; the escape
       ;; defaults to "group" mode instead of "term" mode
       #:do [(define-values (p new-idrs new-sidrs) (handle-group-escape #'$-id #'esc e))]
       #:when p
       (values p new-idrs new-sidrs #f)]
      [((~and tag (~or parens brackets braces quotes multi block))
        (group (op (~and (~literal $) $-id)) esc))
       #:when (and (zero? depth) (not as-tail?))
       ;; Analogous special case, but for blocks (maybe within an `alts`), etc.
       #:do [(define-values (p new-idrs new-sidrs) (handle-multi-escape  #'$-id #'esc e))]
       #:when p
       (values p new-idrs new-sidrs #f)]
      [((~and tag (~or parens brackets braces quotes multi block))
        (~and g (group . _)))
       ;; Special case: for a single group with (), [], {}, '', or block, if the group
       ;; can be empty, allow a match/construction with zero groups
       (define-values (p new-idrs new-sidrs can-be-empty?) (convert #'g #t depth as-tail?))
       (if can-be-empty?
           (handle-maybe-empty-sole-group #'tag p new-idrs new-sidrs)
           (values (quasisyntax/loc e (#,(make-datum #'tag) #,p))
                   new-idrs
                   new-sidrs
                   #f))]
      [((~and tag (~or parens brackets braces quotes multi block alts group))
        g ...)
       ;; Note: this is where `depth` would be incremented, when `tag` is `quotes`, if we wanted that
       (let loop ([gs #'(g ...)] [pend-idrs #f] [idrs '()] [sidrs '()] [ps '()] [can-be-empty? #t] [tail #f] [depth depth])
         (define (simple gs a-depth)
           (syntax-parse gs
             [(g . gs)
              (define-values (p new-ids new-sidrs nested-can-be-empty?) (convert #'g #f a-depth #f))
              (loop #'gs new-ids (append (or pend-idrs '()) idrs) (append new-sidrs sidrs) (cons p ps) (and can-be-empty? (not pend-idrs)) #f depth)]))
         (define (simple2 gs a-depth)
           (syntax-parse gs
             [(g0 g1 . gs)
              (define-values (p0 new-ids0 new-sids0 nested-can-be-empty?0) (convert #'g0 #f a-depth #f))
              (define-values (p1 new-ids1 new-sids1 nested-can-be-empty?1) (convert #'g1 #f a-depth #f))
              (loop #'gs (append new-ids0 new-ids1) (append (or pend-idrs '()) idrs) (append new-sids0 new-sids1 sidrs) (list* p1 p0 ps) (and can-be-empty? (not pend-idrs)) #f depth)]))
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
                 (handle-maybe-empty-alts #'tag ps idrs sidrs)]
                [(and can-be-empty? (eq? (syntax-e #'tag) 'group) (not empty-ok?))
                 (handle-maybe-empty-group #'tag ps idrs sidrs)]
                [else
                 (values
                  (if splice?
                      (quasisyntax/loc e (~seq . #,ps))
                      (quasisyntax/loc e (#,(make-datum #'tag) . #,ps)))
                  idrs
                  sidrs
                  can-be-empty?)]))]
           [(op:tail-repetition)
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.e)))
            (define-values (id new-idrs new-sidrs) (handle-tail-escape #'op.name #'op.e e))
            (loop #'() #f (append new-idrs (or pend-idrs '()) idrs) (append new-sidrs sidrs) ps (and can-be-empty? (not pend-idrs)) id depth)]
           [(op:block-tail-repetition)
            #:when (and (zero? depth)
                        (or tail-any-escape?
                            (identifier? #'op.e)))
            (define-values (id new-idrs new-sidrs) (handle-block-tail-escape #'op.name #'op.e e))
            (loop #'() #f (append new-idrs (or pend-idrs '()) idrs) (append new-sidrs sidrs) ps (and can-be-empty? (not pend-idrs)) id depth)]
           [(op:list-repetition . gs)
            #:when (zero? depth)
            (unless pend-idrs
              (raise-syntax-error #f
                                  "misplaced repetition"
                                  #'op.name))
            (define new-pend-idrs (for/list ([idr (in-list pend-idrs)])
                                    (deepen-escape idr)))
            (loop #'gs #f (append new-pend-idrs idrs) sidrs (cons (quote-syntax ...) ps) can-be-empty? #f depth)]
           [(((~datum op) (~and (~literal $) $-id)) esc . n-gs)
            (cond
              [(zero? depth)
               (define-values (pat new-idrs new-sidrs) (handle-escape #'$-id #'esc e))
               (loop #'n-gs new-idrs (append (or pend-idrs '()) idrs) (append new-sidrs sidrs) (cons pat ps) (and can-be-empty? (not pend-idrs)) #f depth)]
              [else
               (simple2 gs (sub1 depth))])]
           [(g . _)
            (simple gs depth)]))]
      [((~and tag op) op-name)
       (values (quasisyntax/loc e (#,(make-datum #'tag) #,(make-literal #'op-name))) null null #f)]
      [id:identifier
       (values (make-literal #'id) null null #f)]
      [_
       (values e null null #f)])))

(define-for-syntax (convert-pattern e #:as-tail? [as-tail? #f] #:splice? [splice? #f])
  (define (handle-escape $-id e in-e pack* context-syntax-class kind)
    (syntax-parse e
      #:datum-literals (parens op group)
      #:literals (rhombus-_ ::)
      [rhombus-_ (values #'_ null null)]
      [_:identifier
       #:with (tag . _) in-e
       (let ([temp (car (generate-temporaries #'(#,e)))])
         (values temp (list #`[#,e (#,pack* (syntax #,temp) 0)]) null))]
      [(parens (group id:identifier (op ::) stx-class:identifier))
       (define rsc (syntax-local-value (in-syntax-class-space #'stx-class) (lambda () #f)))
       (define (compat pack*)
         (define sc (rhombus-syntax-class-class rsc))
         (define temp-id (car (generate-temporaries (list #'id))))
         (define-values (attribute-bindings attribute-mappings)
           (for/lists (bindings mappings)
                      ([attr (rhombus-syntax-class-attributes rsc)]
                       [temp-attr (generate-temporaries (rhombus-syntax-class-attributes rsc))])
             (define id-with-attr
               (datum->syntax #'id (string->symbol (format "~a.~a" (syntax-e #'id) attr))))
             (values #`[#,temp-attr (#,pack* (syntax #,id-with-attr) 0)]
                     (cons attr temp-attr))))
         (define pack-depth (if (rhombus-syntax-class-built-in? rsc) 0 1))
         (values (if sc
                     #`(~var id #,sc)
                     #'id)
                 (cons #`[#,temp-id (#,pack* (syntax id) #,pack-depth)] attribute-bindings)
                 (list #`[id (make-pattern-variable-syntax
                              (quote-syntax #,temp-id)
                              (hash #,@(apply append (for/list ([b (in-list attribute-mappings)])
                                                       (list #`(quote #,(car b)) #`(quote-syntax #,(cdr b)))))))])))
       (define (incompat)
         (raise-syntax-error #f
                             "unknown syntax class or incompatible with this context"
                             in-e
                             #'stx-class))
       (cond
         [(not (rhombus-syntax-class? rsc))
          (raise-syntax-error #f
                              "not bound as a syntax class"
                              in-e
                              #'stx-class)]
         [(eq? (rhombus-syntax-class-kind rsc) 'term)
          (cond
            [(not (eq? kind 'term))
             (values #f #f #f)]
            [else (compat pack*)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'group)
          (cond
            [(eq? kind 'term) (incompat)]
            [(not (eq? kind 'group)) (values #f #f #f)]
            [else (compat pack*)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'multi)
          (cond
            [(eq? kind 'multi) (compat pack*)]
            [else (incompat)])]
         [(eq? (rhombus-syntax-class-kind rsc) 'block)
          (cond
            [(and (eq? kind 'multi) (syntax-parse in-e
                                      [(head . _) (memq (syntax-e #'head) '(block alts))]))
             (compat #'pack-block*)]
            [else (incompat)])]
         [else
          (error "unrecognized kind" kind)])]))
  (define (handle-escape/match-head $-id e in-e pack* context-syntax-class kind)
    (define-values (p idrs sidrs) (handle-escape $-id e in-e pack* context-syntax-class kind))
    (if p
        (values (syntax-parse in-e
                  [(tag . _) #`(~and ((~datum tag) . _) #,p)])
                idrs
                sidrs)
        (values #f #f #f)))
  (convert-syntax e
                  #:as-tail? as-tail?
                  #:splice? splice?
                  ;; make-datum
                  (lambda (d)
                    #`(~datum #,d))
                  ;; make-literal
                  (lambda (d)
                    #`(~literal #,d))
                  ;; handle-escape:
                  (lambda ($-id e in-e)
                    (handle-escape $-id e in-e #'pack-term* #'Term 'term))
                  ;; handle-group-escape:
                  (lambda ($-id e in-e)
                    (handle-escape/match-head $-id e in-e #'pack-group* #'Group 'group))
                  ;; handle-multi-escape:
                  (lambda ($-id e in-e)
                    (handle-escape/match-head $-id e in-e #'pack-tagged-multi* #'Multi 'multi))
                  ;; deepen-escape
                  (lambda (idr)
                    (syntax-parse idr
                      [(id (pack (_ stx) depth))
                       #`(id (pack (syntax (stx (... ...))) #,(add1 (syntax-e #'depth))))]))
                  ;; handle-tail-escape:
                  (lambda (name e in-e)
                    (if (free-identifier=? e #'rhombus-_)
                        (values #'_ null null)
                        (values e (list #`[#,e (pack-tail* (syntax #,e) 0)]) null)))
                  ;; handle-block-tail-escape:
                  (lambda (name e in-e)
                    (values e (list #`[#,e (pack-multi* (syntax #,e) 0)]) null))
                  ;; handle-maybe-empty-sole-group
                  (lambda (tag pat idrs sidrs)
                    ;; `pat` matches a `group` form that's supposed to be under `tag`,
                    ;; but if `pat` match `(group)`, then allow an overall match to `(tag)`
                    (values #`(~or* ((~datum #,tag) #,pat)
                                    (~and ((~datum #,tag))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,(syntax-parse pat
                                                   [(_ . tail) #'tail]))))
                            idrs
                            sidrs
                            #f))
                  ;; handle-maybe-empty-alts
                  (lambda (tag ps idrs sidrs)
                    ;; if `(tag . ps)` would match `(alts)`, then let it match `(block)`
                    (values #`(~or* ((~datum #,tag) . #,ps)
                                    (~and ((~datum block))
                                          ;; sets all pattern variables to nested empties:
                                          (_ . #,ps)))
                            idrs
                            sidrs
                            #t))
                  ;; handle-maybe-empty-group
                  (lambda (tag ps idrs sidrs)
                    ;; the `(tag . ps)` could match `(group)`, but it just never will,
                    ;; because that won't be an input
                    (values #`((~datum #,tag) . #,ps) idrs sidrs #t))))


(define-for-syntax (convert-template e
                                     #:check-escape [check-escape (lambda (e) (void))]
                                     #:rhombus-expression [rhombus-expression #'rhombus-expression])
  (define-values (template idrs sidrs can-be-empty?)
    (convert-syntax e
                    #:tail-any-escape? #t
                    ;; make-datum
                    (lambda (d) d)
                    ;; make-literal
                    (lambda (d) d)
                    ;; handle-escape:
                    (lambda ($-id e in-e)
                      ; TODO fix how check-escape works
                      #;(check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-term* (quote-syntax #,$-id) (#,rhombus-expression (group #,e)) 0)]) null))
                    ;; handle-group-escape:
                    (lambda ($-id e in-e)
                      #;(check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-group* (quote-syntax #,$-id) (#,rhombus-expression (group #,e)) 0)]) null))
                    ;; handle-multi-escape:
                    (lambda ($-id e in-e)
                      #;(check-escape e)
                      (define id (car (generate-temporaries (list e))))
                      (with-syntax ([(tag . _) in-e])
                        (values #`(tag . #,id) (list #`[#,id (unpack-multi* (quote-syntax #,$-id) (#,rhombus-expression (group #,e)) 0)]) null)))
                    ;; deepen-escape
                    (lambda (idr)
                      (syntax-parse idr
                        #:literals (unpack-term* unpack-group* unpack-multi* unpack-tail*)
                        [(id-pat ((~and unpack (~or unpack-term* unpack-group* unpack-multi* unpack-tail*)) q e depth))
                         #`[(id-pat (... ...)) (unpack q e #,(add1 (syntax-e #'depth)))]]
                        [(id-pat (converter depth (qs t) . args))
                         #`[(id-pat (... ...)) (converter #,(add1 (syntax-e #'depth)) (qs (t (... ...)))) . args]]))
                    ;; handle-tail-escape:
                    (lambda (name e in-e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-tail* '#,name (#,rhombus-expression (group #,e)) 0)]) null))
                    ;; handle-block-tail-escape:
                    (lambda (name e in-e)
                      (define id (car (generate-temporaries (list e))))
                      (values id (list #`[#,id (unpack-multi* '#,name (#,rhombus-expression (group #,e)) 0)]) null))
                    ;; handle-maybe-empty-sole-group
                    (lambda (tag template idrs sidrs)
                      ;; if `template` generates `(group)`, then instead of `(tag (group))`,
                      ;; produce `(tag)`
                      (define id (car (generate-temporaries '(group))))
                      (values #`(#,tag #,id (... ...))
                              (cons #`[(#,id (... ...))
                                       (convert-empty-group 0 (#,(quote-syntax quasisyntax) #,template))]
                                    idrs)
                              sidrs
                              #f))
                    ;; handle-maybe-empty-alts
                    (lambda (tag ts idrs sidrs)
                      ;; if `(tag . ts)` generates `(alts)`, then produce `(block)` instead
                      (define id (car (generate-temporaries '(alts))))
                      (values id
                              (cons #`[#,id (convert-empty-alts 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              sidrs
                              #f))
                    ;; handle-maybe-empty-group
                    (lambda (tag ts idrs sidrs)
                      ;; if `(tag . ts)` generates `(group)`, then error
                      (define id (car (generate-temporaries '(group))))
                      (values id
                              (cons #`[#,id (error-empty-group 0 (#,(quote-syntax quasisyntax) (#,tag . #,ts)))]
                                    idrs)
                              sidrs
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

(define-for-syntax (call-with-quoted-expression stx single-k multi-k literal-k)
  (syntax-parse stx
    #:datum-literals (quotes group op)
    #:literals (rhombus... $)
    [(_ (quotes (group (~and special (op (~or rhombus... $))))) . tail)
     (values (literal-k #'special)
             #'tail)]
    [(_ (quotes (group t)) . tail)
     (values (single-k #'t)
             #'tail)]
    [(_ ((~and tag quotes) . args) . tail)
     (values (multi-k (datum->syntax #f (cons (syntax/loc #'tag multi) #'args)))
             #'tail)]))

(define-for-syntax ((convert-pattern/generate-match repack-id) e)
  (define-values (pattern idrs sidrs can-be-empty?) (convert-pattern e))
  (with-syntax ([((id id-ref) ...) idrs]
                [((sid sid-ref) ...) sidrs])
    (with-syntax ([(tmp-id ...) (generate-temporaries #'(id ...))])
      (binding-form
       #'syntax-infoer
       #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
          #,pattern
          #,repack-id
          (tmp-id ...)
          (id ...)
          (id-ref ...)
          (sid ...)
          (sid-ref ...))))))

(define-syntax #%quote
  (make-expression+binding-prefix-operator
   #'#%quote
   '((default . stronger))
   'macro
   (lambda (stx)
     (call-with-quoted-expression stx
                                  convert-template
                                  convert-template
                                  (lambda (e) #`(quote-syntax #,e))))
   (lambda (stx)
     (call-with-quoted-expression stx
                                  (convert-pattern/generate-match #'repack-as-term)
                                  (convert-pattern/generate-match #'repack-as-multi)
                                  (lambda (e) (binding-form
                                               #'syntax-infoer
                                               #`(#,(string-append "'" (shrubbery-syntax->string e) "'")
                                                  #,(syntax-parse e
                                                      [((~datum op) id) #`((~datum op) (~literal id))])
                                                  repack-as-term
                                                  ()
                                                  ()
                                                  ())))))))

(define-syntax syntax_term
  (make-expression+binding-prefix-operator
   (quote-syntax syntax_term)
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group term)) . tail)
        (values (convert-template #'term) #'tail)]))
   ;; pattern
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(_ (parens (group term)) . tail)
        (values (convert-pattern/generate-match #'term) #'tail)]))))

(define-syntax (syntax-infoer stx)
  (syntax-parse stx
    [(_ static-infos (annotation-str pattern repack tmp-ids (id ...) id-refs (sid ...) sid-refs))
     (binding-info #'annotation-str
                   #'syntax
                   #'()
                   #'((id) ...)
                   #'syntax-matcher
                   #'syntax-binder
                   #'(pattern repack tmp-ids (id ...) id-refs (sid ...) sid-refs))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)) IF success fail)
     #'(IF (syntax? arg-id)
           (begin
             (define-values (match? tmp-id ...)
               (syntax-parse (repack arg-id)
                 [pattern (values #t id-ref ...)]
                 [_ (values #f 'id ...)]))
             (IF match?
                 success
                 fail))
           fail)]))

(define-syntax (syntax-binder stx)
  (syntax-parse stx
    [(_ arg-id (pattern repack (tmp-id ...) (id ...) (id-ref ...) (sid ...) (sid-ref ...)))
     #'(begin
         (define id tmp-id) ...
         (define-syntax sid sid-ref) ...)]))

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

