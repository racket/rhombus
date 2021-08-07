#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/transformer
                     enforest/ref-parse
                     enforest/name-ref-parse
                     enforest/proc-name
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "srcloc.rkt")
         "declaration.rkt"
         (submod "module-path.rkt" for-import-export)
         (submod "import.rkt" for-export))

(provide export

         (for-space rhombus/export
                    rename
                    except
                    names
                    all_from
                    all_in
                    |.|
                    #%juxtapose))

(begin-for-syntax
  (property export-prefix-operator prefix-operator)
  (property export-infix-operator infix-operator)

  (property export-modifier transformer)

  (define in-export-space (make-interned-syntax-introducer 'rhombus/export))

  (define (check-export-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define (make-identifier-export id)
    (define for-spaces
      (for*/list ([sym (in-list (syntax-local-module-interned-scope-symbols))]
                  [(intro) (in-value (make-interned-syntax-introducer sym))]
                  [(space-id) (in-value (intro id))]
                  #:when (and (not (free-identifier=? id space-id))
                              (identifier-binding space-id)))
        #`(for-space #,sym #,id)))
    (cond
      [(identifier-binding id)
       (if (null? for-spaces)
           id
           #`(combine-out #,id . #,for-spaces))]
      [(null? for-spaces) id]
      [else #`(combine-out . #,for-spaces)]))

  (define-enforest
    #:syntax-class :export
    #:infix-more-syntax-class :export-infix-op+form+tail
    #:desc "export"
    #:operator-desc "export operator"
    #:in-space in-export-space
    #:name-path-op name-path-op
    #:prefix-operator-ref export-prefix-operator-ref
    #:infix-operator-ref export-infix-operator-ref
    #:check-result check-export-result
    #:make-identifier-form make-identifier-export
    #:make-operator-form make-identifier-export)

  (define (make-export-modifier-ref transform-in ex)
    ;; "accessor" closes over `ex`:
    (lambda (v)
      (define mod (export-modifier-ref v))
      (and mod
           (transformer (lambda (stx)
                          ((transformer-proc mod) (transform-in ex) stx))))))

  (define-transform
    #:syntax-class (:export-modifier req)
    #:desc "export modifier"
    #:in-space in-export-space
    #:name-path-op name-path-op
    #:transformer-ref (make-export-modifier-ref transform-in req))

  (define (apply-modifiers mods e-parsed)
    (cond
      [(null? mods) e-parsed]
      [else
       (syntax-parse (car mods)
         #:datum-literals (group)
         [(~var ex (:export-modifier e-parsed))
          (apply-modifiers (cdr mods) #'ex.parsed)]
         [(group form . _)
          (raise-syntax-error #f
                              "not an export modifier"
                              #'form)])])))

(define-syntax export
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block e::export ...))
        #`((provide e.parsed ...))]))))

(define-syntax (define-export-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-export-space #'name) rhs))]))

(begin-for-syntax
  (define-syntax-class :renaming
    #:datum-literals (group)
    (pattern (group . (~var int (:name-ref-seq values name-path-op)))
             #:with (#:to ext::reference) #'int.tail
             #:attr int-name #'int.name
             #:attr ext-name #'ext.name)))

(define-export-syntax rename
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block r::renaming ...)
           . tail)
        (values #`(rename-out [r.int-name r.ext-name] ...)
                #'tail)]))))

(define-export-syntax except
  (export-modifier
   (lambda (ex stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block e::export ...))
        #`(except-out #,ex e.parsed ...)]))))
     
(define-export-syntax names
  (export-prefix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group ref::reference ...) ...)
           . tail)
        (values #`(combine-out ref.name ... ...)
                #'tail)]))))

(define-export-syntax all_from
  (export-prefix-operator
   #'all_from
   '((default . stronger))
   'macro
   (lambda (stx)
     (parameterize ([current-module-path-context 'export])
       (syntax-parse stx
         #:datum-literals (parens group)
         [(_ (parens mod-path::module-path)
             . tail)
          (values #`(all-from-out mod-path.parsed)
                  #'tail)])))))

(define-export-syntax all_in
  (export-prefix-operator
   #'all_from
   '((default . stronger))
   'macro
   (lambda (stx)
     (parameterize ([current-module-path-context 'export])
       (syntax-parse stx
         #:datum-literals (parens group)
         [(form-id (~and arg ((~and tag parens) (group id:identifier)))
                   . tail)
          (define v (syntax-local-value* #'id import-name-root-ref))
          (unless v
            (raise-syntax-error #f
                                "not an import name"
                                (datum->syntax #f
                                               (list #'form-id #'arg)
                                               (span-srcloc #'form-id #'tag))))
          (values #`(all-from-out #,(relocate #'id
                                              (import-name-root-module-path v)))
                  #'tail)])))))

(define-export-syntax #%juxtapose
  (export-infix-operator
   #'#%juxtapose
   '((default . weaker))
   'macro
   (lambda (form1 stx)
     (syntax-parse stx
       #:datum-literals (block group)
       [(_ (block mod ...) . tail)
        (values (apply-modifiers (syntax->list #'(mod ...))
                                 form1)
                #'tail)]
       [e::export-infix-op+form+tail
        (values #`(combine-out #,form1
                               e.parsed)
                #'e.tail)]))
   'left))

(define-export-syntax |.|
  (export-infix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (form stx)
     (syntax-parse stx
       #:datum-literals (op)
       [((op form-id) . _)
        (raise-syntax-error #f
                            "allowed here only as a name-path separator, used as an operator"
                            #'form-id)]))
   'left))
