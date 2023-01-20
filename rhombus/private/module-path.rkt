#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/name-root
                     "srcloc.rkt"
                     "introducer.rkt"
                     "name-path-op.rkt"
                     "realm.rkt")
         "provide.rkt"
         (only-in "implicit.rkt"
                  #%literal)
         (only-in "arithmetic.rkt"
                  [/ rhombus/])
         "name-root.rkt")

;; The syntax of module paths is not meant to be extensible, but it
;; may be useful to expose `:module-path` parsing. For imports, we
;; bind module-path operators in the same space as import operators,
;; so that they can be mixed more freely. For exports, we distinguish
;; module paths and export operators.

(provide (for-space rhombus/modpath
                    #%literal
                    (rename-out [rhombus/ /]
                                [rhombus-! !]
                                [rhombus-file file]
                                [rhombus-lib lib])))

(module+ for-import-export
  (provide (for-syntax make-module-path-literal-operator
                       make-module-path-/-operator
                       make-module-path-file-operator
                       make-module-path-lib-operator
                       make-module-path-submod-operator

                       :module-path

                       current-module-path-context

                       convert-symbol-module-path)))

(module+ for-meta
  (provide (for-syntax in-module-path-space)
           (for-space rhombus/namespace
                      modpath)))

(begin-for-syntax
  (property module-path-prefix-operator prefix-operator)
  (property module-path-infix-operator infix-operator)

  (define in-module-path-space (make-interned-syntax-introducer/add 'rhombus/modpath))

  (define current-module-path-context (make-parameter 'import))

  (define (check-module-path-result form proc)
    (unless (and (syntax? form)
                 (module-path? (syntax->datum form)))
      (raise-result-error* (proc-name proc) rhombus-realm "Module_Path_Syntax" form))
    form)

  (define (make-identifier-module-path id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error (current-module-path-context)
                          "not a valid module path element"
                          id))
    id)

  (define-enforest
    #:syntax-class :module-path
    #:desc "module path"
    #:operator-desc "module-path operator"
    #:in-space in-module-path-space
    #:name-path-op #f
    #:prefix-operator-ref module-path-prefix-operator-ref
    #:infix-operator-ref module-path-infix-operator-ref
    #:check-result check-module-path-result
    #:make-identifier-form make-identifier-module-path))

(define-for-syntax (convert-symbol-module-path mp)
  (cond
    [(identifier? mp)
     (define str (symbol->string (syntax-e mp)))
     (define name (if (regexp-match? #rx"/" str)
                      (string-append str ".rhm")
                      (string-append str "/main.rhm")))
     (datum->syntax mp `(,#'lib ,name) mp mp)]
    [else mp]))

(define-syntax (define-module-path-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-module-path-space #'name) rhs))]))

(define-for-syntax (make-module-path-literal-operator prefix-operator)
  (prefix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ a . tail)
        (unless (module-path? (syntax->datum #'a))
          (raise-syntax-error (current-module-path-context)
                              "not a valid module path"
                              #'a))
        (values #'a
                #'tail)]))))

(define-module-path-syntax #%literal
  (make-module-path-literal-operator module-path-prefix-operator))

(define-for-syntax (make-module-path-/-operator infix-operator)
  (infix-operator
   #'rhombus/
   '((default . stronger))
   'macro
   (lambda (form1 stx)
     (unless (identifier? form1)
       (raise-syntax-error (current-module-path-context)
                           "not a valid module path element"
                           form1))
     (syntax-parse stx
       [(_ a . tail)
        (unless (and (identifier? #'a)
                     (module-path? (syntax->datum #'a)))
          (raise-syntax-error (current-module-path-context)
                              "not a valid module path element"
                              #'a))
        (values (datum->syntax #'a
                               (string->symbol
                                (format "~a/~a"
                                        (syntax-e form1)
                                        (syntax-e #'a)))
                               (span-srcloc form1 #'a)
                               #'a)
                #'tail)]))
   'left))

(define-module-path-syntax rhombus/
  (make-module-path-/-operator module-path-infix-operator))

(define-for-syntax (make-module-path-string-arg-operator prefix-operator name mp-form-id check)
  (prefix-operator
   name
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group)
       [(form-id ((~and tag parens) (group str:string)) . tail)
        (define new-str (check #'str))
        (values (datum->syntax #'str
                               (list mp-form-id new-str)
                               (span-srcloc #'form-id #'tag)
                               #'form-id)
                #'tail)]))))

(define-for-syntax (make-module-path-file-operator prefix-operator)
  (make-module-path-string-arg-operator
   prefix-operator #'rhombus-file #'file
   (lambda (str)
     (unless (path-string? (syntax->datum str))
       (raise-syntax-error (current-module-path-context)
                           "not a valid path"
                           str))
     str)))

(define-module-path-syntax rhombus-file
  (make-module-path-file-operator module-path-prefix-operator))

(define-for-syntax (make-module-path-lib-operator prefix-operator)
  (make-module-path-string-arg-operator
   prefix-operator #'rhombus-lib #'lib
   (lambda (str)
     (define (maybe-add-rhm-suffix s)
       (if (regexp-match? #rx"[.]" s) s (string-append s ".rhm")))
     (define new-str (maybe-add-rhm-suffix (syntax->datum str)))
     (unless (module-path? `(lib ,new-str))
       (raise-syntax-error (current-module-path-context)
                           "not a valid library path"
                           str))
     new-str)))

(define-module-path-syntax rhombus-lib
  (make-module-path-lib-operator module-path-prefix-operator))

(define-for-syntax (make-module-path-submod-operator infix-operator)
  (infix-operator
   #'rhombus-!
   '((default . stronger))
   'macro
   (lambda (mp stx)
     (unless (module-path? (syntax->datum mp))
       (raise-syntax-error (current-module-path-context)
                           "not a valid submodule prefix"
                           mp))
     (syntax-parse stx
       #:datum-literals ()
       [(form-id id:identifier . tail)
        (values (datum->syntax #'id
                               (list #'submod mp #'id)
                               (span-srcloc #'form-id #'tag)
                               #'form-id)
                #'tail)]))
   'left))

(define-module-path-syntax rhombus-!
  (make-module-path-submod-operator module-path-infix-operator))

;; Defines a fake meta namespace that would be exposed if we allowed
;; new module-path forms, needed to document the built-in ones
(define-name-root modpath
  #:fields ([macro modpath-macro]))
(define-syntax modpath-macro 'placeholder)
