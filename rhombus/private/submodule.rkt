#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (submod "module-path.rkt" for-import-export)
         "declaration.rkt"
         "parens.rkt")

(provide submodule)

(define-syntax submodule
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ name:identifier (_::block body ...))
        (list (datum->syntax
               #'name
               (syntax-e
                #`(rhombus-module+ name body ...))))]
       [(_ name:identifier #:lang mp ...+ (_::block body ...))
        #:with lang::module-path #'(group mp ...)
        #`((module name lang.parsed
             (#,(datum->syntax #'lang.parsed '#%module-begin)
              (top body
                   ...))))]
       [(_ (~optional (~and order (~or #:late #:early))
                      #:defaults ([order #'#:early]))
           name:identifier
           #:lang mp ...+ (_::block body ...))
        #:with lang::module-path #'(group mp ...)
        #:with module (if (eq? (syntax-e #'order) '#:late) #'module* #'module)
        #`((module name lang.parsed
             (#,(datum->syntax #'lang.parsed '#%module-begin)
              (top body
                   ...))))]))))

;; ----------------------------------------

;; It would be better if `module+` somehow supported the body-wrapping
;; step that we need for Rhombus. For now, this is mostly a copy of
;; `module+`.

(define-syntaxes (rhombus-module+)
  (lambda (stx)
    (unless (eq? (syntax-local-context) 'module)
      (error "a `rhombus-module+` was somehow misplaced"))
    (syntax-case stx ()
      [(_ the-submodule e ...)
       (begin
         (unless (symbol? (syntax-e #'the-submodule))
           (raise-syntax-error #f
                               "expected an identifier for a submodule, found something else"
                               stx
                               #'the-submodule))
         ;; This looks it up the first time and is allowed to create a
         ;; list and lift a module-end declaration if necessary:
         (let ([stxs-box (get-stxs-box stx #'the-submodule #t)])
           (set-box! stxs-box
                     (cons (append (reverse (syntax->list (syntax-local-introduce #'(e ...))))
                                   (car (unbox stxs-box)))
                           (cons (syntax-local-introduce stx) (cdr (unbox stxs-box))))))
         (syntax/loc stx (begin)))])))

(begin-for-syntax
  ;; The following table is newly instantiated for each module
  ;; expansion that uses `module+', so it is effectively
  ;; module-local:
  (define-values (submodule->stxs-box) (make-weak-hash))
  (define-values (get-stxs-box)
    (lambda (form-stx the-submodule-stx lift?)
      (hash-ref! submodule->stxs-box (syntax-e the-submodule-stx)
                 (lambda ()
                   (when lift?
                     (syntax-local-lift-module-end-declaration
                      ;; Use the lexical context of the first `module+'
                      ;; form as the context of the implicit `#%module-begin':
                      (datum->syntax
                       form-stx
                       (list #'define-module the-submodule-stx)
                       form-stx)))
                   (box (cons null null)))))))

;; A use of this form is lifted to the end of the enclosing module
;; for each submodule created by `module+':
(define-syntaxes (define-module)
  (lambda (stx)
    (syntax-case stx ()
      [(_ the-submodule)
       (let* ([stxs-box (get-stxs-box #f #'the-submodule #f)]
              ;; Propagate the lexical context of the first `module+'
              ;; for the implicit `#%module-begin':
              [module-decl
               (datum->syntax
                stx
                (list
                 #'module* 
                 #'the-submodule 
                 #f ; namespace context is the original context
                 (list
                  '#%module-begin
                  (cons
                   'top
                   (map syntax-local-introduce (reverse (car (unbox stxs-box)))))))
                stx)])
         ;; Add 'origin and copy properties for every original declaration
         (let loop ([stx module-decl]
                    [origs (cdr (unbox stxs-box))])
           (if (null? origs) stx
               (let* ([orig (car origs)]
                      [id-stx (if (symbol? (syntax-e orig)) orig
                                  (car (syntax-e orig)))])
                 (loop (syntax-track-origin stx orig id-stx)
                       (cdr origs))))))])))
