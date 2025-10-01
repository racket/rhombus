#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parse.rkt"
         "class-clause-tag.rkt"
         "sentinel-declaration.rkt")

(provide define-class-body-step)

(define-syntax-rule (define-class-body-step class-body-step
                      :class-clause
                      class-clause?
                      class-expand-data
                      class-clause-accum)
  (...
   (begin
     (define-syntax class-body-step
       (lambda (stx)
         (syntax-parse stx
           [(_ data+accum) #'(begin)]
           [(_ data+accum . forms)
            ;; If there are no declaration or clause forms left, then it's
            ;; expressions and definitions that we delay until after the
            ;; class is expanded. Those definitions might be exported, but
            ;; maybe they want to refer to the class annotation, or even
            ;; its namespace.
            #:when (ormap (lambda (e) (or (nestable-declaration? e)
                                          (class-clause? e)))
                          (syntax->list #'forms))
            #'(class-body-step/to-clause-or-decl data+accum . forms)]
           [(_ ([orig-stx base-stx scope-stx reflect-name effect-id . _] . _) . forms)
            #'(quote-syntax (rhombus-class (#:post-forms ((rhombus-nested
                                                           reflect-name
                                                           effect-id
                                                           . forms))))
                            #:local)])))
     (define-syntax class-body-step/to-clause-or-decl
       (lambda (stx)
         ;; parse the first form as a class clause, if possible, otherwise assume
         ;; an expression or definition
         (syntax-parse stx
           [(_ (data accum) form . rest)
            #:with (~var clause (:class-clause (class-expand-data #'data #'accum))) (syntax-local-introduce #'form)
            (syntax-parse (syntax-local-introduce #'clause.parsed)
              #:datum-literals (group parsed)
              [((group (parsed #:rhombus/class_clause p)) ...)
               #:with (new-accum ...) (class-clause-accum #'(p ...))
               #`(begin p ... (class-body-step (data (new-accum ... . accum)) . rest))]
              [(form ...)
               #`(class-body-step (data accum) form ... (group sentinel_declaration) . rest)])]
           [(_ (~and data+accum ([orig-stx base-stx scope-stx reflect-name effect-id . _] . _)) form . rest)
            #`(rhombus-top-step
               #,(if (nestable-declaration? #'form)
                     #'class-body-step
                     #'class-body-step/to-clause-or-decl)
               #f
               reflect-name
               effect-id
               (data+accum)
               form . rest)]
           [(_ data+accum) #'(begin)]))))))
