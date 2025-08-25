#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         (submod "annotation.rkt" for-class)
         "define-arity.rkt"
         "name-root.rkt"
         "function-arity-key.rkt"
         "call-result-key.rkt"
         "definition.rkt"
         "parens.rkt"
         "static-info.rkt"
         (submod "function.rkt" for-info)
         (submod "equal.rkt" for-parse)
         "dotted-sequence-parse.rkt"
         "parse.rkt"
         "extract-name.rkt"
         "name-prefix.rkt"
         "call-result-key.rkt"
         "realm.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Parameter))

(module+ for-info
  (provide (for-syntax get-parameter-static-infos)))

(define-static-info-getter get-parameter-static-infos
  (#%function-arity 3)
  . #,(get-function-static-infos))

(define/arity (Parameter.make v
                              #:guard [guard #f]
                              #:name [name 'parameter]
                              #:realm [realm rhombus-realm])
  #:static-infos ((#%call-result #,(get-parameter-static-infos)))
  (make-parameter v guard name realm))

(define-name-root Parameter
  #:fields
  ([make Parameter.make]
   [def Parameter.def]))

(define-annotation-syntax Parameter
  (identifier-annotation parameter? #,(get-parameter-static-infos)))

(define-syntax Parameter.def
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        [(_ any ...+ _::equal rhs ...+)
         (check-multiple-equals stx)
         (build-parameter-definition #'(any ...) #'(rhombus-expression (group rhs ...)) stx #f name-prefix)]
        [(_ any ...+ (b-tag::block
                      (~optional (~and name-option (group #:name . _)))
                      g ...))
         (build-parameter-definition #'(any ...) #'(rhombus-body-at b-tag g ...) stx (attribute name-option) name-prefix)]))))

(define-for-syntax (build-parameter-definition lhs rhs stx name-option name-prefix)
  (with-syntax ([(name extends converter annotation-str static-infos)
                 (syntax-parse (respan lhs)
                   [(name::dotted-identifier-sequence annot::inline-annotation)
                    (syntax-parse #'name
                      [name::dotted-identifier
                       (list #'name.name #'name.extends
                             #'annot.converter #'annot.annotation-str #'annot.static-infos)])]
                   [name::dotted-identifier
                    (list #'name.name #'name.extends #f #f #'())])])
    (with-syntax ([reflect-name (or (extract-name name-option stx)
                                    (add-name-prefix name-prefix #'name))])
      (append
       (build-definitions/maybe-extension
        #f #'name #'extends
        #`(make-parameter #,rhs
                          #,(if (syntax-e #'converter)
                                #`(lambda (v)
                                    (converter v 'name (lambda (v who)
                                                         (raise-annotation-failure who v 'annotation-str))))
                                #f)
                          'reflect-name
                          rhombus-realm))
       (list (if (static-infos-empty? #'static-infos)
                 #'(define-static-info-syntax name
                     #:getter get-parameter-static-infos)
                 #'(define-static-info-syntax name
                     (#%call-result (#:at_arities
                                     ((1 static-infos)
                                      (2 ()))))
                     . #,(get-parameter-static-infos))))))))
