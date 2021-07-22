#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/provide-transform
                     "format-id.rkt")
         racket/provide-syntax)

(provide property)

;; Defines:
;;   `prop:name` - a property whose value should be a procedure
;;                 that takes an instance of the property
;;   `name?` - detects instances of `prop:name`
;;   `name-ref` - uses the property value on the instance, returns
;;                #f if `name?` would return #f
;;   `name` - convenience constructor for an instance of `prop:name`
(define-syntax (property stx)
  (syntax-parse stx
    [(_ name base-impl (~optional (~seq #:super prop-super)))
     (with-syntax ([prop:name (format-id "prop:~a" #'name)]
                   [name-ref (format-id "~a-ref" #'name)]
                   [name? (format-id "~a?" #'name)]
                   [convenience-name (car (generate-temporaries (list #'name)))]
                   [super-spec (if (attribute prop-super)
                                   #'(list (cons prop-super values))
                                   #'null)])
       #'(begin
           (define-values (prop:name name? ref)
             (make-struct-type-property 'name #f super-spec))
           (define (name-ref v)
             (define acc (ref v (lambda () #f)))
             (and acc (acc v)))
           (struct convenience-name base-impl ()
             #:property prop:name (lambda (self) self)
             #:reflection-name 'name)
           (define name convenience-name)))]))
