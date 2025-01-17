#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-parse.rkt"
                     "veneer-parse.rkt"))

(provide (for-syntax able-statinfo-indirect-id
                     able-super-statinfo-indirect-id
                     able-method-status
                     able-method-as-property
                     able-method-for-class-desc))

;; get an identifier whose static info describes <which>able instances as functions;
;; if the relevant <which> method is public or might be privately implemented
;; in this new class, we bind a fresh name to static info, since
;; we haven't parsed enough about the class to predict its implementation's name
(define-for-syntax (able-statinfo-indirect-id which super interfaces name-id intro)
  (or (and (or (and super
                    (memq which (objects-desc-flags super)))
               (for/or ([intf (in-list interfaces)])
                 (memq which (objects-desc-flags intf))))
           ;; => which method's info is relevant
           (intro (datum->syntax #f (string->symbol (format "~a-~a-indirect" (syntax-e name-id) which)))))
      (and (eq? which 'call)
           ;; need the designated identifier for callable info, because it
           ;; has different arity information than the implementing method:
           (able-super-statinfo-indirect-id which super interfaces))
      (and (class-desc? super)
           ;; => can refer directly to superclass's private implementation, since it can't be overridden here
           (case which
             [(call) (class-desc-call-method-id super)]
             [(get) (class-desc-index-method-id super)]
             [(set) (class-desc-index-set-method-id super)]
             [(append) (class-desc-append-method-id super)]
             [(compare) (class-desc-compare-method-id super)]
             [(contains) (class-desc-contains-method-id super)]
             [else (error "unknown able" which)]))))

;; gets public or private id, whatever is available to supply arity info in case
;; it's not overridden
(define-for-syntax (able-super-statinfo-indirect-id which super interfaces)
  (unless (eq? which 'call) (error 'able-super-statinfo-indirect-id "only handles 'call"))
  (or (and (class-desc? super)
           (class-desc-indirect-call-method-id super))
      (for/or ([intf (in-list interfaces)])
        (interface-desc-indirect-call-method-id intf))))

;; returns `(values here-<which>able? public-<which>able?)`:
;;   * `<which>able?` implies that a <which> method is declared or inherited, maybe abstract
;;   * `here-<which>able?` implies that a <which> method is declared or inherited and implements
;;     `<Which>able`; if #f, instances may still be callable through an inherited private implementation
;;   * `public-<which>able?` can be #f even if `here-<which>able?` if the <which> method is not public
(define-for-syntax (able-method-status which super interfaces method-mindex method-vtable method-private
                                       #:name [which-name which])
  (define is-able?
    (or (and super (memq which (objects-desc-flags super)))
        (for/or ([intf (in-list interfaces)])
          (memq which (objects-desc-flags intf)))))
  (values is-able?
          (and is-able?
               (or (hash-ref method-private which-name #f)
                   (let ([m (hash-ref method-mindex which-name #f)])
                     (and m
                          (not (eq? '#:abstract (vector-ref method-vtable (mindex-index m))))))))
          (and is-able?
               (not (hash-ref method-private which #f)))))

(define-for-syntax (able-method-as-property which prop:whichable-id able?
                                            method-mindex method-vtable method-private
                                            #:name [which-name which])
  (cond
    [able?
     (cond
       [(hash-ref method-private which-name #f)
        => (lambda (which-id)
             (list #`(cons #,prop:whichable-id #,which-id)))]
       [else
        (define midx (hash-ref method-mindex which-name))
        (list #`(cons #,prop:whichable-id
                      #,(vector-ref method-vtable (mindex-index midx))))])]
    [else null]))

(define-for-syntax (able-method-for-class-desc which
                                               able? public-able?
                                               super
                                               method-mindex method-vtable method-private
                                               #:const [const #f])
  (cond
    [(and able? (not public-able?))
     #`(quote-syntax #,(cond
                         [const const]
                         [(hash-ref method-private which #f)
                          => (lambda (which-id) which-id)]
                         [else
                          (unless super (error "cannot find private <which> method"))
                          (case which
                            [(call) (class-desc-call-method-id super)]
                            [(get) (class-desc-index-method-id super)]
                            [(set) (class-desc-index-set-method-id super)]
                            [(append) (class-desc-append-method-id super)]
                            [(compare) (class-desc-compare-method-id super)]
                            [(contains) (class-desc-contains-method-id super)]
                            [else (error "unknown able" which)])]))]
    [else #'#f]))
