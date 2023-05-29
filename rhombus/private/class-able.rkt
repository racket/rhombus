#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-parse.rkt"))

(provide (for-syntax able-statinfo-indirect-id
                     able-method-status
                     able-method-as-property
                     able-method-for-class-desc))

;; get an identifier whose static info describes <which>able instances as functions;
;; if the relevant <which> method is public or might be privately implemented
;; in this new class, we bind a fresh name to static info, since
;; we haven't parsed enough about the class to predict its implementation's name
(define-for-syntax (able-statinfo-indirect-id which super interfaces name-id intro)
  (or (and (or (and super
                    (memq which (class-desc-flags super)))
               (for/or ([intf (in-list interfaces)])
                 (memq which (interface-desc-flags intf))))
           ;; => which method's info is relevant
           (intro (datum->syntax #f (string->symbol (format "~a-~a-indirect" (syntax-e name-id) which)))))
      (and super
           ;; => can refer directly to superclass's private implementation, since it can't be overridden here
           (case which
             [(call) (class-desc-call-method-id super)]
             [(get) (class-desc-index-method-id super)]
             [(set) (class-desc-index-set-method-id super)]
             [else (error "unknown able")]))))

;; returns `(values here-<which>able? public-<which>able?)`:
;;   * `<which>able?` implies that a <which> method is declared or inherited, maybe abstract
;;   * `here-<which>able?` implies that a <which> method is declared or inherited and implements
;;     `<Which>able`; if #f, instances may still be callable through an inherited private implementation
;;   * `public-<which>able?` can be #f even if `here-<which>able?` if the <which> method is not public
(define-for-syntax (able-method-status which super interfaces method-mindex method-vtable method-private)
  (define is-able?
    (or (and super (memq which (class-desc-flags super)))
        (for/or ([intf (in-list interfaces)])
          (memq which (interface-desc-flags intf)))))
  (values is-able?
          (and is-able?
               (or (hash-ref method-private which #f)
                   (let ([m (hash-ref method-mindex which #f)])
                     (and m
                          (not (eq? '#:abstract (vector-ref method-vtable (mindex-index m))))))))
          (and is-able?
               (not (hash-ref method-private which #f)))))

(define-for-syntax (able-method-as-property which prop:whichable-id able?
                                            method-mindex method-vtable method-private)
  (cond
    [able?
     (cond
       [(hash-ref method-private which #f)
        => (lambda (which-id)
             (list #`(cons #,prop:whichable-id #,which-id)))]
       [else
        (define midx (hash-ref method-mindex which))
        (list #`(cons #,prop:whichable-id
                      #,(vector-ref method-vtable (mindex-index midx))))])]
    [else null]))

(define-for-syntax (able-method-for-class-desc which
                                               able? public-able?
                                               super
                                               method-mindex method-vtable method-private)
  (cond
    [(and able? (not public-able?))
     #`(quote-syntax #,(cond
                         [(hash-ref method-private which #f)
                          => (lambda (which-id) which-id)]
                         [else
                          (unless super (error "cannot find private <which> method"))
                          (case which
                            [(call) (class-desc-call-method-id super)]
                            [(get) (class-desc-index-method-id super)]
                            [(set) (class-desc-index-set-method-id super)]
                            [else (error "unknown able")])]))]
    [else #'#f]))
