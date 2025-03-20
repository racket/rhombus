#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-method-result.rkt")
         "to-list.rkt"
         "provide.rkt"
         "printer-property.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "define-arity.rkt"
         "call-result-key.rkt"
         (submod "parameter.rkt" for-info)
         "number.rkt"
         "static-info.rkt"
         "print-desc.rkt"
         (submod "print.rkt" for-printable)
         (submod "print.rkt" redirect)
         "enum.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/namespace)
                     Printable)
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     PrintDesc))

(define-values (prop:Printable Printable? Printable-ref)
  (make-struct-type-property 'Printable
                             #f
                             (list (cons prop:printer
                                         (lambda (v) bounce-to-printer-interface)))))

(define (bounce-to-printer-interface v mode recur)
  (define pd ((vector-ref (Printable-ref v) 0) v mode recur))
  ;; guarded by method result
  (print-description-unwrap #f pd))

(define-class-desc-syntax Printable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&describe)
                     #'#(#:abstract)
                     (hasheq 'describe 0)
                     (hasheq 'describe #'describe-result)
                     '()
                     #f
                     #'()
                     '()
                     ;; --------------------
                     #'Printable
                     #'Printable
                     #'prop:Printable
                     #'prop:Printable
                     #'Printable-ref
                     #'Printable-ref
                     #t
                     #f
                     null))))

(define-syntax describe-result
  (method-result-maker
   (lambda ()
     (method-result #'print-description? #t 1 "PrintDesc" #'() 8))))

(define-name-root Printable
  #:fields
  ([describe Printable.describe]
   [render Printable.render]
   [current_page_width current-page-width]
   [current_graph print-graph]
   [current_pretty current-print-as-pretty]
   [current_optimal current-pretty-as-optimal]))

(define-name-root PrintDesc
  #:fields
  ([concat PrintDesc.concat]
   [newline PrintDesc.newline]
   [nest PrintDesc.nest]
   [align PrintDesc.align]
   [or PrintDesc.or]
   [flat PrintDesc.flat]
   [list PrintDesc.list]
   [block PrintDesc.block]
   [special PrintDesc.special]
   SpecialMode))

(define-annotation-syntax PrintDesc
  (identifier-annotation print-description? ()))

(define (print-description? pd)
  (or (PrintDesc? pd)
      (string? pd)
      (bytes? pd)))

(define (print-description-unwrap who pd)
  (cond
    [(PrintDesc? pd) (PrintDesc-doc pd)]
    [(string? pd) (pretty-text pd)]
    [(bytes? pd) (pretty-text pd)]
    [else (and who
               (raise-annotation-failure who pd "PrintDesc"))]))

(define (check-int who n)
  (unless (exact-integer? n)
    (raise-annotation-failure who n "Int")))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-annotation-failure who n "NonnegInt")))

(define/arity (PrintDesc.concat . pds)
  (PrintDesc
   (pretty-concat-list
    (for/list ([pd (in-list pds)])
      (print-description-unwrap who pd)))))

(define/arity (PrintDesc.newline)
  (PrintDesc (pretty-newline)))

(define/arity (PrintDesc.nest n pd)
  (check-int who n)
  (PrintDesc
   (pretty-nest n (print-description-unwrap who pd))))

(define/arity (PrintDesc.align pd)
  (PrintDesc
   (pretty-align (print-description-unwrap who pd))))

(define/arity (PrintDesc.or pd1 pd2)
  (PrintDesc
   (pretty-or (print-description-unwrap who pd1)
              (print-description-unwrap who pd2))))

(define/arity (PrintDesc.flat pd)
  (PrintDesc
   (pretty-flat (print-description-unwrap who pd))))

(define/arity (PrintDesc.list pre elems post)
  (define pre-pd (print-description-unwrap who pre))
  (define (bad-elems)
    (raise-annotation-failure who elems "Listable.to_list && List.of(PrintDesc)"))
  (define elem-pds (cond
                     [(to-list #f elems)
                      => (lambda (elems)
                           (for/list ([elem (in-list elems)])
                             (or (print-description-unwrap #f elem)
                                 (bad-elems))))]
                     [else (bad-elems)]))
  (PrintDesc
   (pretty-listlike pre-pd
                    elem-pds
                    (print-description-unwrap who post))))

(define/arity (PrintDesc.block head body)
  (PrintDesc
   (pretty-blocklike (print-description-unwrap who head)
                     (print-description-unwrap who body))))

(define-simple-symbol-enum SpecialMode
  [write_special write-special]
  print
  write
  display)

(define/arity (PrintDesc.special v alt-pd
                                 #:mode [mode-in 'write_special]
                                 #:length [len 1])
  (define alt (print-description-unwrap who alt-pd))
  (define mode (->SpecialMode mode-in))
  (unless mode
    (raise-annotation-failure who mode-in "PrintDesc.SpecialMode"))
  (check-nonneg-int who len)
  (PrintDesc (pretty-special v len mode alt)))

(define-static-info-syntax current-page-width
  (#%call-result (#:at_arities
                  ((1 #,(get-int-static-infos))
                   (2 ()))))
  . #,(get-parameter-static-infos))
(define-static-info-syntaxes (print-graph
                              current-print-as-pretty
                              current-pretty-as-optimal)
  #:getter get-parameter-static-infos)

(define/arity (Printable.describe v
                                  #:mode [mode 'text]
                                  #:pretty [pretty? #t])
  (check-mode who mode)
  (parameterize ([current-print-as-pretty pretty?])
    (PrintDesc (pretty v mode (make-hasheq)))))

(define/arity (Printable.render pd
                                [op (current-output-port)]
                                #:column [column 0])
  (define doc (print-description-unwrap who pd))
  (check-output-port who op)
  (check-nonneg-int who column)
  (render-pretty doc op racket-print-redirect
                 #:column column))
