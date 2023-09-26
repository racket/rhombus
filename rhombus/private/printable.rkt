#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt")
         "provide.rkt"
         "annotation.rkt"
         "printer-property.rkt"
         "name-root.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "realm.rkt"
         "class-dot.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "define-arity.rkt"
         "static-info.rkt"
         "print-desc.rkt"
         (submod "print.rkt" for-printable))

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
  (print-description-unwrap 'Printable.print pd #t))

(define-class-desc-syntax Printable
  (interface-desc #'Printable
                  #'Printable
                  #'()
                  #'prop:Printable
                  #'prop:Printable
                  #'Printable-ref
                  '#(#&describe)
                  #'#(#:abstract)
                  (hasheq 'describe 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  '()))


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
   [block PrintDesc.block]))

(define (get-printer who v)
  (define vt (Printable-ref v #f))
  (unless vt
    (raise-argument-error* who rhombus-realm "Printable" v))
  vt)

(define-annotation-syntax PrintDesc
  (identifier-annotation #'printable-description? #'()))

(define description-ann-str "PrintDesc || String || Bytes")

(define (print-description-unwrap who pd [result? #f])
  (cond
    [(PrintDesc? pd)
     (PrintDesc-doc pd)]
    [(string? pd) (pretty-text pd)]
    [(bytes? pd) (pretty-text pd)]
    [else
     (and who
          (if result?
              (raise-result-error* who rhombus-realm description-ann-str pd)
              (raise-argument-error* who rhombus-realm description-ann-str pd)))]))

(define (printable-description? v)
  (and (print-description-unwrap #f v) #t))

(define/arity (PrintDesc.concat . pds)
  (PrintDesc
   `(seq ,@(for/list ([pd (in-list pds)])
             (print-description-unwrap 'PrintDesc.concat pd)))))
       
(define/arity (PrintDesc.newline)
  (PrintDesc (pretty-newline)))
       
(define/arity (PrintDesc.nest n pd)
  (unless (exact-integer? n)
    (raise-argument-error* rhombus-realm "Integer" pd))
  (PrintDesc
   (pretty-nest n (print-description-unwrap 'PrintDesc.nest pd))))

(define/arity (PrintDesc.align pd)
  (PrintDesc
   (pretty-align (print-description-unwrap 'PrintDesc.align pd))))

(define/arity (PrintDesc.or pd1 pd2)
  (PrintDesc
   (pretty-or (print-description-unwrap 'PrintDesc.or pd1)
              (print-description-unwrap 'PrintDesc.or pd2))))

(define/arity (PrintDesc.flat pd)
  (PrintDesc
   (pretty-flat (print-description-unwrap 'PrintDesc.flat pd))))

(define/arity (PrintDesc.list pre elems post)
  (define pre-pd (print-description-unwrap 'PrintDesc.list pre))
  (define (bad-elems)
    (raise-argument-error* 'PrintDesc.list rhombus-realm
                           (format "ConsList.of(~a)" description-ann-str)
                           elems))
  (define elem-pds (if (list? elems)
                       (for/list ([elem (in-list elems)])
                         (or (print-description-unwrap #f elem)
                             (bad-elems)))
                       (bad-elems)))  
  (PrintDesc
   (pretty-listlike pre-pd
                    elem-pds
                    (print-description-unwrap 'PrintDesc.list post))))

(define/arity (PrintDesc.block head body)
  (PrintDesc
   (pretty-blocklike (print-description-unwrap 'PrintDesc.block head)
                     (print-description-unwrap 'PrintDesc.block body))))

(define-static-info-syntaxes (print-graph)
  (#%function-arity 3))

(define/arity (Printable.describe v
                                  #:mode [mode 'text]
                                  #:pretty [pretty? #t])
  (check-mode  'Printable.describe mode)
  (parameterize ([current-print-as-pretty pretty?])
    (PrintDesc (pretty v mode (make-hasheq)))))

(define/arity (Printable.render pd
                                [op (current-output-port)]
                                #:column [column 0])
  (define who 'Printable.render)
  (define doc (print-description-unwrap who pd))
  (check-output-port who op)
  (unless (exact-nonnegative-integer? column)
    (raise-argument-error* who rhombus-realm "NonnegInt" column))
  (render-pretty doc op
                 #:column column))
