#lang racket/base
(require (prefix-in render: shrubbery/render)
         scribble/racket
         (only-in scribble/core
                  element
                  element?
                  element-content
                  delayed-element
                  delayed-element?
                  delayed-element-plain
                  content?
                  content-width
                  paragraph
                  table
                  style
                  plain
                  nested-flow
                  resolve-get
                  resolve-get/tentative)
         (submod scribble/racket id-element)
         (only-in scribble/search
                  find-racket-tag)
         (for-template
          (only-in rhombus/private/name-root
                   portal-syntax->lookup))
         "typeset-key-help.rkt"
         "hspace.rkt"
         "defining-element.rkt"
         "spacer-binding.rkt")

(provide typeset-rhombus
         typeset-rhombusblock)

(define (element*? v)
  (and (not (null? v))
       (not (string? v))
       (not (symbol? v))
       (content? v)))

(define tt-style (style 'tt null))

(struct target-style (prefix-len))

(define (element-shape e e-len e-style)
  (values (content-width e)
          (or e-style
              (let loop ([e e])
                (cond
                  [(pair? e) (loop (car e))]
                  [(defining-element? e) (target-style (defining-element-prefix-len e))]
                  [(element? e) (loop (element-content e))]
                  [(delayed-element? e) (loop ((delayed-element-plain e)))]
                  [else #f])))))

(define-values (render_code
                render_code_block)
  (render:make
   #:render (lambda (kind str)
              (element (case kind
                         [(paren) paren-color]
                         [(variable) variable-color]
                         [(meta plain datum) tt-style]
                         [(value) value-color]
                         [(result) result-color]
                         [(error) error-color]
                         [(comment) comment-color]
                         [else tt-style])
                (if (eq? kind 'meta)
                    str
                    (keep-spaces str))))
   #:render_in_space (lambda (space-name
                              #:prefix [prefix-str #f]
                              str
                              id
                              #:suffix [suffix-target #f]
                              #:suffix-space [suffix-space-name #f])
                       (define as-define? (syntax-property id 'typeset-define))
                       (define r
                         (element tt-style
                           (let ()
                             (define main
                               (make-id-element id str as-define?
                                                #:space space-name
                                                #:unlinked-ok? #t
                                                #:suffix (if suffix-target
                                                             (list (target-id-key-symbol suffix-target)
                                                                   suffix-space-name)
                                                             space-name)))
                             (if prefix-str
                                 (list prefix-str main)
                                 main))))
                       (cond
                         [as-define? (defining-element #f r (if prefix-str
                                                                (string-length prefix-str)
                                                                0))]
                         [else r]))
   #:render_via_result_annotation (let ([ns (make-base-namespace)])
                                    (lambda (rator field field-str)
                                      (delayed-element
                                       (lambda (renderer sec ri)
                                         (define default (element tt-style field-str))
                                         (define tag (find-racket-tag sec ri rator #f
                                                                      #:unlinked-ok? #t))
                                         (define spacer-infos (resolve-get/tentative sec ri (list 'spacer-infos tag)))
                                         (define result-annot (and spacer-infos
                                                                   (hash-ref spacer-infos 'result_annotation #f)))
                                         (cond
                                           [(and result-annot
                                                 (spacer-binding? result-annot))
                                            (parameterize ([current-namespace ns])
                                              (define root-sym (spacer-binding-datum result-annot))
                                              (define nom-mpi (spacer-binding-ns-nom-mpi result-annot))
                                              (define mpi (spacer-binding-ns-mpi result-annot))
                                              (define (reset mpi)
                                                (define-values (sub base) (module-path-index-split mpi))
                                                (if (not sub)
                                                    #f
                                                    (module-path-index-join sub (and base (reset base)))))
                                              (module-path-index-resolve (reset nom-mpi) #t)
                                              (module-path-index-resolve (reset mpi) #t)
                                              (define ns-id
                                                (syntax-binding-set->syntax (syntax-binding-set-extend
                                                                             (syntax-binding-set)
                                                                             root-sym
                                                                             0
                                                                             mpi
                                                                             #:source-symbol (spacer-binding-ns-sym result-annot)
                                                                             #:source-phase (spacer-binding-ns-phase result-annot)
                                                                             #:nominal-module nom-mpi
                                                                             #:nominal-symbol (spacer-binding-ns-nom-sym result-annot)
                                                                             #:nominal-phase (spacer-binding-ns-export-phase result-annot)
                                                                             #:nominal-require-phase (spacer-binding-ns-import-phase result-annot))
                                                                            root-sym))
                                              (define p (identifier-binding-portal-syntax ns-id 0))
                                              (define lookup (and p (portal-syntax->lookup p (lambda (self-id lookup) lookup) #f)))
                                              (define field-id (and lookup (lookup #f "identifier" field values)))
                                              (cond
                                                [field-id
                                                 (define e
                                                   (make-id-element (syntax-shift-phase-level ns-id #f) field-str #f
                                                                    ;; #:unlinked-ok? #t
                                                                    #:space 'rhombus/namespace
                                                                    #:suffix (list (string->symbol
                                                                                    (format "~a.~a"
                                                                                            root-sym
                                                                                            (syntax-e field)))
                                                                                   #f)))
                                                 (element tt-style e)]
                                                [else
                                                 default]))]
                                           [else
                                            default]))
                                       (lambda () field-str)
                                       (lambda () field-str))))
   #:render_whitespace (lambda (n)
                         (element hspace-style (make-string n #\space)))
   #:render_indentation (lambda (n offset-in-orig orig-n orig-size style)
                          (cond
                            [(target-style? style)
                             (let* ([pre (min (max (- (target-style-prefix-len style)
                                                      offset-in-orig)
                                                   0)
                                              n)]
                                    [post (- n pre)])
                               (define bold-spaces
                                 (element 'tt (element value-def-color
                                                       (for/list ([i (in-range post)]) 'nbsp))))
                               (if (= pre 0)
                                   bold-spaces
                                   (list (element hspace-style (make-string pre #\space))
                                         bold-spaces)))]
                            [else
                             (element hspace-style (make-string n #\space))]))
   #:render_one_line (lambda (elems) elems)
   #:render_line (lambda (elems)
                   (paragraph plain elems))
   #:render_lines (lambda (lines)
                    (if (null? lines)
                        (element plain "")
                        (table plain (map list lines))))
   #:rendered_shape element-shape
   #:is_rendered element*?))

(define (typeset-rhombus stx
                         #:space [space-name-in #f])
  (render_code stx #:space space-name-in))

(define (typeset-rhombusblock stx
                              #:inset [inset? #t]
                              #:indent [indent-amt 0]
                              #:prompt [prompt ""]
                              #:indent_from_block [indent-from-block? #t])
  (define output-block
    (render_code_block stx
                       #:indent indent-amt
                       #:prompt prompt
                       #:indent_from_block indent-from-block?))
  (if inset?
      (nested-flow (style 'code-inset null) (list output-block))
      output-block))
