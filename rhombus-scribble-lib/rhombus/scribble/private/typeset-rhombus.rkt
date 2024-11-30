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
                  content->string
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
                                    (lambda (root-annot rators field field-str)
                                      (delayed-element
                                       (lambda (renderer sec ri)
                                         (define default (element tt-style field-str))
                                         (define (start)
                                           (cond
                                             [root-annot
                                              (define in-name-root-space (make-interned-syntax-introducer 'rhombus/namespace))
                                              (define in-annot-space (make-interned-syntax-introducer 'rhombus/annot))
                                              (define ns-id (in-name-root-space root-annot 'add))
                                              (define annot-id (in-annot-space root-annot 'add))
                                              (prep-namespace-for-binding ns-id)
                                              (find-via-namespace-id ns-id annot-id rators #f)]
                                             [else
                                              (define rator (car rators))
                                              (define tag (find-racket-tag sec ri rator #f
                                                                           #:unlinked-ok? #t))
                                              (parameterize ([current-namespace ns])
                                                (find-via-rator-tag tag (cdr rators)))]))

                                         (define (find-via-rator-tag rator-tag more-rators)
                                           (define spacer-infos (and rator-tag
                                                                     (resolve-get/tentative sec ri (list 'spacer-infos rator-tag))))
                                           (define result-annot (and spacer-infos
                                                                     (hash-ref spacer-infos 'result_annotation #f)))
                                           (find-via-annot-spacer-binding result-annot more-rators))

                                         (define (find-via-annot-spacer-binding result-annot more-rators)
                                           (cond
                                             [(and result-annot
                                                   (spacer-binding? result-annot))
                                              (define root-sym (spacer-binding-datum result-annot))
                                              (define ns-id (binding->id root-sym (spacer-binding-ns-b result-annot)))
                                              (define annot-id (binding->id root-sym (spacer-binding-annot-b result-annot)))
                                              (find-via-namespace-id ns-id annot-id more-rators #t)]
                                             [else default]))

                                         (define (find-via-namespace-id ns-id annot-id more-rators shift?)
                                           (define (try-fallback)
                                             (cond
                                               [annot-id
                                                (define tag (find-racket-tag sec ri (syntax-shift-phase-level annot-id #f) #f
                                                                             #:space 'rhombus/annot
                                                                             #:unlinked-ok? #t))
                                                (define spacer-infos (and tag
                                                                          (resolve-get/tentative sec ri (list 'spacer-infos tag))))
                                                (define fallback-annot (and spacer-infos
                                                                            (hash-ref spacer-infos 'method_fallback #f)))
                                                (if fallback-annot
                                                    (find-via-annot-spacer-binding fallback-annot more-rators)
                                                    default)]
                                               [else default]))
                                           (define p (and ns-id (with-handlers ([exn:fail? (lambda (x) #f)])
                                                                  (identifier-binding-portal-syntax ns-id 0))))
                                           (define lookup (and p (portal-syntax->lookup p (lambda (self-id lookup) lookup) #f)))
                                           (define next-field (if (null? more-rators)
                                                                  field
                                                                  (car more-rators)))
                                           (define next-id (and lookup (lookup #f "identifier"
                                                                               next-field
                                                                               values)))
                                           (cond
                                             [next-id
                                              (cond
                                                [(find-racket-tag sec ri (if shift? (syntax-shift-phase-level ns-id #f) ns-id) #f
                                                                  #:space 'rhombus/namespace
                                                                  #:suffix (list (string->symbol
                                                                                  (format "~a.~a"
                                                                                          (syntax-e ns-id)
                                                                                          (syntax-e next-field)))
                                                                                 #f)
                                                                  #:unlinked-ok? #t)
                                                 => (lambda (tag)
                                                      (cond
                                                        [(pair? more-rators)
                                                         (find-via-rator-tag tag (cdr more-rators))]
                                                        [else
                                                         (define e
                                                           (make-id-element (if shift? (syntax-shift-phase-level ns-id #f) ns-id) field-str #f
                                                                            #:unlinked-ok? #t
                                                                            #:space 'rhombus/namespace
                                                                            #:suffix (list (string->symbol
                                                                                            (format "~a.~a"
                                                                                                    (syntax-e ns-id)
                                                                                                    (syntax-e field)))
                                                                                           #f)))
                                                         (element tt-style e)]))]
                                                [else (try-fallback)])]
                                             [else
                                              (try-fallback)]))

                                         (start))
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
                         #:space [space-name-in #f]
                         #:content [content #f])
  (render_code stx #:space space-name-in #:content (and content
                                                        (content->string content))))

(define (typeset-rhombusblock stx
                              #:inset [inset? #t]
                              #:indent [indent-amt 0]
                              #:prompt [prompt ""]
                              #:indent_from_block [indent-from-block? #t]
                              #:spacer_info_box [info-box #f])
  (define output-block
    (render_code_block stx
                       #:indent indent-amt
                       #:prompt prompt
                       #:indent_from_block indent-from-block?
                       #:spacer_info_box info-box))
  (if inset?
      (nested-flow (style 'code-inset null) (list output-block))
      output-block))

(define (binding->id root-sym b)
  (cond
    [(not b) #f]
    [else
     (define-values (mpi sym nom-mpi nom-sym phase import-phase export-phase)
       (apply values b))
     (load-mpi nom-mpi)
     (load-mpi mpi)
     (syntax-binding-set->syntax (syntax-binding-set-extend
                                  (syntax-binding-set)
                                  root-sym
                                  0
                                  mpi
                                  #:source-symbol sym
                                  #:source-phase phase
                                  #:nominal-module nom-mpi
                                  #:nominal-symbol nom-sym
                                  #:nominal-phase export-phase
                                  #:nominal-require-phase import-phase)
                                 root-sym)]))

(define (prep-namespace-for-binding id)
  (define b (identifier-binding id #f))
  (when b
    (define-values (mpi sym nom-mpi nom-sym phase import-phase export-phase) (apply values b))
    (load-mpi nom-mpi)
    (load-mpi mpi)))

(define (load-mpi mpi)
  (define (reset mpi)
    (define-values (sub base) (module-path-index-split mpi))
    (if (not sub)
        #f
        (module-path-index-join sub (and base (reset base)))))
  (with-handlers ([exn:fail? void])
    (module-path-index-resolve (reset mpi) #t)))
