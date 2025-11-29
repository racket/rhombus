#lang racket/base
(require (prefix-in render: shrubbery/render)
         shrubbery/render/private/log
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
                  resolve-get/tentative
                  color-property)
         (submod scribble/racket id-element)
         (only-in scribble/search
                  find-racket-tag)
         (for-template
          (only-in rhombus/private/name-root
                   portal-syntax->lookup))
         rhombus/syntax
         "typeset-key-help.rkt"
         "hspace.rkt"
         "defining-element.rkt"
         "spacer-binding.rkt")

(provide typeset-rhombus
         typeset-rhombusblock)

(define (element*? v)
  (let ([v (if (injected? v)
               (injected-e v)
               v)])
    (and (not (null? v))
         (not (string? v))
         (not (symbol? v))
         (content? v))))

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

;; backward compatibility before v9.0.0.4
(define link-style-supported?
  (let-values ([(reqd allowed) (procedure-keywords make-id-element)])
    (and allowed (memq '#:link-style allowed))))

(define (make-id-element* id content as-define?
                          #:space space-name
                          #:link-style link-style
                          #:unlinked-ok? unlinked-ok?
                          #:suffix suffix)
  (if link-style-supported?
      (make-id-element id content as-define?
                       #:space space-name
                       #:link-style link-style
                       #:unlinked-ok? unlinked-ok?
                       #:suffix suffix)
      (make-id-element id (content->string content) as-define?
                       #:space space-name
                       #:unlinked-ok? unlinked-ok?
                       #:suffix suffix)))

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
                         [(lineno) (style 'smaller (list (color-property "blue")))]
                         [else tt-style])
                (if (eq? kind 'meta)
                    str
                    (keep-spaces (if (eq? kind 'lineno)
                                     (string-append str " ")
                                     str)))))
   #:render_in_space (lambda (space-name
                              #:prefix [prefix-str #f]
                              content
                              id
                              #:suffix [suffix-target #f]
                              #:suffix-space [suffix-space-name #f]
                              #:raw [raw? #f])
                       (define as-define? (syntax-property id 'typeset-define))
                       (define r
                         (element (and (not raw?) tt-style)
                           (let ()
                             (define main
                               (make-id-element* id content as-define?
                                                 #:space space-name
                                                 #:link-style (and raw? (style #f null))
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
                                    (define in-name-root-space (make-interned-syntax-introducer 'rhombus/namespace))
                                    (define in-annot-space (make-interned-syntax-introducer 'rhombus/annot))                                    
                                    (lambda (root-id ns-id root-names rators field field-str)
                                      ;; Try to get a result from calling `root . root . ... rator() . rator() ... . field`.
                                      ;; Even though `root` is in principle a namespace, it may be documented only
                                      ;; as an annotation, so try that as a fallback.
                                      ;; A `root-id` can be #f, in which case `root-names` must be empty.
                                      ;; Otherwise, `root-id` should have a corresponding initial element in
                                      ;; `root-names`, and `ns-id` has a corresponding final element in `root-names`,
                                      ;; but `root-id` and `ns-id` are composed with preceding roots and maybe an import
                                      ;; namespace so that they have a binding, while `root-names` is used for
                                      ;; the documented dotted name.
                                      (delayed-element
                                       (lambda (renderer sec ri)
                                         (define default (element tt-style field-str))
                                         (define (find-racket-tag* id root-id root-names
                                                                   #:space [space #f]
                                                                   #:shift? shift?)
                                           (define id* (if root-id
                                                           (in-name-root-space root-id 'add)
                                                           id))
                                           (log-shrubbery-render-info "FIND-RACKET-TAG~a"
                                                                      (format-log
                                                                       'shift? shift?
                                                                       'id id*
                                                                       'id-scopes (hash-ref (syntax-debug-info id*) 'context #f)
                                                                       'space (if root-id 'rhombus/namespace space)
                                                                       'suffix (if root-id
                                                                                   (list (format-suffix root-names id)
                                                                                         space)
                                                                                   space)
                                                                       'binding (identifier-binding
                                                                                 (if shift? (syntax-shift-phase-level id* #f) id*))))
                                           (define tag
                                             (find-racket-tag sec ri
                                                              (if shift? (syntax-shift-phase-level id* #f) id*)
                                                              #f
                                                              #:space (if root-id 'rhombus/namespace space)
                                                              #:suffix (if root-id
                                                                           (list (format-suffix root-names id)
                                                                                 space)
                                                                           space)
                                                              #:unlinked-ok? #t))
                                           (log-shrubbery-render-info "FIND-RACKET-TAG~a"
                                                                      (format-log
                                                                       'result tag))
                                           tag)
                                         (define (format-suffix root-names id)
                                           (string->symbol
                                            (string-append
                                             (apply string-append
                                                    (for/list ([name (in-list root-names)])
                                                      (format "~a." (syntax-e name))))
                                             (symbol->string (syntax-e id)))))
                                         (let root-loop ([rators rators] [root-id root-id] [ns-id ns-id] [root-names root-names]) 
                                           (log-shrubbery-render-info "RESULT LOOP~a"
                                                                      (format-log
                                                                       'root-id root-id
                                                                       'root-names root-names
                                                                       'rators rators
                                                                       'field field))
                                           (define (start)
                                             (cond
                                               [root-id                                                
                                                (define ns-id* (in-name-root-space ns-id 'add))
                                                (define annot-id (in-annot-space ns-id 'add))
                                                (prep-namespace-for-binding ns-id*)
                                                (find-via-namespace-id ns-id* annot-id rators #f root-id root-names)]
                                               [else
                                                (define rator (car rators))
                                                (define tag (find-racket-tag* rator #f null
                                                                              #:shift? #f))
                                                (parameterize ([current-namespace ns])
                                                  (find-via-rator-tag tag rator (cdr rators)))]))

                                           (define (find-via-rator-tag rator-tag rator more-rators)
                                             (define spacer-infos (and rator-tag
                                                                       (resolve-get/tentative sec ri (list 'spacer-infos rator-tag))))
                                             (define result-annot (and spacer-infos
                                                                       (hash-ref spacer-infos 'result_annotation #f)))
                                             (log-shrubbery-render-info "RATOR~a"
                                                                        (format-log
                                                                         'rator rator
                                                                         'rator-tag rator-tag))
                                             (cond
                                               [result-annot
                                                (find-via-annot-spacer-binding result-annot more-rators)]
                                               [else
                                                ;; try a class binding => constructor
                                                (define in-class-space (make-interned-syntax-introducer 'rhombus/class))
                                                (define class-id (in-class-space rator 'add))
                                                (log-shrubbery-render-info "CLASS~a"
                                                                           (format-log
                                                                            'class-id class-id
                                                                            'binding (identifier-binding class-id #f)))
                                                (cond
                                                  [(find-racket-tag* class-id #f null
                                                                     #:shift? #f
                                                                     #:space 'rhombus/class)
                                                   => (lambda (tag)
                                                        (log-shrubbery-render-info "CLASS~a"
                                                                                   (format-log
                                                                                    'tag tag))
                                                        (root-loop (cdr rators) rator rator (list rator)))]
                                                  [else default])]))

                                           (define (find-via-annot-spacer-binding result-annot more-rators)
                                             (log-shrubbery-render-info "ANNOT-SPACER~a"
                                                                        (format-log
                                                                         'result-annot result-annot))
                                             (cond
                                               [(and result-annot
                                                     (spacer-binding? result-annot))
                                                (define sb result-annot)
                                                (define sym (spacer-binding-datum sb))
                                                (define ns-id (let ([id (binding->id sym (spacer-binding-ns-b sb))])
                                                                (and id (syntax-shift-phase-level id #f))))
                                                (define annot-id (let ([id (binding->id sym (spacer-binding-annot-b sb))])
                                                                   (and id
                                                                        (syntax-shift-phase-level id #f))))
                                                (find-via-namespace-id ns-id annot-id more-rators #f ns-id (list (datum->syntax #f sym)))]
                                               [(and (hash? result-annot)
                                                     (spacer-binding? (hash-ref result-annot 'id #f))
                                                     (symbol? (hash-ref result-annot 'sym #f))
                                                     (spacer-binding? (hash-ref result-annot 'root_id #f))
                                                     (let ([l (hash-ref result-annot 'root_syms #f)])
                                                       (and (pair? l) (list? l) (andmap symbol? l))))
                                                (define sb (hash-ref result-annot 'id))
                                                (define root-sb (hash-ref result-annot 'root_id))
                                                (define root-syms (hash-ref result-annot 'root_syms))
                                                (define sym (hash-ref result-annot 'sym))
                                                (define root-names (map
                                                                    (lambda (sym) (datum->syntax #f sym))
                                                                    (append root-syms (list sym))))
                                                (define root-id (let ([id (binding->id (car root-syms) (spacer-binding-ns-b root-sb))])
                                                                  (and id (syntax-shift-phase-level id #f))))
                                                (define ns-id (let ([id (binding->id sym (spacer-binding-ns-b sb))])
                                                                (and id (syntax-shift-phase-level (in-name-root-space id 'add) #f))))
                                                (define annot-id (let ([id (binding->id sym (spacer-binding-annot-b sb))])
                                                                   (and id (in-annot-space (syntax-shift-phase-level id #f) 'add))))
                                                (find-via-namespace-id ns-id annot-id more-rators #f root-id root-names)]
                                               [else default]))

                                           (define (find-via-namespace-id ns-id annot-id more-rators shift? root-id root-names)
                                             (define (try-fallback)
                                               (cond
                                                 [annot-id
                                                  (log-shrubbery-render-info "FALLBACK~a"
                                                                             (format-log
                                                                              'annot-id annot-id
                                                                              'root-id root-id
                                                                              'root-name root-names
                                                                              'shift? shift?))
                                                  (define tag (find-racket-tag* (if (null? (cdr root-names))
                                                                                    annot-id
                                                                                    (car (reverse root-names)))
                                                                                (and (pair? (cdr root-names))
                                                                                     root-id)
                                                                                (reverse (cdr (reverse root-names)))
                                                                                #:shift? shift?
                                                                                #:space 'rhombus/annot))
                                                  (define spacer-infos (and tag
                                                                            (resolve-get/tentative sec ri (list 'spacer-infos tag))))
                                                  (define fallback-annot (and spacer-infos
                                                                              (hash-ref spacer-infos 'method_fallback #f)))
                                                  (log-shrubbery-render-info "FALLBACK-R~a"
                                                                             (format-log
                                                                              'tag tag
                                                                              'spacer-infos spacer-infos))
                                                  (if fallback-annot
                                                      (find-via-annot-spacer-binding fallback-annot more-rators)
                                                      default)]
                                                 [else default]))
                                             (define p (and ns-id (with-handlers ([exn:fail? (lambda (x) #f)])
                                                                    (identifier-binding-portal-syntax ns-id (if shift? 0 #f)))))
                                             (define lookup (and p (portal-syntax->lookup p (lambda (self-id lookup) lookup) #f)))
                                             (define next-field (if (null? more-rators)
                                                                    field
                                                                    (car more-rators)))
                                             (define next-id (and lookup (lookup #f "identifier"
                                                                                 next-field
                                                                                 values)))
                                             (log-shrubbery-render-info "SEARCH~a"
                                                                        (format-log
                                                                         'ns-id ns-id
                                                                         'ns-binding (and ns-id (identifier-binding
                                                                                                 (in-name-root-space ns-id 'add)
                                                                                                 #f))
                                                                         'ns-context (and ns-id
                                                                                          (hash-ref (syntax-debug-info ns-id) 'context))
                                                                         'shift? shift?
                                                                         'lookup lookup
                                                                         'root-id root-id
                                                                         'root-names root-names
                                                                         'root-binding (and root-id (identifier-binding
                                                                                                     (in-name-root-space root-id 'add)
                                                                                                     #f))
                                                                         'root-context (and root-id
                                                                                            (hash-ref (syntax-debug-info
                                                                                                       (in-name-root-space root-id 'add))
                                                                                                      'context))
                                                                         'next-field next-field
                                                                         'next-id next-id))
                                             (cond
                                               [next-id
                                                (cond
                                                  [(find-racket-tag* next-field root-id root-names
                                                                     #:shift? shift?)
                                                   => (lambda (tag)
                                                        (cond
                                                          [(pair? more-rators)
                                                           (find-via-rator-tag tag next-id (cdr more-rators))]
                                                          [else
                                                           (define e
                                                             (make-id-element (in-name-root-space
                                                                               (if shift? (syntax-shift-phase-level root-id #f) root-id)
                                                                               'add)
                                                                              field-str #f
                                                                              #:unlinked-ok? #t
                                                                              #:space 'rhombus/namespace
                                                                              #:suffix (list (format-suffix root-names field)
                                                                                             #f)))
                                                           (element tt-style e)]))]
                                                  [else
                                                   (try-fallback)])]
                                               [else
                                                (try-fallback)]))

                                           (start)))
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
  (render_code stx #:space space-name-in #:content content))

(define (typeset-rhombusblock stx
                              #:inline [inline? #f]
                              #:text [text? #f]
                              #:inset [inset? #t]
                              #:indent [indent-amt 0]
                              #:prompt [prompt ""]
                              #:indent_from_block [indent-from-block? #t]
                              #:spacer_info_box [info-box #f]
                              #:number_from [number-from #f])
  (define output-block
    (render_code_block stx
                       #:inline inline?
                       #:text text?
                       #:indent indent-amt
                       #:prompt prompt
                       #:indent_from_block indent-from-block?
                       #:spacer_info_box info-box
                       #:number_from number-from))
  (if (and inset? (not inline?))
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
