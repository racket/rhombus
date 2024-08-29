#lang racket/base
(require (prefix-in render: shrubbery/render)
         scribble/racket
         (only-in scribble/core
                  element
                  element?
                  element-content
                  content?
                  content-width
                  paragraph
                  table
                  style
                  plain
                  nested-flow)
         (submod scribble/racket id-element)
         "typeset-key-help.rkt"
         "hspace.rkt"
         "defining-element.rkt")

(provide typeset-rhombus
         typeset-rhombusblock)

(define (element*? v)
  (and (not (null? v))
       (not (string? v))
       (not (symbol? v))
       (content? v)))

(define tt-style (style 'tt null))

(define (element-shape e e-len e-style)
  (values (content-width e)
          (or e-style
              (let loop ([e e])
                (cond
                  [(pair? e) (loop (car e))]
                  [(defining-element? e) 'target]
                  [(element? e) (loop (element-content e))]
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
                              str
                              id
                              #:suffix [suffix-target #f]
                              #:suffix-space [suffix-space-name #f])
                       (element tt-style
                         (make-id-element id str #f
                                          #:space space-name
                                          #:unlinked-ok? #t
                                          #:suffix (if suffix-target
                                                       (list (target-id-key-symbol suffix-target)
                                                             suffix-space-name)
                                                       space-name))))
   #:render_whitespace (lambda (n)
                         (element hspace-style (make-string n #\space)))
   #:render_indentation (lambda (n orig-n orig-size style)
                          (if (eq? style 'target)
                              (element 'tt (element value-def-color
                                             (for/list ([i (in-range n)]) 'nbsp)))
                              (element hspace-style (make-string n #\space))))
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
