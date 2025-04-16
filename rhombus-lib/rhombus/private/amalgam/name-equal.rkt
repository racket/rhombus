#lang racket/base
(require syntax/parse/pre
         racket/phase+space
         racket/symbol
         "pack.rkt"
         "dotted-sequence.rkt"
         "annotation-failure.rkt"
         "syntax-wrap.rkt")

(provide equal-name-and-scopes?
         name-to-symbol)

(define (equal-name-and-scopes? who id1 id2 phase)
  (define l1 (extract-name-components who id1))
  (define l2 (extract-name-components who id2))
  (unless (phase? phase)
    (raise-annotation-failure who phase "SyntaxPhase"))
  (and (= (length l1) (length l2))
       (for/and ([n1 (in-list l1)]
                 [n2 (in-list l2)])
         (bound-identifier=? n1 n2 phase))))

(define (name-to-symbol who v)
  (syntax-parse (and (syntax*? v)
                     (unpack-group v who #f #t))
    #:datum-literals (group op)
    [(group (op o)) (syntax-e #'o)]
    [(group x:identifier) (syntax-e #'x)]
    [(group s::dotted-operator-or-identifier-sequence)
     (string->symbol
      (apply
       string-append
       (let loop ([s #'s])
         (syntax-parse s
           #:datum-literals (parens group op |.|)
           [(head (op |.|) . tail)
            (list* (symbol->immutable-string (syntax-e #'head))
                   "."
                   (loop #'tail))]
           [((parens (group (op o)))) (list "(" (symbol->immutable-string (syntax-e #'o)) ")")]
           [(x) (list (symbol->immutable-string (syntax-e #'x)))]))))]
    [_ (raise-annotation-failure who v "Name")]))

(define (extract-name-components who stx)
  (define s (unpack-term stx #f #f))
  (or (cond
        [(identifier? s) (list s)]
        [s
         (syntax-parse s
           #:datum-literals (op)
           [(op id) (list #'id)]
           [_ #f])]
        [else
         (define g (unpack-group stx #f #f))
         (and g
              (syntax-parse g
                #:datum-literals (group)
                [(group n::dotted-operator-or-identifier-sequence)
                 (let loop ([l (syntax->list #'n)])
                   (cond
                     [(null? (cdr l)) l]
                     [else (cons (car l) (loop (cddr l)))]))]
                [_ #f]))])
      (raise-annotation-failure who stx "Name")))

