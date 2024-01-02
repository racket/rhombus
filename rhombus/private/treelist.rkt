#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum
         racket/vector
         racket/hash-code
         "version-case.rkt")

(provide make-treelist
         treelist-ref
         treelist-set
         treelist-add
         treelist-cons
         treelist-append
         treelist-insert
         treelist-remove
         treelist-take
         treelist-drop
         treelist-length
         treelist->list
         list->treelist
         in-treelist
         chaperone-treelist)

(#%declare #:unsafe)

(define BITS 5)
(define MAX_WIDTH (expt 2 BITS))
(define MASK (- MAX_WIDTH 1))
(define MAX_ERROR 2)

(define (radix index height)
  (bitwise-and (fxrshift index (fx* BITS height)) MASK))

;; a node in the RRB Tree
;;
;;  - a node is fully dense if it has exactly `m` children where `m` is the branching factor of the overall Tree
;;    and each child is also fully dense
;;  - a node is leftwise dense if its first `n - 1` children, where `n` is its total number of children,
;;    are fully dense, and its `n`th child is leftwise dense or fully dense. `n` is allowed to be < `m`
;;  - note dense implies leftwise dense, and leaves are always at least leftwise dense
;;  - a node that is not leftwise dense contain a size array `sizes`; leftwise dense node usually don't,
;;    but a transition into leftwise dense is not always detected

(define-syntax Node
  (syntax-rules ()
    [(Node) empty-node]
    [(Node children) children]
    [(Node children sizes) (let ([cs children]
                                 [szs sizes])
                             (unless (variable-reference-from-unsafe? (#%variable-reference))
                               #;(unless (vector? cs) (error 'Node "bad children vector: ~v" cs))
                               (unless (or (not szs) (vector? szs)) (error 'Node "bad sizes vector: ~v" szs)))
                             (if szs (cons cs szs) cs))]))

(meta-if-version-at-least
 "8.11.1.10"
 (begin)
 (begin
   (define vector*-copy vector-copy)
   (define vector*-append vector-append)
   (define vector*-set/copy (lambda (vec i val)
                              (define new-vec (vector-copy vec))
                              (vector*-set! vec i val)
                              vec))))

(define (vector*-take vec n) (vector*-copy vec 0 n))
(define (vector*-drop vec n) (vector*-copy vec n (vector*-length vec)))
(define (vector*-drop-right vec n) (vector*-copy vec 0 (- (vector*-length vec) n)))
(define (vector*-add-left val a) (vector*-append (vector val) a))
(define (vector*-add-right a val) (vector*-append a (vector val)))

(define (assert-node n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (or (vector? n) (and (pair? n) (vector? (car n)) (vector? (cdr n))))
      (error 'node "expected a node: ~v" n))))

(define (node-leftwise-dense? n) (assert-node n) (not (pair? n)))
(define (node-children n) (assert-node n) (if (pair? n) (car n) n))
(define (node-sizes n) (assert-node n) (and (pair? n) (cdr n)))
(define (node-size n) (assert-node n) (vector*-length (node-children n)))
(define (node-first n) (assert-node n) (vector*-ref (node-children n) 0))
(define (node-last n) (assert-node n) (let ([cs (node-children n)])
                                        (vector*-ref cs (fx- (vector*-length cs) 1))))
(define (node-ref n i) (assert-node n) (vector*-ref (node-children n) i))
(define (node-set n i v) (assert-node n) (vector*-set/copy (node-children n) i v))
(define (node-length n) (assert-node n) (vector*-length (node-children n)))

;; `node*` refers to a leftwise dense node
(define (assert-node* n)
  (unless (variable-reference-from-unsafe? (#%variable-reference))
    (unless (vector? n)
      (error 'node* "expected a node*: ~v" n))))

(define (node*-children n) (assert-node* n) n)
(define (node*-ref n i) (assert-node* n) (vector*-ref (node*-children n) i))
(define (node*-set n i v) (assert-node* n) (vector*-set/copy (node*-children n) i v))
(define (node*-length n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-size n) (assert-node* n) (vector*-length (node*-children n)))
(define (node*-last n) (assert-node* n) (let ([cs (node*-children n)])
                                          (vector*-ref cs (fx- (vector*-length cs) 1))))

(define empty-node (Node (vector)))

(define (leaf v) (Node (vector v)))

(struct treelist (root size height)
  #:property prop:equal+hash (list
                              (lambda (tl other-tl recur)
                                (treelist-equal? tl other-tl recur))
                              (lambda (v recur)
                                (treelist-hash-code v recur))
                              (lambda (v recur)
                                42)))

(define empty-treelist (treelist empty-node 0 0))

(define make-treelist
  (case-lambda
    [() empty-treelist]
    [(a) (treelist (leaf a) 1 0)]
    [(a b) (treelist (Node (vector a b)) 2 0)]
    [(a b c) (treelist (Node (vector a b c)) 3 0)]
    [ds (list->treelist ds)]))

(define (check-treelist who tl)
  (unless (treelist? tl)
    (raise-argument-error who "treelist?" tl)))

(define (check-treelist-index who size index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "index out of range: ~v" index)))
  (when (or (index . fx< . 0) (index . fx>= . size))
    (error who "index out of range: ~v / ~s" index size)))

(define (check-treelist-end-index who size index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "count out of range: ~v" index)))
  (when (or (index . fx< . 0) (index . fx> . size))
    (error who "count out of range: ~v / ~v" index size)))

(define-sequence-syntax in-treelist
  (lambda () #'in-treelist/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(d) (_ tl-expr)]
       #'[(d)
          (:do-in
           ([(tl) tl-expr])
           (unless (variable-reference-from-unsafe? (#%variable-reference))
             (check-treelist 'in-treelist tl))
           ([pos 0]
            [node empty-node]
            [node-pos 0])
           (pos . fx< . (treelist-size tl))
           ([(d next-node next-node-pos)
             (if (node-pos . fx< . (node-size node))
                 (values (node-ref node node-pos) node (fx+ node-pos 1))
                 (let-values ([(node node-pos) (treelist-node-for tl pos)])
                   (values (node-ref node node-pos) node (fx+ node-pos 1))))])
           #t
           #t
           ((fx+ pos 1) next-node next-node-pos))]])))

(define (in-treelist/proc tl)
  ;; Slower strategy than the inline version, but the
  ;; `make-do-sequence` protocol requires a single value for the
  ;; position, and allocating a value to track index plus node defeats
  ;; the benefit of threading the current node
  (check-treelist 'in-treelist tl)
  (make-do-sequence
   (lambda ()
     (values
      (lambda (i) (treelist-ref tl i))
      (lambda (i) (fx+ i 1))
      0
      (lambda (i) (i . fx< . (treelist-size tl)))
      #f
      #f))))

(define (treelist-ref tl index)
  (cond
    [(impersonator? tl) (treelist-ref/slow tl index)]
    [else
     (check-treelist 'treelist-ref tl)
     (check-treelist-index 'treelist-ref (treelist-size tl) index)
     (define-values (node pos) (treelist-node-for tl index))
     (node-ref node pos)]))

(define (treelist-node-for tl index)
  (cond
    [(impersonator? tl) (treelist-node-for/slow tl index)]
    [else
     (let walk ([node (treelist-root tl)]
                [index index]
                [height (treelist-height tl)])
       (cond
         [(fx= height 0)
          (values node (bitwise-and index MASK))]
         [(node-leftwise-dense? node)
          (walk (node*-ref node (radix index height)) index (fx- height 1))]
         [else
          (define-values (bi si) (step node index height))
          (walk (node-ref node bi) si (fx- height 1))]))]))

(define (treelist-equal? tl other-tl recur)
  (cond
    [(or (impersonator? tl)
         (impersonator? other-tl))
     (treelist-equal?/slow tl other-tl recur)]
    [else
     (define len (treelist-size tl))
     (cond
       [(not (fx= len (treelist-size other-tl)))
        #f]
       [else
        ;; we could use `for` to iterate through the trees, but
        ;; we implement the traversal manually so that we can detect
        ;; a shared subtree and skip it
        (let loop ([i 0]
                   [a-node empty-node]
                   [a-pos 0]
                   [b-node empty-node]
                   [b-pos 0])
          (cond
            [(fx= i len) #t]
            [(a-pos . >= . (node-size a-node))
             (cond
               [(and (b-pos . >= . (node-size b-node))
                     (shared-subtree-size tl other-tl i))
                => (lambda (len)
                     (loop (fx+ i len) empty-node 0 empty-node 0))]
               [else
                (define-values (a-node a-pos) (treelist-node-for tl i))
                (loop i a-node a-pos b-node b-pos)])]
            [(b-pos . >= . (node-size b-node))
             (define-values (b-node b-pos) (treelist-node-for other-tl i))
             (loop i a-node a-pos b-node b-pos)]
            [else
             (and (recur (node-ref a-node a-pos) (node-ref b-node b-pos))
                  (loop (fx+ i 1) a-node (fx+ a-pos 1) b-node (fx+ b-pos 1)))]))])]))

;; same traversal as `treelist-node-for`, but for two trees at the
;; same time to try to find a shared subtree and return its size
(define (shared-subtree-size tl other-tl index)
  (let walk ([node (treelist-root tl)]
             [other-node (treelist-root other-tl)]
             [index index]
             [other-index index]
             [height (treelist-height tl)]
             [other-height (treelist-height other-tl)])
    (cond
      [(fx= height 0)
       (and (eq? node other-node)
            (node*-size node))]
      [(eq? node other-node)
       (size-subtree node height)]
      [(not (fx= other-height height))
       (cond
         [(fx< other-height height)
          (define-values (bi si) (if (node-leftwise-dense? node)
                                     (values (radix index height) index)
                                     (step node index height)))
          (walk (node-ref node bi)
                other-node
                si
                other-index
                (fx- height 1)
                other-height)]
         [else (walk other-node
                     node
                     other-index
                     index
                     other-height
                     height)])]
      [else
       (define-values (bi si) (if (node-leftwise-dense? node)
                                  (values (radix index height) index)
                                  (step node index height)))
       (define-values (other-bi other-si) (if (node-leftwise-dense? other-node)
                                              (values (radix other-index height) other-index)
                                              (step other-node other-index height)))
       (walk (node-ref node bi)
             (node-ref other-node other-bi)
             si
             other-si
             (fx- height 1)
             (fx- height 1))])))

(define (treelist-hash-code tl recur)
  ;; limit the number of elements that we inspect to 48
  (define len (treelist-size tl))
  (cond
    [(len . fx< . 48)
     (for/fold ([hc 0]) ([elem (in-treelist tl)])
       (hash-code-combine hc (recur elem)))]
    [else
     (hash-code-combine
      ;; first 16
      (for/fold ([hc 0]) ([i (in-range 0 16)])
        (hash-code-combine hc (recur (treelist-ref tl i))))
      ;; sparse middle 16
      (let* ([n (fx- len 32)]
             [skip (quotient n 16)])
        (for/fold ([hc 0]) ([i (in-range 16 (fx- len 16) skip)])
          (hash-code-combine hc (recur (treelist-ref tl i)))))
      ;; last 16
      (for/fold ([hc 0]) ([i (in-range (fx- len 16) len)])
        (hash-code-combine hc (recur (treelist-ref tl i)))))]))

;; functionally update the slot at `index` to `el`
(define (treelist-set tl index el)
  (cond
    [(impersonator? tl)
     (treelist-set/slow tl index el)]
    [else
     (check-treelist 'treelist-set tl)
     (define size (treelist-size tl))
     (check-treelist-index 'treelist-set size index)
     (define height (treelist-height tl))
     (define new-node
       (let set ([node (treelist-root tl)]
                 [index index]
                 [el el]
                 [height height])
         (cond
           [(fx= height 0)
            (node-set node (radix index height) el)]
           [(node-leftwise-dense? node)
            (define branch-index (radix index height))
            (node*-set node branch-index (set (node*-ref node branch-index) index el (fx- height 1)))]
           [else
            (define-values (branch-index subindex) (step node index height))
            (node-set node branch-index (set (node-ref node branch-index) subindex el (fx- height 1)))])))
     (treelist new-node size height)]))

;; add `el` to end of vector
(define (treelist-add tl el)
  (cond
    [(impersonator? tl)
     (treelist-add/slow tl el)]
    [else
     (check-treelist 'treelist-set tl)
     (define size (treelist-size tl))
     (cond
       [(fx= size 0)
        (treelist (leaf el) 1 0)]
       [else
        (define root (treelist-root tl))
        (define height (treelist-height tl))
        (define new-root (build root height el))
        (if new-root
            ;; enough space in original tree
            (treelist new-root (fx+ size 1) height)
            ;; not enough space in original tree
            (treelist (Node (vector root
                                    (new-branch el height)))
                      (fx+ size 1)
                      (fx+ height 1)))])]))

(define (treelist->list tl)
  (check-treelist 'treelist->list tl)
  (for/list ([el (in-treelist tl)])
    el))

(define (list->treelist lst)
  ;; build a dense tree of vectors directly
  (define vec (list->vector lst))
  (define size (vector*-length vec))
  (define-values (root height)
    (cond
      [(size . fx<= . MAX_WIDTH) (values (Node vec) 0)]
      [else
       (define height (fx- (fxquotient (fx+ (integer-length (fx- size 1)) BITS -1) BITS) 1))
       (values
        (let loop ([start 0] [height height])
          (cond
            [(fx= height 0)
             (Node (vector*-copy vec start (fxmin size (fx+ start MAX_WIDTH))))]
            [else
             (define width (fxlshift MAX_WIDTH (fx* height BITS)))
             (define end (fxmin size (fx+ width start)))
             (define step (fxrshift width BITS))
             (define len (fxquotient (fx+ (fx- end start) (fx- step 1)) step))
             (Node (for/vector #:length len ([start (in-range start end step)])
                      (loop start (fx- height 1))))]))
        height)]))
  (treelist root size height))

(define (treelist-length treelist)
  (treelist-size treelist))

;; trees that are a result of this method may not meet invariants, but rebalancing is costly
;; and future concatenations would restore the invariants due to rebalancing being done on concats.
;; TODO write some tests showing this
(define (treelist-take tl pos)
  (cond
    [(impersonator? tl)
     (treelist-take/slow tl pos)]
    [else
     (check-treelist 'treelist-take tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-take size pos)
     (cond
       [(fx= pos 0)
        empty-treelist]
       [(fx= pos size)
        tl]
       [else
        (define height (treelist-height tl))
        (define new-root
          (let take ([node (treelist-root tl)]
                     [index (fx- pos 1)]
                     [height height])
            (cond
              [(fx= height 0)
               (Node (vector*-take (node-children node) (fx+ (radix index 0) 1)))]
              [(node-leftwise-dense? node)
               (define branch-index (radix index height))
               (define children (node*-children node))
               (define new-child (take (vector*-ref children branch-index) index (fx- height 1)))
               (define new-children (vector*-take children (fx+ branch-index 1)))
               (vector*-set! new-children branch-index new-child)
               (Node new-children)]
              [else
               (define-values (branch-index subindex) (step node index height))
               (define children (node-children node))
               (define new-child (take (vector*-ref children branch-index) subindex (fx- height 1)))
               (define new-children (vector*-take children (fx+ branch-index 1)))
               (vector*-set! new-children branch-index new-child)
               (cond
                 [(fx= 1 (vector*-length new-children))
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  ;; it's possible that we drop off a non-dense part and end up leftwise dense, but we don't try to check
                  (define new-sizes (vector*-take (node-sizes node) (fx+ branch-index 1)))
                  (vector*-set! new-sizes branch-index (fx+ index 1))
                  (Node new-children new-sizes)])])))
        (squash new-root pos height)])]))

(define (treelist-drop tl pos)
  (cond
    [(impersonator? tl)
     (treelist-drop/slow tl pos)]
    [else
     (check-treelist 'treelist-drop tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-drop size pos)
     (cond
       [(fx= pos 0)
        tl]
       [(fx= pos size)
        empty-treelist]
       [else
        (define height (treelist-height tl))
        (define new-root
          (let drop ([node (treelist-root tl)]
                     [index pos]
                     [height height])
            (cond
              [(fx= height 0)
               (Node (vector*-drop (node-children node) (radix index 0)))]
              [(node-leftwise-dense? node)
               (define branch-index (radix index height))
               (define children (node*-children node))
               (define new-child (drop (vector*-ref children branch-index) index (fx- height 1)))
               (define new-children (vector*-drop children branch-index))
               (vector*-set! new-children 0 new-child)

               (define new-len (fx- (node-size node) branch-index))
               (cond
                 [(fx= new-len 1)
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  (define size0 (size-subtree new-child (fx- height 1)))
                  (define step (fxlshift 1 (fx* height BITS)))
                  (cond
                    [(fx= size0 step)
                     ;; we dropped complete subtress to stay leftwise dense
                     (Node new-children)]
                    [else
                     (define new-sizes (make-vector new-len size0))
                     (for ([i (in-range 0 (fx- new-len 1))])
                       (vector*-set! new-sizes i (fx+ size0 (fx* i step))))
                     (define sizeN (size-subtree (vector*-ref new-children (fx- new-len 1)) (fx- height 1)))
                     (vector*-set! new-sizes (fx- new-len 1) (fx+ size0 (fx* (fx- new-len 2) step) sizeN))
                     (Node new-children new-sizes)])])]
              [else
               (define-values (branch-index subindex) (step node index height))
               (define children (node-children node))
               (define new-child (drop (vector*-ref children branch-index) subindex (fx- height 1)))
               (define new-children (vector*-drop children branch-index))
               (vector*-set! new-children 0 new-child)
               (define old-len (vector*-length (node-sizes node)))
               (define new-len (fx- old-len branch-index))
               (cond
                 [(fx= new-len 1)
                  ;; one child => leftwise dense
                  (Node new-children)]
                 [else
                  ;; it's possible that the result is leftwise dense, but we don't try to check
                  (define new-sizes (for/vector #:length new-len
                                                ([i (in-range branch-index old-len)])
                                                (fx- (vector*-ref (node-sizes node) i) index)))
                  (Node new-children new-sizes)])])))
        (squash new-root (fx- size pos) height)])]))

(define (treelist-split tl at)
  (check-treelist 'treelist-split tl)
  (check-treelist-end-index 'treelist-split (treelist-size tl) at)
  (cond
    [(fx= at 0) (values empty-treelist tl)]
    [(fx= at (treelist-size tl)) (values tl empty-treelist)]
    [else (values (treelist-take tl at) (treelist-drop tl at))]))

(define (treelist-insert tl at el)
  (cond
    [(impersonator? tl)
     (treelist-insert/slow tl at el)]
    [else
     (check-treelist 'treelist-insert tl)
     (define size (treelist-size tl))
     (check-treelist-end-index 'treelist-insert size at)
     (cond
       [(fx= at 0) (treelist-cons tl el)]
       [(fx= at size) (treelist-add tl el)]
       [else (treelist-append (treelist-add (treelist-take tl at) el)
                              (treelist-drop tl at))])]))

(define (treelist-remove tl at)
  (cond
    [(impersonator? tl)
     (treelist-remove/slow tl at)]
    [else
     (check-treelist 'treelist-remove tl)
     (define size (treelist-size tl))
     (check-treelist-index 'treelist-remove size at)
     (cond
       [(fx= at 0) (treelist-drop tl 1)]
       [(fx= at (fx- size 1)) (treelist-take tl at)]
       [else (treelist-append (treelist-take tl at)
                              (treelist-drop tl (fx+ at 1)))])]))

(define (treelist-cons tl el)
  (cond
    [(impersonator? tl)
     (treelist-cons/slow tl el)]
    [else
     (check-treelist 'treelist-cons tl)
     (define size (treelist-size tl))
     (cond
       [(fx= 0 size)
        (treelist (leaf el) 1 0)]
       [else
        ;; insert in leftmost node, if it has space; this
        ;; will always work for small lists
        (define height (treelist-height tl))
        (define new-root
          (let insert-left ([a (treelist-root tl)]
                            [height height])
            (cond
              [(fx= height 0)
               (and ((node-size a) . < . MAX_WIDTH)
                    (Node (vector*-add-left el (node-children a))))]
              [else
               (define left (insert-left (vector*-ref (node-children a) 0) (fx- height 1)))
               (and left
                    (Node (vector*-set/copy (node-children a) 0 left)
                          (let ([sizes (node-sizes a)])
                            (for/vector #:length (vector*-length sizes) ([n (in-vector sizes)])
                                        (fx+ n 1)))))])))
        (cond
          [new-root
           (treelist new-root (fx+ size 1) height)]
          [else
           (treelist-append (treelist (leaf el) 1 0) tl)])])]))

(define (treelist-append tl rhs)
  (cond
    [(or (impersonator? tl)
         (impersonator? rhs))
     (treelist-append/slow tl rhs)]
    [else
     (check-treelist 'treelist-append tl)
     (check-treelist 'treelist-append rhs)
     (cond
       [(fx= 0 (treelist-size tl)) rhs]
       [(fx= 0 (treelist-size rhs)) tl]
       [else
        (define-values (new-children new-height)
          (concat-subtree (treelist-root tl)
                          (treelist-height tl)
                          (treelist-root rhs)
                          (treelist-height rhs)))
        (treelist new-children
                  (fx+ (treelist-size tl)
                       (treelist-size rhs))
                  new-height)])]))

;; after take or drop, squash tree if it can be shorter:
(define (squash node new-size new-height)
  (cond
    [(and (fx= (node-size node) 1)
          (fx> new-height 0))
     (squash (node-first node) new-size (fx- new-height 1))]
    [else
     (treelist node new-size new-height)]))

;; result height is either max of two heights or one more
;; than the max of the heights
(define (concat-subtree left
                        height-l
                        right
                        height-r)
  ;; only trees of the same height can be concatenated
  (cond
    [(fx> height-l height-r)
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       right
                       height-r))
     (rebalance left
                mid
                #f
                height-l
                height-m)]
    [(fx< height-l height-r)
     (define-values (mid height-m)
       (concat-subtree left
                       height-l
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance #f
                mid
                right
                height-r
                height-m)]
    [(fx= height-l 0)
     (cond
       [(fx<= (fx+ (node-size left) (node-size right)) MAX_WIDTH)
        (values (Node (vector*-append (node-children left) (node-children right)))
                0)]
       [else
        (values (Node (vector (node-children left) (node-children right))
                      (vector (node-size left) (fx+ (node-size left) (node-size right))))
                1)])]
    [else
     ;; two internal nodes with same height
     (define-values (mid height-m)
       (concat-subtree (node-last left)
                       (fx- height-l 1)
                       (node-first right)
                       (fx- height-r 1)))
     (rebalance left
                mid
                right
                height-l
                height-m)]))

;; keeps all but last of `left`, all but first of `right`,
;; and all of `center`; height is the same for `left` and
;; `right`, which `center` height might be one less; height
;; is at least 1; the resulting height grows by either 0 or 1
(define (rebalance left
                   center
                   right
                   height
                   height_c)
  (define all-slots (merge-nodes left
                                 (if (fx< height_c height)
                                     (Node (vector center))
                                     center)
                                 right))
  (define plan (concat-plan all-slots))
  (define rebalanced-slots (exec-concat-plan all-slots plan height))

  (cond
    [(fx<= (vector*-length rebalanced-slots) MAX_WIDTH)
     (values (set-sizes rebalanced-slots height)
             height)]
    [else
     (define new-left (vector*-take rebalanced-slots MAX_WIDTH))
     (define new-right (vector*-drop rebalanced-slots MAX_WIDTH))
     (values (set-sizes (vector (set-sizes new-left height)
                                (set-sizes new-right height))
                        (fx+ height 1))
             (fx+ height 1))]))

;; merge all children except for the rightmost in `left` and leftmost in `right`
(define (merge-nodes left center right)
  (vector*-append (if (not left) (vector) (vector*-drop-right (node-children left) 1))
                  (node-children center)
                  (if (not right) (vector) (vector*-drop (node-children right) 1))))

;; TODO how to avoid setting sizes when the tree is leftwise dense?
(define (set-sizes children height)
  (cond
    [(fx= height 0)
     (Node children)]
    [else
     (define sizes (make-vector (vector*-length children)))
     (define mask (fx- (fxlshift 1 (fx* height BITS)) 1))
     (define-values (sum leftwise-dense?)
       (for/fold ([sum 0] [leftwise-dense? #t]) ([i (in-range 0 (vector*-length children))])
         (define new-sum (fx+ sum (size-subtree (vector*-ref children i) (fx- height 1))))
         (vector*-set! sizes i new-sum)
         (values new-sum (and leftwise-dense? (fx= 0 (fxand sum mask))))))
     (if leftwise-dense?
         (Node children)
         (Node children sizes))]))

;; TODO redesign this to be less imperative?
;; receives a node that is temporarily allowed to have > max_width children, redistributes it to conform to invariant
(define (concat-plan slots)
  (define plan (make-vector (vector*-length slots)))
  (define child-count
    (for/fold ([count 0]) ([i (in-range 0 (vector*-length slots))])
      (define sz (node-size (vector*-ref slots i)))
      (vector*-set! plan i sz)
      (fx+ count sz)))

  (define optimal-node-len (fxquotient (fx+ child-count MAX_WIDTH -1) MAX_WIDTH))
  (define target-node-len (fx+ optimal-node-len MAX_ERROR))
  
  (if (fx>= target-node-len (vector*-length plan))
      #false
      (distribute plan target-node-len (vector*-length plan))))

(define (distribute plan target count [node-idx 0])
  (cond
    [(fx>= target count)
     (vector*-take plan count)]
    [else
     (define init-i (short-node plan node-idx))
     (define-values (i r)
       (let loop ([i init-i]
                  [r (vector*-ref plan init-i)])
         (cond
           [(fx= r 0)
            (values i r)]
           [else
            (define min-size (min (fx+ r (vector*-ref plan (fx+ i 1))) MAX_WIDTH))
            (vector*-set! plan i min-size)
            (loop (fx+ i 1) (fx- (fx+ r (vector*-ref plan (fx+ i 1))) min-size))])))

     ;; we've removed a node (conceptually) at this point,
     ;; so move nodes to the right of current node left by one
     (for ([j (in-range i (fx- count 1))])
       (vector*-set! plan j (vector*-ref plan (fx+ j 1))))

     (distribute plan target (fx- count 1) (fx- i 1))]))

(define (short-node plan i)
  (if (fx< (vector*-ref plan i) (fx- MAX_WIDTH 1))
      i
      (short-node plan (fx+ i 1))))

(define (exec-concat-plan slots plan height)
  (cond
    [(not plan) slots]
    [else
     (define flattened-size
       (for/fold ([sum 0]) ([node (in-vector slots)])
         (fx+ sum (node-size node))))
     (define flattened
       (for*/vector #:length flattened-size ([node (in-vector slots)]
                                             [child (in-vector (node-children node))])
         child))

     (define new-slots (make-vector (vector*-length plan)))
     (for/fold ([sum 0]) ([i (in-range 0 (vector*-length plan))])
       (define new-sum (fx+ sum (vector*-ref plan i)))
       (define new-node
         (for/vector #:length (fx- new-sum sum)
                     ([j (in-range sum new-sum)])
                     (vector*-ref flattened j)))
       (vector*-set! new-slots i (set-sizes new-node (fx- height 1)))
       new-sum)
     
     new-slots]))

(define (size-subtree node height)
  (cond
    [(fx= height 0)
     (vector*-length (node-children node))]
    [(node-sizes node)
     => (lambda (sizes)
          (vector*-ref sizes (fx- (vector*-length sizes) 1)))]
    [else
     ;; if sizes is #false, then we know we have a leftwise dense subtree
     (fx+ (fxlshift (fx- (node*-size node) 1) (fx* height BITS))
          (size-subtree (node*-last node) (fx- height 1)))]))

;; helper functions

;; calculate next branch to take and subindex of `index` along that path;
;; the returned subindex is always in range for the subtree (i.e., no bits
;; set at `height` radix or above)
(define (step node index height)
  (define sizes (node-sizes node))
  (define target-index (bitwise-and index (fx- (fxlshift 1 (fx* (fx+ height 1) BITS)) 1)))
  (define branch (let loop ([i 0])
                   (if (fx<= (vector*-ref sizes i) target-index)
                       (loop (fx+ i 1))
                       i)))
  (values branch
          (if (fx= branch 0)
              target-index
              (fx- target-index (vector*-ref sizes (fx- branch 1))))))

;; add if there's room, return #false otherwise
(define (build n height el)
  (cond
    [(fx= height 0)
     (if (fx< (node-size n) MAX_WIDTH)
         (Node (vector*-add-right (node-children n) el))
         #false)]
    [else
     (define size (node-size n))
     (define child (and (fx> size 0)
                        (build (node-ref n (fx- size 1)) (fx- height 1) el)))
     (cond
       [child
        (Node (vector*-set/copy (node-children n) (fx- size 1) child)
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-set/copy sizes
                                       (fx- (vector*-length sizes) 1)
                                       (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [(fx< (node-size n) MAX_WIDTH)
        (Node (vector*-add-right (node-children n)
                           (new-branch el (fx- height 1)))
              (let ([sizes (node-sizes n)])
                (and sizes
                     (vector*-add-right sizes
                                        (fx+ (vector*-ref sizes (fx- (vector*-length sizes) 1)) 1)))))]
       [else
        #false])]))

;; create a branch of height `height` terminating in a unary leaf node containing `el`
(define (new-branch el height)
  (if (fx= height 0)
      (leaf el)
      (Node (vector (new-branch el (fx- height 1))))))

(define-values (prop:treelist-chaperone
                treelist-chaperone?
                treelist-chaperone-ref)
  (make-impersonator-property 'treelist))

(struct treelist-wrapper (prev ref set insert append))

(define (chaperone-treelist tl
                            ref-proc
                            set-proc
                            insert-proc
                            append-proc)
  (check-treelist 'in-treelist tl)
  (chaperone-struct tl
                    struct:treelist
                    prop:treelist-chaperone
                    (treelist-wrapper tl
                                      ref-proc
                                      set-proc
                                      insert-proc
                                      append-proc)))

(define (check-chaperone who v old-v)
  (unless (chaperone-of? v old-v)
    (error who "result is not a chaperone"))
  v)

(define (re-chaperone new-prev w)
  (chaperone-struct new-prev
                    struct:treelist
                    prop:treelist-chaperone
                    (struct-copy treelist-wrapper w [prev new-prev])))

(define (treelist-ref/slow tl index)
  (define who 'treelist-ref)
  (check-treelist who tl)
  (check-treelist-index who (treelist-size tl) index)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define v (treelist-ref prev index))
  (check-chaperone who ((treelist-wrapper-ref w) prev index v) v))

(define (treelist-node-for/slow tl index)
  (values (Node (vector (treelist-ref tl index)))
          0))

(define (treelist-equal?/slow tl other-tl recur)
  (define len (treelist-size tl))
  (and (fx= len (treelist-size other-tl))
       (for/and ([i (in-range len)])
         (recur (treelist-ref tl i) (treelist-ref other-tl i)))))

(define (treelist-set/slow tl index el)
  (define who 'treelist-set)
  (check-treelist who tl)
  (check-treelist-index who (treelist-size tl) index)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define new-el
    (check-chaperone who ((treelist-wrapper-set w) prev index el) el))
  (define new-prev (treelist-set prev index new-el))
  (re-chaperone new-prev w))

(define (treelist-insert/slow tl at el)
  (define who 'treelist-insert)
  (check-treelist who tl)
  (define size (treelist-size tl))
  (check-treelist-end-index who size at)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define new-el
    (check-chaperone who ((treelist-wrapper-insert w) prev at el) el))
  (define new-prev (treelist-insert prev at new-el))
  (re-chaperone new-prev w))

(define (treelist-add/slow tl el)
  (check-treelist 'treelist-add tl)
  (treelist-insert/slow tl (treelist-length tl) el))

(define (treelist-cons/slow tl el)
  (check-treelist 'treelist-cons tl)
  (treelist-insert/slow tl 0 el))

(define (treelist-do-slow who op tl at)
  (define who who)
  (check-treelist who tl)
  (define size (treelist-size tl))
  (check-treelist-end-index who size at)
  (define w (treelist-chaperone-ref tl))
  (define prev (treelist-wrapper-prev w))
  (define new-prev (op prev at))
  (re-chaperone new-prev w))

(define (treelist-remove/slow tl at)
  (treelist-do-slow 'treelist-remove treelist-remove tl at))

(define (treelist-take/slow tl pos)
  (treelist-do-slow 'treelist-take treelist-take tl pos))

(define (treelist-drop/slow tl pos)
  (treelist-do-slow 'treelist-drop treelist-drop tl pos))

(define (treelist-append/slow tl rhs)
  (cond
    [(impersonator? tl)
     (define who 'treelist-append)
     (check-treelist who tl)
     (define w (treelist-chaperone-ref tl))
     (define prev (treelist-wrapper-prev w))
     (define new-tl ((treelist-wrapper-append w) prev rhs))
     (unless (treelist? new-tl)
       (error who "result is not a treelist"))
     (re-chaperone new-tl w)]
    [else
     (for/fold ([tl tl]) ([e (in-treelist rhs)])
       (treelist-add tl e))]))
