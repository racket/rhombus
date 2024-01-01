#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum
         racket/vector
         "version-case.rkt")

(provide make-treelist
         treelist-add
         treelist-cons
         treelist-append
         treelist-insert
         treelist-drop
         treelist->list)

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
;;    are fully dense, and its `n`th child is leftwise-dense or fully dense. `n` is allowed to be < `m`
;;  - a node is balanced if it is leftwise dense or fully dense (note that leaves are always at least leftwise dense)
;;  - unbalanced nodes contain a size array `sizes`, balanced nodes do not

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

(define (node-balanced? n) (assert-node n) (not (pair? n)))
(define (node-children n) (assert-node n) (if (pair? n) (car n) n))
(define (node-sizes n) (assert-node n) (and (pair? n) (cdr n)))
(define (node-size n) (assert-node n) (vector*-length (node-children n)))
(define (node-first n) (assert-node n) (vector*-ref (node-children n) 0))
(define (node-last n) (assert-node n) (let ([cs (node-children n)])
                                        (vector*-ref cs (fx- (vector*-length cs) 1))))
(define (node-ref n i) (assert-node n) (vector*-ref (node-children n) i))
(define (node-set n i v) (assert-node n) (vector*-set/copy (node-children n) i v))
(define (node-length n) (assert-node n) (vector*-length (node-children n)))

;; `node*` refers to a balanced node
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
  #:authentic
  #:property prop:equal+hash (list
                              ;; TODO: make faster
                              (lambda (v other recur)
                                (and (fx= (treelist-size v)
                                          (treelist-size other))
                                     (for/and ([a (in-treelist v)]
                                               [b (in-treelist other)])
                                       (recur a b))))
                              ;; TODO: hash only subset
                              (lambda (v recur)
                                (recur (treelist->list v)))
                              ;; TODO: hash only subset
                              (lambda (v recur)
                                (recur (treelist->list v)))))

(define empty-treelist (treelist empty-node 0 0))

(define make-treelist
  (case-lambda
    [() empty-treelist]
    [(a) (treelist (leaf a) 1 0)]
    [(a b) (treelist (Node (vector a b)) 2 0)]
    [(a b c) (treelist (Node (vector a b c)) 3 0)]
    [(a b c . ds) (treelist-add-all (treelist (Node (vector a b c)) 3 0) ds)]))

(define (check-treelist who tl)
  (unless (treelist? tl)
    (raise-argument-error who "treelist?" tl)))

(define (check-treelist-index who tl index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "index out of range: ~v" index)))
  (define size (treelist-size tl))
  (when (or (index . fx< . 0) (index . fx>= . size))
    (error who "index out of range: ~v / ~s" index size)))

(define (check-treelist-end-index who tl index)
  (unless (fixnum? index)
    (if (exact-nonnegative-integer? index)
        (raise-argument-error who "exact-nonnegative-integer?" index)
        (error who "count out of range: ~v" index)))
  (define size (treelist-size tl))
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
                 (let-values ([(node node-pos) (treelist-node-for 'in-treelist tl pos)])
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
  (check-treelist 'treelist-ref tl)
  (check-treelist-index 'treelist-ref tl index)
  (define-values (node pos) (treelist-node-for 'node-ref tl index))
  (node-ref node pos))

(define (treelist-node-for who tl index)
  (define height (treelist-height tl))
  (define size (treelist-size tl))
  (cond
    [(fx= height 0)
     (values (treelist-root tl) index)]
    [else
     (let walk ([node (treelist-root tl)]
                [index index]
                [height height])
       (cond
         [(node-balanced? node)
          (values (let sub ([n node] [height height])
                    (cond
                      [(fx= height 0) n]
                      [else (sub (node*-ref n (radix index height))
                                 (fx- height 1))]))
                  (bitwise-and index MASK))]
         [(fx= height 1)
          (define-values (bi si) (step node index height))
          (values (node-ref node bi) (bitwise-and si MASK))]
         [else
          (define-values (bi si) (step node index height))
          (walk (node-ref node bi) si (fx- height 1))]))]))

;; functionally update the slot at `index` to `el`
(define (treelist-set tl index el)
  (check-treelist 'treelist-set tl)
  (check-treelist-index 'treelist-set tl index)
  (define new-node
    (let set ([node (treelist-root tl)]
              [index index]
              [el el]
              [height (treelist-height tl)])
      (cond
        [(fx= height 0)
         (node-set node (radix index height) el)]
        [(node-balanced? node)
         (define branch-index (radix index height))
         (node*-set node branch-index (set (node*-ref node branch-index) index el (fx- height 1)))]
        [else
         (define-values (branch-index subindex) (step node index height))
         (node-set node branch-index (set (node-ref node branch-index) subindex el (fx- height 1)))])))
  (treelist new-node (treelist-size tl) (treelist-height tl)))

;; add `el` to end of vector
(define (treelist-add tl el)
  (check-treelist 'treelist-set tl)
  (define size (treelist-size tl))
  (cond
    [(fx= size 0)
     (treelist (leaf el) 1 0)]
    [else
     (define new-root (build (treelist-root tl) (treelist-height tl) el))
     (if new-root
         ;; enough space in original tree
         (treelist new-root (fx+ size 1) (treelist-height tl))
         ;; not enough space in original tree
         (treelist (Node (vector (treelist-root tl)
                                (new-branch el (treelist-height tl))))
                  (fx+ size 1)
                  (fx+ (treelist-height tl) 1)))]))

;; TODO chunk adding here by 32 and add whole nodes at a time?
(define (treelist-add-all tl els)
  (check-treelist 'treelist-add-all tl)
  (for/fold ([tl tl]) ([el (in-list els)])
    (treelist-add tl el)))

(define (treelist->list tl)
  (check-treelist 'treelist->list tl)
  (for/list ([el (in-treelist tl)])
    el))

(define (treelist-length treelist)
  (treelist-size treelist))

;; trees that are a result of this method may not meet invariants, but rebalancing is costly
;; and future concatenations would restore the invariants due to rebalancing being done on concats.
;; TODO write some tests showing this
(define (treelist-take tl pos)
  (check-treelist 'treelist-take tl)
  (check-treelist-end-index 'treelist-take tl pos)
  (cond
    [(fx= pos 0)
     empty-treelist]
    [(fx= pos (treelist-size tl))
     tl]
    [else
     (define new-root
       (let take ([node (treelist-root tl)]
                  [index (fx- pos 1)]
                  [height (treelist-height tl)])
         (cond
           [(fx= height 0)
            (Node (vector*-take (node-children node) (fx+ (radix index 0) 1)))]
           [(node-balanced? node)
            (define branch-index (radix index height))
            (define new-children (vector*-take (node*-children node) (fx+ branch-index 1)))
            (vector*-set! new-children branch-index (take (vector*-ref new-children branch-index) index (fx- height 1)))
            (Node new-children)]
           [else
            (define-values (branch-index subindex) (step node index height))
            (define new-children (vector*-take (node-children node) (fx+ branch-index 1)))
            (define new-sizes (vector*-take (node-sizes node) (fx+ branch-index 1)))
            (vector*-set! new-children branch-index (take (node-ref new-children branch-index) subindex (fx- height 1)))
            (vector*-set! new-sizes branch-index (fx+ index 1))
            (Node new-children new-sizes)])))
     (squash new-root pos (treelist-height tl))]))

(define (treelist-drop tl pos)
  (check-treelist 'treelist-drop tl)
  (check-treelist-end-index 'treelist-drop tl pos)
  (cond
    [(fx= pos 0)
     tl]
    [(fx= pos (treelist-size tl))
     empty-treelist]
    [else
     (define new-root
       (let drop ([node (treelist-root tl)]
                  [index pos]
                  [height (treelist-height tl)])
         (cond
           [(fx= height 0)
            (Node (vector*-drop (node-children node) (radix index 0)))]
           [(node-balanced? node)
            (define branch-index (radix index height))
            (define new-children (vector*-drop (node*-children node) branch-index))
            (define new-child (drop (node*-ref node branch-index) index (fx- height 1)))
            (vector*-set! new-children 0 new-child)

            (define size0 (size-subtree new-child (fx- height 1)))
            (define new-len (fx- (node-size node) branch-index))
            (define new-sizes (make-vector new-len size0))

            (cond
              [(fx= new-len 1)
               (void)]
              [else
               (define step (fxlshift 1 (fx* height BITS)))
               (for ([i (in-range 0 (fx- new-len 1))])
                 (vector*-set! new-sizes i (fx+ size0 (fx* i step))))
               (define sizeN (size-subtree (vector*-ref new-children (fx- new-len 1)) (fx- height 1)))
               (vector*-set! new-sizes (fx- new-len 1) (fx+ size0 (fx* (fx- new-len 2) step) sizeN))
               (Node new-children new-sizes)])]
           [else
            (define-values (branch-index subindex) (step node index height))
            (define new-children (vector*-drop (node-children node) branch-index))
            (define old-len (vector*-length (node-sizes node)))
            (define new-sizes (for/vector #:length (- old-len branch-index)
                                          ([i (in-range branch-index old-len)])
                                          (fx- (vector*-ref (node-sizes node) i) index)))
            (define new-child (drop (node-ref node branch-index) subindex (fx- height 1)))
            (vector*-set! new-children 0 new-child)
            (Node new-children new-sizes)])))
     (squash new-root (fx- (treelist-size tl) pos) (treelist-height tl))]))

(define (treelist-split tl at)
  (check-treelist 'treelist-split tl)
  (check-treelist-end-index 'treelist-split tl at)
  (cond
    [(fx= at 0) (values empty-treelist tl)]
    [(fx= at (treelist-size tl)) (values tl empty-treelist)]
    [else (values (treelist-take tl at) (treelist-drop tl at))]))

(define (treelist-insert tl at el)
  (check-treelist 'treelist-insert tl)
  (check-treelist-end-index 'treelist-insert tl at)
  (cond
    [(fx= at 0) (treelist-cons tl el)]
    [(fx= at (treelist-size tl)) (treelist-add tl el)]
    [else (treelist-append (treelist-add (treelist-take tl at) el)
                           (treelist-drop tl at))]))

(define (treelist-cons tl el)
  (check-treelist 'treelist-insert tl)
  (cond
    [(fx= 0 (treelist-size tl))
     (treelist (leaf el) 1 0)]
    [else
     ;; insert in leftmost node, if it has space; this
     ;; will always work for small lists
     (define new-root
       (let insert-left ([a (treelist-root tl)]
                         [height (treelist-height tl)])
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
        (treelist new-root (fx+ (treelist-size tl) 1) (treelist-height tl))]
       [else
        (treelist-append (treelist (leaf el) 1 0) tl)])]))

(define (treelist-append tl rhs)
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
              new-height)]))


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
     (for/fold ([sum 0]) ([i (in-range 0 (vector*-length children))])
       (define new-sum (fx+ sum (size-subtree (vector*-ref children i) (fx- height 1))))
       (vector*-set! sizes i new-sum)
       new-sum)
     (Node children sizes)]))

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
     ;; if sizes is #false, then we know we have a leftwise-dense subtree
     (fx+ (fxlshift (fx- (node*-size node) 1) (fx* height BITS))
          (size-subtree (node*-last node) (fx- height 1)))]))

;; helper functions

(define (scan-sizes sizes target-index [i 0])
  (if (fx<= (vector*-ref sizes i) target-index)
      (scan-sizes sizes target-index (fx+ i 1))
      i))

;; calculate next branch to take and subindex of `index` along that path
(define (step node index height)
  (define sizes (node-sizes node))
  (define branch (scan-sizes sizes index (radix index height)))
  (values branch
          (if (fx= branch 0)
              index
              (fx- index (vector*-ref sizes (fx- branch 1))))))

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
