#lang racket/base
(require syntax-color/racket-lexer
         "../lex.rkt"
         "../lex-comment.rkt"
         "input.rkt")

(define slow? #f)

(define (make-key pos status) (cons pos status))
(define (key-pos k) (car k))
(define (key-status k) (cdr k))
(struct token (type paren span backup) #:transparent)

;; returns (values results memo)
(define (lex-all/incremental str offset memo init-status
                             #:check-memo? [check-memo? #f])
  (define in (open-input-string str))
  (port-count-lines! in)
  (let loop ([status init-status])
    (define-values (line col cpos) (port-next-location in))
    (define pos (+ (sub1 cpos) offset))
    (cond
      [(and (not check-memo?)
            (hash-ref memo (cons pos status) #f))
       => (lambda (r)
            (values r (for/hash ([(k v) (in-hash memo)]
                                 #:when ((key-pos k) . >= . pos))
                        (values k v))))]
      [else
       (when check-memo?
         (unless (hash-ref memo (cons pos status) #f)
           (error "not found in memo:" pos status)))
       (define-values (r type paren start-pos end-pos backup new-status)
         (lex/comment/status in offset status racket-lexer*/status))
       (define tok (token type paren (and end-pos (- end-pos start-pos)) backup))
       (cond
         [(eq? type 'eof)
          (define r (list tok))
          (values r (hash (cons pos status) r))]
         [else
          (define-values (new-r new-memo) (loop new-status))
          (define r (cons tok new-r))
          (values r
                  (hash-set new-memo (cons pos status) r))])])))

;; dumb, linear-time token lookup
(define (find-token memo pos)
  (cond
    [(zero? pos)
     ;; can't go backward
     (define r (hash-ref memo (cons 0 #f) (lambda () (error "no zero token?"))))
     (values (car r) 0 #f)]
    [else
     ;; if `pos` is on a boundary, get token just before
     (for/fold ([tok #f] [tok-pos #f] [tok-status #f]) ([(k v) (in-hash memo)])
       (define v-tok (car v))
       (define v-pos (key-pos k))
       (cond
         [(v-pos . < . pos)
          (if (or (not tok)
                  (v-pos . > . tok-pos))
              (values v-tok v-pos (key-status k))
              (values tok tok-pos tok-status))]
         [else (values tok tok-pos tok-status)]))]))

(define (find-token/backup memo pos)
  (define-values (tok tok-pos tok-status) (find-token memo pos))
  (define backup (token-backup tok))
  (cond
    [(or (zero? backup) (zero? tok-pos))
     (values tok tok-pos tok-status)]
    [else (find-token/backup memo (- pos backup))]))

;; dumb, linear-time adjustment of a memo table
(define (shift-memo memo at-pos delta backup-pos)
  (define new-memo
    (for/hash ([(k v) (in-hash memo)])
      (define pos (key-pos k))
      (define status (key-status k))
      (cond
        [(pos . < . backup-pos)
         (values k v)]
        [((+ pos delta) . < . at-pos)
         ;; (log-error "drop(~s, ~s) ~s ~s" at-pos delta k (car v))
         ;; drop it
         (values #f #f)]
        [else
         (values (cons (+ pos delta) status) v)])))
  (hash-remove new-memo #f))

(define (try-delta pos delta new-input input memo1 all1)
  (define (fragment s2) (substring s2 0 (min 8 (string-length s2))))
  (define-values (tok tok-pos tok-status) (find-token/backup memo1 pos))
  (define prefix (let loop ([all1 all1])
                   (cond
                     [(eq? tok (car all1)) null]
                     [else (cons (car all1) (loop (cdr all1)))])))
  ;; (log-error "~s => ~s ~s" pos tok-pos tok)
  (define shifted-memo (shift-memo memo1 pos delta tok-pos))
  (define re-input (substring new-input tok-pos))
  (define-values (re-all-rest re-memo)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (log-error "failing at ~s ~s ~s" pos (fragment re-input) delta)
                                 (raise exn))])
      (lex-all/incremental re-input tok-pos shifted-memo tok-status)))
  (define re-all (append prefix re-all-rest))
  (define-values (new-all new-memo) (lex-all/incremental new-input 0 #hash() #f))
  (unless (equal? re-all new-all)
    (let loop ([re-all re-all] [new-all new-all] [count 0] [span 0])
      (cond
        [(null? new-all) (log-error "extra re: ~.s" re-all)]
        [(null? re-all) (log-error "extra new: ~.s" new-all)]
        [(equal? (car re-all) (car new-all))
         (loop (cdr re-all) (cdr new-all) (add1 count) (+ span (token-span (car re-all))))]
        [else
         (define CONTEXT-LEN 3)
         (unless (null? prefix)
           (log-error "pre: ~.s" (car (reverse prefix))))
         (log-error " re: ~.s" (car re-all))
         (for ([r (in-list (cdr re-all))]
               [n (in-range CONTEXT-LEN)])
           (log-error "     ~.s" r))
         (log-error "new: ~.s" (car new-all))
         (for ([r (in-list (cdr new-all))]
               [n (in-range CONTEXT-LEN)])
           (log-error "     ~.s" r))
         (error "failed at" 'pos pos 'adj (fragment (substring input pos)) delta
                'restarted tok-pos (fragment re-input) 'at-token count 'at-offset span)])))
  (values new-memo new-all))

(define (try-input which input)
  (printf "incremental ~s\n" which)
  (define-values (all1 memo1) (lex-all/incremental input 0 #hash() #f))
  (define-values (all2 memo2) (lex-all/incremental input 0 memo1 #f #:check-memo? #t))
  (unless (equal? all1 all2)
    (error "not deterministic"))
  (define N (sub1 (string-length input)))
  (define (sel-pos i) i)
  ;; try inserting or deleting then re-lexing
  (for ([ins (if slow?
                 '("x" "1" "$" ")" "'" " ")
                 '("x"))])
    (for ([i (in-range N)])
      (define pos (sel-pos i))

      ;; delte char
      (define new-input- (string-append (substring input 0 pos)
                                        (substring input (+ pos 1))))
      (define-values (memo- all-)
        (try-delta pos -1 new-input- input memo1 all1))

      ;; insert char
      (define new-input+ (string-append (substring input 0 pos)
                                        ins
                                        (substring input pos)))
      (try-delta pos 1 new-input+ input memo1 all1)

      ;; reinsert deleted char
      (try-delta pos 1 input new-input- memo- all-))))

(for ([input1 (in-list input1s)]
      [i (in-naturals)])
  (try-input (format "1[~a]" i) input1))
(try-input '1a input1a)
(try-input '1b input1b)
(try-input 2 input2)
(try-input 3 input3)
(try-input 4 input4)
(try-input 5 input5)

;; make sure in EOF here is ok:
(try-input "open" "@{{")
