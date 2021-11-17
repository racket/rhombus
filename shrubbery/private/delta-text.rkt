#lang racket/base
(require racket/class)

(provide make-delta-text)

(define delta-text%
  (class object%
    (init-field next    ; the text that this is a delta from
                at-pos  ; the start of a line where whitespace is inserted or deleted
                delta)  ; amount to insert or (when negative) delete

    (super-new)

    ;; No effect before this position:
    (define pre at-pos)
    ;; Simple token shifting after this position (new coordinates):
    (define post (let-values ([(s e) (send next get-token-range pre)])
                   (unless (= pre s) (error "bad delta construction"))
                   (+ e delta)))
    ;; The range from `pre` to `post` is a whitespace token,
    ;; either newly extended to newly truncated

    (define/public (get-text s e)
      (cond
        [(e . <= . pre) (send next get-text s e)]
        [(s . >= . post)
         (send next get-text (- s delta) (- e delta))]
        [(s . < . pre)
         (string-append
          (get-text s pre)
          (get-text pre e))]
        [(e . > . post)
         (string-append
          (get-text s post)
          (get-text post e))]
        [else
         ;; bounds are completely in whitespace region:
         (make-string #\space (- e s))]))

    (define/public (classify-position* pos)
      (cond
        [(pos . < . pre)
         (send next classify-position* pos)]
        [(pos . >= . post)
         (send next classify-position* (- pos delta))]
        [else 'white-space]))
    
    (define/public (classify-position pos)
      (define type (classify-position* pos))
      (if (hash? type) (hash-ref type 'type 'unknown) type))

    (define/public (get-token-range pos)
      (cond
        [(pos . < . pre)
         (send next get-token-range pos)]
        [(pos . >= . post)
         (define-values (s e) (send next get-token-range (- pos delta)))
         (values (+ s delta) (+ e delta))]
        [else (values pre post)]))

    (define/private (shift-in r)
      (if (r . < . pre)
          r
          (max pre (- r delta))))

    ;; biased to the end of inserted whitespace
    (define/private (shift-out r)
      (and r
           (cond
             [(r . <= . pre) r]
             [else (max pre (+ r delta))])))
    
    (define/public (last-position)
      (shift-out (send next last-position)))

    (define/public (position-paragraph pos [eol? #f])
      (send next position-paragraph (shift-in pos) eol?))
    
    (define/public (paragraph-start-position para)
      (shift-out (send next paragraph-start-position para)))
    
    (define/public (paragraph-end-position para)
      (shift-out (send next paragraph-end-position para)))

    (define/public (backward-match pos cutoff)
      (shift-out (send next backward-match (shift-in pos) (shift-in cutoff))))

    (define/public (forward-match pos cutoff)
      (shift-out (send next forward-match (shift-in pos) (shift-in cutoff))))))

(define (make-delta-text t at-pos delta)
  (new delta-text%
       [next t]
       [at-pos at-pos]
       [delta delta]))
