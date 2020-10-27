#lang wraith racket

require "../read-wraith-syntax.rkt"
module+ test
  require rackunit
  ;; the first is the expected, rest are actual
  define-check (check-sexprs-same sexprs)
    match-define (cons expected actuals) sexprs
    for [actual (in-list actuals)]
      check-equal? actual expected

module+ test
  check-sexprs-same
    '[(define (conversion f)
        (* 5/9 (- f 32)))
      define (conversion f)
        * 5/9 (- f 32)
      define (conversion f)
        * 5/9
          - f 32
      define (conversion f)
        {5/9 * {f - 32}}
      ; and even though it's bad style:
      (define (conversion f)
        (* 5/9 (- f 32)
          )
        )]
  check-sexprs-same
    '[(define modes
        '(edit
          help
          debug
          test
          trace
          step))
      (define modes
        '(edit
          help
          debug
          test
          trace
          step
          ))
      define modes
        '(edit
          help
          debug
          test
          trace
          step
          )]
  check-sexprs-same
    '[(define turn%
        (class object%
          (init-field state)
 
          (super-new)
 
          (define/public (place where tile)
            (send state where tile))
 
          (define/public (is-placable? place)
            (send state legal? place))))
      (define turn%
        (class object%
          (init-field state)
 
          (super-new)
 
          (define/public (place where tile)
            (send state where tile))
 
          (define/public (is-placable? place)
            (send state legal? place))
          ))
      (define turn%
        (class object%
          init-field state
 
          (super-new)
 
          define/public (place where tile)
            send state where tile
 
          define/public (is-placable? place)
            send state legal? place
          ))
      define turn%
        class object%
          init-field state
 
          (super-new)
 
          define/public (place where tile)
            send state where tile
 
          define/public (is-placable? place)
            send state legal? place]
  check-sexprs-same
    '[(if (positive? (rocket-x r))
          (launch r)
          (redirect (- x)))
      if (positive? (rocket-x r))
         launch r
         redirect (- x)
      ; and even though it's bad style:
      (if (positive? (rocket-x r))
          (launch r)
        (redirect (- x)))
      if (positive? (rocket-x r))
         launch r
        redirect (- x)
      ; also even though it's bad style:
      (if (positive? (rocket-x r)) (launch r)
          (redirect (- x)))
      if (positive? (rocket-x r)) (launch r)
         redirect (- x)]
  check-sexprs-same
    '[(if (positive? x) x (- x))
      if (positive? x) x (- x)]
  check-sexprs-same
    '[(define (launch x)
        (define w 9)
        (define h 33)
        ...)
      define (launch x)
        define w 9
        define h 33
        ...
      ; and even though it's bad style:
      (define (launch x)
        (define w 9) & (define h 33)
        ...)
      define (launch x)
        define w 9 & define h 33
        ...]
  check-sexprs-same
    '[(place-image img 10 10 background)
      place-image img 10 10 background
      (place-image img
                   10
                   10
                   background)
      place-image img
                  10
                  10
                  background
      ; and even though it's bad style:
      (place-image img
                   10 & 10 & background)
      place-image img
                  10 & 10 & background]
  check-sexprs-same
    '[(above img
             (- width  hdelta)
             (- height vdelta)
             bg)
      above img
            - width  hdelta
            - height vdelta
            bg
      above img
            {width - hdelta}
            {height - vdelta}
            bg
      ; and even though it's bad style:
      (above img
             (- width hdelta) & (- height vdelta) & bg)
      above img
            - width hdelta & - height vdelta & bg
      above img
            {width - hdelta} & {height - vdelta} & bg]
  check-sexprs-same
    '[(above ufo 10 v-delta bg)
      (above ufo 10 v-delta
             bg)
      (above ufo
             10
             v-delta
             bg)
      ; and even though it's bad style:
      (above ufo
             10 & v-delta & bg)
      above ufo
            10 & v-delta & bg]
  check-sexprs-same
    '[(overlay/offset (rectangle 100 10 "solid" "blue")
                      10
                      10
                      (rectangle 10 100 "solid" "red"))
      overlay/offset
        rectangle 100 10 "solid" "blue"
        10
        10
        rectangle 10 100 "solid" "red"
      ; this is the exception to the guideline about each argument
      ; after the first line belonging on its own line:
      ; both conceptually related and short, separated by `&` ampersand
      (overlay/offset (rectangle 100 10 "solid" "blue")
                      10 & 10
                      (rectangle 10 100 "solid" "red"))
      overlay/offset
        rectangle 100 10 "solid" "blue"
        10 & 10
        rectangle 10 100 "solid" "red"]
  check-sexprs-same
    '[(define (f x g)
        (cond
          ; note [] square brackets would not work here
          ((< x 3) (g (g 3)))))
      (define (f x g)
        (cond
          {x < 3} (g (g 3))))
      define (f x g)
        cond
          {x < 3} (g (g 3))
      ; and even though it's bad style:
      (define(f x g)
        (cond
          ((< x 3)(g(g 3)))))
      define(f x g)
       cond
        {x < 3}(g(g 3))]
