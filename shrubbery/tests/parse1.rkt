#lang racket/base
(require "../parse.rkt")

(define input
#<<INPUT

// A set of examples to see what happens with verious forms,
// where many of them seem nonsensical

somthing else: 8

this is a \
  very long linear group \
  that spans multiple lines

this is | one: a \
                 long result
        | two \
            also long
        | three:
            result \
              nested \
              here

this is \
  the first group
this \ is \ the \ second \ group

this is a group \  with (a,
                         nested,
                         list)

this is a group \
  with (a,
                         nested,
                         list)

this is a group \
 with (a,
                \
       nested,
                \
       list)

this is a group \
 with (a,
                \
       /* this a comment on `nested`: */
       nested,
                \
       list)

hello | a | c\
 :
               d

this: \
      is more
             foo

foo
 | more \
 | again:
              sub

something +
more stuff

something:
  more stuff

define
 | fib(0) = 0
 | fib(1) = 1
 | fib(n) =
   fib(n-1) + fib(n-2)

define
 | fib(0) = 0
 | fib(1) = 1
 | fib(n) = fib(n-1) + fib(n-2)
 | more

nonsense:
  hello | there 4.5
        | more f(8)
          next (ok |
                   | stuff: (begin:
                               more
                               things
                             ,
                             separately)
                            again)
          something

q:
  x; y
z

q:
  x; y;
z

q:
  w:
    x; y;
z

x + w | z : :
              y

let | x = 8
    | y = 9: 10
             let | x = 8
    | y = 9: 10
             show(x + y)
             x - y

letrec_syntax_and_values:
  (m = (syntax_rules ....),
   n = (syntax_rules ....)):
    (x = 10,
     y = 12)
    => body
    more

x something | a
              y:
                w:
                  q
            | c
              z
              & b

x | indentize
    y
    z
  | indentize:
      y

define fib(n):
 match n
  | 0
    : 0
  | 1
    : 1
  | n
    : fib(n-1)
      + fib(n-2)
      more

begin:
  dictionary = [
    "foo" : 17,
    "bar" : "string",
    "baz" : #true
  ]

cond | null: 0 | list(x): 1
     | cons(a, d): f(a) + f(d)
INPUT
  )

(define expected
  '(top
    (group somthing else (block (group 8)))
    (group this is a very long linear group that spans multiple lines)
    (group
     this
     is
     (alts
      (block (group one (block (group a long result))))
      (block (group two also long))
      (block (group three (block (group result nested here))))))
    (group this is the first group)
    (group this is the second group)
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group this is a group with (parens (group a) (group nested) (group list)))
    (group hello (alts (block (group a)) (block (group c (block (group d))))))
    (group this (block (group is more) (group foo)))
    (group
     foo
     (alts (block (group more)) (block (group again (block (group sub))))))
    (group something (op +))
    (group more stuff)
    (group something (block (group more stuff)))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (op =) 0))
      (block (group fib (parens (group 1)) (op =) 1))
      (block
       (group fib (parens (group n)) (op =))
       (group
        fib
        (parens (group n (op -) 1))
        (op +)
        fib
        (parens (group n (op -) 2))))))
    (group
     define
     (alts
      (block (group fib (parens (group 0)) (op =) 0))
      (block (group fib (parens (group 1)) (op =) 1))
      (block
       (group
        fib
        (parens (group n))
        (op =)
        fib
        (parens (group n (op -) 1))
        (op +)
        fib
        (parens (group n (op -) 2))))
      (block (group more))))
    (group
     nonsense
     (block
      (group
       hello
       (alts
        (block (group there 4.5))
        (block
         (group more f (parens (group 8)))
         (group
          next
          (parens
           (group
            ok
            (alts
             (block)
             (block
              (group
               stuff
               (block
                (group
                 (parens
                  (group begin (block (group more) (group things)))
                  (group separately)))
                (group again))))))))
         (group something))))))
    (group q (block (group x) (group y)))
    (group z)
    (group q (block (group x) (group y)))
    (group z)
    (group q (block (group w (block (group x) (group y)))))
    (group z)
    (group x (op +) w (alts (block (group z (block (group (block (group y))))))))
    (group
     let
     (alts
      (block (group x (op =) 8))
      (block
       (group
        y
        (op =)
        9
        (block (group 10) (group let (alts (block (group x (op =) 8)))))))
      (block
       (group
        y
        (op =)
        9
        (block
         (group 10)
         (group show (parens (group x (op +) y)))
         (group x (op -) y))))))
    (group
     letrec_syntax_and_values
     (block
      (group
       (parens
        (group m (op =) (parens (group syntax_rules (op ....))))
        (group n (op =) (parens (group syntax_rules (op ....)))))
       (block
        (group (parens (group x (op =) 10) (group y (op =) 12)))
        (group (op =>) body)
        (group more)))))
    (group
     x
     something
     (alts
      (block (group a) (group y (block (group w (block (group q))))))
      (block (group c) (group z) (group (op &) b))))
    (group
     x
     (alts
      (block (group indentize) (group y) (group z))
      (block (group indentize (block (group y))))))
    (group
     define
     fib
     (parens (group n))
     (block
      (group
       match
       n
       (alts
        (block (group 0) (group (block (group 0))))
        (block (group 1) (group (block (group 1))))
        (block
         (group n)
         (group
          (block
           (group fib (parens (group n (op -) 1)))
           (group (op +) fib (parens (group n (op -) 2)))
           (group more))))))))
    (group
     begin
     (block
      (group
       dictionary
       (op =)
       (brackets
        (group "foo" (block (group 17)))
        (group "bar" (block (group "string")))
        (group "baz" (block (group #t)))))))
 (group
  cond
  (alts
   (block (group null (block (group 0))))
   (block (group list (parens (group x)) (block (group 1))))
   (block
    (group
     cons
     (parens (group a) (group d))
     (block (group f (parens (group a)) (op +) f (parens (group d))))))))))

(let ([in (open-input-string input)])
  (port-count-lines! in)
  (unless (equal? expected (syntax->datum (parse-all in)))
    (error "failed")))


