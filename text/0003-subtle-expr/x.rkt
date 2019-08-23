x y z
// (x y z)

x.y.z
// (#%dot (#%dot x y) z)

x.y.5
// (#%dot (#%dot x y) 5)

5.0
// 5.0

x.5.z
// (#%dot (#%dot x 5) z)

x.5.0.z
// (#%dot (#%dot x 5.0) z)


1 + 2 + 3
// (+ 1 2 3)

1 * 3 + 4 * 7 + 6 * 9
// (+ (* 1 3) (* 4 7) (* 6 9))

1 + 3, 4 + 7
// (, (+ 1 3) (+ 4 7))

(1 + 2 + 3)
// (+ 1 2 3)
1 + (2 + 3)
// (+ 1 (+ 2 3))
(1 + 2) + 3
// (+ (+ 1 2) 3)

()
// null

[a b c]
// (#%group #\[ (a b c))
{a b c}
// (#%group #\{ (a b c))

f(x, y, z)
// (#%app #\( (, x y z))

f[x, y, z]
// (#%app #\[ (, x y z))

f<x, y, z>
// (#%app #\< (, x y z))

x + #;a z
// (+ x z)

x + #;a.b z
// (+ x z)

[x : y]
// (#%group #\[ (x #%colon y))

/* multi
 line
 comment */

if (x < y):
  a
  b
  c
else:
  d
  e
  f
/* (if (x < y)
       (#%indent ((a) (b) (c)))
    else 
       (#%indent ((d) (e) (f)))) */

/*
This appears to require changing the lexer while @ mode is on.

@datalog{
 import "family.thy"
 add(x, @5, y)?}
*/
