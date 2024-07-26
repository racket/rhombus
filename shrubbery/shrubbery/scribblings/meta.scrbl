#lang rhombus/scribble/manual

@title{Design Considerations}

Shrubbery notation serves the same role as S-expression notation as a
vehicle for a programming language, but with different trade-offs.

S-expression notation imposes a grouping at the token level that is all
but guaranteed to be respected by further parsing via macro expansion.
One consequence of this token-based grouping is that programs can be
pretty-printed and textually traversed in standard ways.

A traditional use of S-expression notation, however, insists that
@emph{all} grouping is reflected in the S-expression. Reifying all
grouping at the token level is so onerous that many practical
deployments of S-expressions include deviations from the rule, such as
keyword-based arguments or implicit grouping by position (as in various
Clojure forms).

Another disadvantage of S-expressions is that many of the parentheses
are redundant after the expression is pretty-printed, because
indentation provides the same grouping information in a more
human-readable way. That observation suggests instead relying on line
breaks and indentation to impart grouping information, as in Python.

Shrubbery notation explores a point in the design space where the
notation is

@itemlist(

 @item{line- and indentation-sensitive, and},
 
 @item{intended to constrain grouping but not reflect every detail of
  grouping.}

)

Deferring complete grouping to another parser relieves a burden on
reader-level notation. At the same time, line- and indentation-sensitive
rules constrain parsing to ensure that line breaks and indentation in
the source are not misleading.

@include_section("rationale.scrbl")
@include_section("prior-art.scrbl")