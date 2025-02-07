#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Ranges}

A @deftech{range}, or an @deftech{interval}, represents a contiguous
set of integers between two points. When the starting point is
included, the range can be used as a @tech{sequence}; in addition,
when the ending point is not @rhombus(#inf), the range is
@tech{listable}. Generally, the starting point must be less than or
equal to the ending point, so that the lower bound is ``less than or
equal to'' the upper bound by comparison on bounds.

A range supports @tech{membership tests} using the @rhombus(in)
operator, which is the same as @rhombus(Range.contains).

@doc(
  annot.macro 'Range'
  annot.macro 'SequenceRange'
  annot.macro 'ListRange'
){

 The @rhombus(Range, ~annot) annotation matches any range.

 The @rhombus(SequenceRange, ~annot) annotation matches a range that
 can be used as a @tech{sequence}, for which
 @rhombus(Range.includes_start) returns true.

 The @rhombus(ListRange, ~annot) annotation matches a range that is
 @tech{listable}, for which @rhombus(Range.includes_start) returns
 true, and @rhombus(Range.end) returns non-@rhombus(#inf).

 Static information associated by @rhombus(SequenceRange, ~annot) or
 @rhombus(ListRange, ~annot) makes an expression acceptable as a
 sequence to @rhombus(for) in static mode.

}


@doc(
  ~nonterminal:
    start_expr: block expr
    end_expr: block expr
    start_repet: block repet
    end_repet: block repet
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  expr.macro '$start_expr .. $end_expr'
  repet.macro '$start_repet .. $end_repet'
  bind.macro '$start_bind .. $end_bind'
  expr.macro '$start_expr ..'
  repet.macro '$start_repet ..'
  bind.macro '$start_bind ..'
  expr.macro '.. $end_expr'
  repet.macro '.. $end_repet'
  bind.macro '.. $end_bind'
  expr.macro '..'
  repet.macro '..'
  bind.macro '..'
  operator_order:
    ~order: enumeration
){

 The same as @rhombus(Range.from_to, ~expr),
 @rhombus(Range.from, ~expr), @rhombus(Range.to, ~expr), and
 @rhombus(Range.full, ~expr), respectively.

 When @rhombus(start_expr .. end_expr) or @rhombus(start_expr ..) is
 used in an @rhombus(each, ~for_clause) clause of @rhombus(for), the
 optimization is more aggressive in that no intermediate range is
 created.

}

@doc(
  ~nonterminal:
    start_expr: block expr
    end_expr: block expr
    start_repet: block repet
    end_repet: block repet
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  expr.macro '$start_expr ..= $end_expr'
  repet.macro '$start_repet ..= $end_repet'
  bind.macro '$start_bind ..= $end_bind'
  expr.macro '..= $end_expr'
  repet.macro '..= $end_repet'
  bind.macro '..= $end_bind'
  operator_order:
    ~order: enumeration
){

 The same as @rhombus(Range.from_to_inclusive, ~expr) and
 @rhombus(Range.to_inclusive, ~expr), respectively.

 When @rhombus(start_expr ..= end_expr) is used in an
 @rhombus(each, ~for_clause) clause of @rhombus(for), the optimization
 is more aggressive in that no intermediate range is created.

}

@doc(
  ~nonterminal:
    start_expr: block expr
    end_expr: block expr
    start_repet: block repet
    end_repet: block repet
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  expr.macro '$start_expr <.. $end_expr'
  repet.macro '$start_repet <.. $end_repet'
  bind.macro '$start_bind <.. $end_bind'
  expr.macro '$start_expr <..'
  repet.macro '$start_repet <..'
  bind.macro '$start_bind <..'
  operator_order:
    ~order: enumeration
){

 The same as @rhombus(Range.from_exclusive_to, ~expr) and
 @rhombus(Range.from_exclusive, ~expr), respectively.

}

@doc(
  ~nonterminal:
    start_expr: block expr
    end_expr: block expr
    start_repet: block repet
    end_repet: block repet
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  expr.macro '$start_expr <..= $end_expr'
  repet.macro '$start_repet <..= $end_repet'
  bind.macro '$start_bind <..= $end_bind'
  operator_order:
    ~order: enumeration
){

 The same as @rhombus(Range.from_exclusive_to_inclusive, ~expr).

}


@doc(
  ~nonterminal:
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  fun Range.from_to(start :: Int, end :: Int) :: ListRange
  bind.macro 'Range.from_to($start_bind, $end_bind)'
){

 Constructs a range that includes @rhombus(start), but does not
 include @rhombus(end). The corresponding binding matches the
 constructed range.

}

@doc(
  ~nonterminal:
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  fun Range.from_to_inclusive(start :: Int, end :: Int)
    :: ListRange
  bind.macro 'Range.from_to_inclusive($start_bind, $end_bind)'
){

 Constructs a range that includes both @rhombus(start) and
 @rhombus(end). The corresponding binding matches the constructed
 range.

}

@doc(
  ~nonterminal:
    start_bind: def bind ~defn
  fun Range.from(start :: Int) :: SequenceRange
  bind.macro 'Range.from($start_bind)'
){

 Constructs a range that includes @rhombus(start), and with
 @rhombus(#inf) as the ending point. The corresponding binding matches
 the constructed range.

}

@doc(
  ~nonterminal:
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  fun Range.from_exclusive_to(start :: Int, end :: Int)
    :: Range
  bind.macro 'Range.from_exclusive_to($start_bind, $end_bind)'
){

 Constructs a range that does not include either @rhombus(start) or
 @rhombus(end). Unlike the other constructors, @rhombus(start) must be
 less than (but @emph{not} equal to) @rhombus(end), otherwise the
 lower bound would be ``greater than'' the upper bound. The
 corresponding binding matches the constructed range.

}

@doc(
  ~nonterminal:
    start_bind: def bind ~defn
    end_bind: def bind ~defn
  fun Range.from_exclusive_to_inclusive(start :: Int, end :: Int)
    :: Range
  bind.macro 'Range.from_exclusive_to_inclusive($start_bind, $end_bind)'
){

 Constructs a range that does not include @rhombus(start), but
 includes @rhombus(end). The corresponding binding matches the
 constructed range.

}

@doc(
  ~nonterminal:
    start_bind: def bind ~defn
  fun Range.from_exclusive(start :: Int) :: Range
  bind.macro 'Range.from_exclusive($start_bind)'
){

 Constructs a range that does not include @rhombus(start), and with
 @rhombus(#inf) as the ending point. The corresponding binding matches
 the constructed range.

}

@doc(
  ~nonterminal:
    end_bind: def bind ~defn
  fun Range.to(end :: Int) :: Range
  bind.macro 'Range.to($end_bind)'
){

 Constructs a range with @rhombus(#neginf) as the starting point, and
 does not include @rhombus(end). The corresponding binding matches the
 constructed range.

}

@doc(
  ~nonterminal:
    end_bind: def bind ~defn
  fun Range.to_inclusive(end :: Int) :: Range
  bind.macro 'Range.to_inclusive($end_bind)'
){

 Constructs a range with @rhombus(#neginf) as the starting point, and
 includes @rhombus(end). The corresponding binding matches the
 constructed range.

}

@doc(
  fun Range.full() :: Range
  bind.macro 'Range.full()'
){

 Constructs a range with @rhombus(#neginf) as the starting point and
 @rhombus(#inf) as the ending point. The corresponding binding matches
 the constructed range.

}


@doc(
  method (rge :: Range).start() :: Int || matching(#neginf)
  method (rge :: Range).end() :: Int || matching(#inf)
){

 Returns the starting point and ending point of @rhombus(rge),
 respectively. The starting point can be @rhombus(#neginf), and the
 ending point can be @rhombus(#inf), indicating the lack of a starting
 point or ending point.

}

@doc(
  method (rge :: Range).includes_start() :: Boolean
  method (rge :: Range).includes_end() :: Boolean
){

 Returns @rhombus(#true) if @rhombus(rge) includes the starting point
 and the ending point, respectively, @rhombus(#false) otherwise. A
 @rhombus(#neginf) starting point or @rhombus(#inf) ending point
 cannot be included.

}

@doc(
  method (rge :: Range).is_empty() :: Boolean
){

 Returns @rhombus(#true) if @rhombus(rge) is an @deftech{empty range},
 @rhombus(#false) otherwise. An empty range is empty
 ``by definition,'' meaning that its lower bound is ``equal to'' its
 upper bound, and therefore it cannot have anything at all in the
 range that it represents. By contrast, a range may have no integers
 even if its lower bound is strictly ``less than'' its upper bound
 (but it may well have real numbers, in principle); in such case, use
 @rhombus(rge.canonicalize().is_empty()) to check for its
 ``emptiness.''

@examples(
  (3..4).is_empty()
  (3..=3).is_empty()
  (3..3).is_empty()
  (3 <..= 3).is_empty()
  (3 <.. 4).is_empty()
  (3 <.. 4).canonicalize().is_empty()
)

}

@doc(
  method (rge :: Range).canonicalize() :: Range
){

 Returns the canonical form of @rhombus(rge) with respect to the
 discrete domain of integers. The canonical form has and only has all
 integers that @rhombus(rge) has, and is guaranteed to be in one of
 the following forms:

@itemlist(

 @item{@rhombus(#,(@rhombus(start, ~var)) .. #,(@rhombus(end, ~var))),
  if @rhombus(rge.start()) and @rhombus(rge.end()) are both integers;}

 @item{@rhombus(#,(@rhombus(start, ~var)) ..), if @rhombus(rge.start())
  is an integer and @rhombus(rge.end()) is @rhombus(#inf);}

 @item{@rhombus(.. #,(@rhombus(end, ~var))), if @rhombus(rge.start())
  is @rhombus(#neginf) and @rhombus(rge.end()) is an integer; or}

 @item{@rhombus(..), if @rhombus(rge.start()) is @rhombus(#neginf) and
  @rhombus(rge.end()) is @rhombus(#inf).}

)

 Furthermore, if @rhombus(rge) is already in canonical form, it is
 returned as-is.

@examples(
  (1..5).canonicalize()
  (1..).canonicalize()
  (..5).canonicalize()
  (..).canonicalize()
  (1..=5).canonicalize()
  (..=5).canonicalize()
  (1 <.. 5).canonicalize()
  (1 <..).canonicalize()
  (1 <..= 5).canonicalize()
)

}


@doc(
  method (rge :: Range).contains(int :: Int) :: Boolean
){

 Checks whether @rhombus(rge) has @rhombus(int) in the range that it
 represents. See also @rhombus(in).

@examples(
  (3..=7).contains(5)
  (3..=7).contains(7)
  (3..=7).contains(10)
  5 in 3..=7
)

}


@doc(
  method (rge :: Range).encloses(another_rge :: Range, ...) :: Boolean
  fun Range.encloses(rge :: Range, ...) :: Boolean
){

 Checks whether the given ranges are in enclosing order. A range
 @rhombus(rge, ~var) encloses another range @rhombus(rge2, ~var) when
 @rhombus(rge, ~var) has every integer in @rhombus(rge2, ~var). Every
 range encloses itself, and empty ranges never enclose non-empty
 ranges.

@examples(
  Range.encloses()
  (2 <.. 8).encloses()
  (2 <.. 8).encloses(4..=6)
  (2 <.. 8).encloses(2..=6, 4..=6)
  (2 <.. 8).encloses(5..)
  (2 <..).encloses(5..)
)

}

@doc(
  method (rge :: Range).is_connected(rge2 :: Range) :: Boolean
){

 Checks whether @rhombus(rge) is connected with @rhombus(rge2), that
 is, whether there exists a range (possibly empty) that is enclosed by
 both @rhombus(rge) and @rhombus(rge2).

@examples(
  (2..=7).is_connected(3 <.. 8)
  (2..=5).is_connected(5 <.. 8)
  (2 <.. 5).is_connected(5 <.. 8)
)

}

@doc(
  method (rge :: Range).overlaps(rge2 :: Range) :: Boolean
){

 Checks whether @rhombus(rge) overlaps with @rhombus(rge2), that is,
 whether there exists a non-empty range that is enclosed by both
 @rhombus(rge) and @rhombus(rge2).

@examples(
  (2..=7).overlaps(3 <.. 8)
  (2..=5).overlaps(5..=8)
  (2..=5).overlaps(5 <.. 8)
)

}


@doc(
  method (rge :: Range).span(another_rge :: Range, ...) :: Range
){

 Returns the smallest range that encloses @rhombus(rgs) and all
 @rhombus(rge)s.

@examples(
  (2..=5).span()
  (2..=5).span(8 <.. 9)
  (..4).span(6..=6)
  (2 <.. 8).span(..=5, 8 <.. 9)
)

}

@doc(
  method (rge :: Range).gap(rge2 :: Range) :: maybe(Range)
){

 Returns the largest range that lies between @rhombus(rge) and
 @rhombus(rge2), or @rhombus(#false) if no such range exists
 (precisely when @rhombus(rge) overlaps with @rhombus(rge2)).

@examples(
  (2..=5).gap(8..=9)
  (..4).gap(6..=6)
  (2 <.. 8).gap(8..=10)
  (2..=8).gap(8..=10)
)

}

@doc(
  method (rge :: Range).intersect(another_rge :: Range, ...)
    :: maybe(Range)
  fun Range.intersect(rge :: Range, ...) :: maybe(Range)
){

 Returns the intersection of the given ranges, or @rhombus(#false)
 if no such range exists. The intersection of a range
 @rhombus(rge, ~var) and another range @rhombus(rge2, ~var) is the
 largest range that is enclosed by both @rhombus(rge, ~var) and
 @rhombus(rge2, ~var), which only exists when @rhombus(rge, ~var) is
 connected with @rhombus(rge2, ~var).

@examples(
  Range.intersect()
  (2..=8).intersect()
  (2..=8).intersect(4 <.. 16)
  (4 <..).intersect(..6, 2..=8)
  (2 <.. 8).intersect(..=5)
  (2 <.. 8).intersect(8 <.. 10)
)

}


@doc(
  method (rge :: ListRange).to_list() :: List
){

 Implements @rhombus(Listable, ~class) by returning a @tech{list} of
 integers in @rhombus(rge) in order.

}


@doc(
  method (rge :: SequenceRange).to_sequence() :: Sequence
){

 Implements @rhombus(Sequenceable, ~class) by returning a
 @tech{sequence} of integers in @rhombus(rge) in order. The sequence
 is infinite when the ending point is @rhombus(#inf).

}

@doc(
  method (rge :: SequenceRange).step_by(step :: PosInt) :: Sequence
){

 Returns a @tech{sequence} of integers in @rhombus(rge) in order,
 stepping by the given @rhombus(step) size.

 When invoked as @rhombus(rge.step_by(step)) in an
 @rhombus(each, ~for_clause) clause of @rhombus(for), the sequence is
 optimized, in addition to the optimization in @rhombus(..) or
 @rhombus(..=).

}
