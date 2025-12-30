#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def range_eval = make_rhombus_eval())

@title(~tag: "range"){Ranges}

As we saw in @secref("for"), some ranges can be useful when used as
sequences. More generally, ranges represent a contiguous set of
integers between two points, and they support constant-time operations
that work with integers. Ranges are typically created with the
@rhombus(..), @rhombus(..=), @rhombus(<..), or @rhombus(<..=)
operators, where a prefix @litchar{<} alludes to an exclusive (as
opposed to inclusive) starting point, while a suffix @litchar{=} alludes
to an inclusive (as opposed to exclusive) ending point. There are also
@rhombus(Range, ~annot) functions that correspond to these operators.

A range supports @tech(~doc: ref_doc){membership tests} with the
@rhombus(in) operator.

@examples(
  ~eval: range_eval
  ~defn:
    def natural_numbers = 0..
  ~repl:
    0 in natural_numbers
    5 in natural_numbers
    -5 in natural_numbers
)

A range is said to enclose another range when it contains every
integer that the other range contains.

@examples(
  ~eval: range_eval
  ~defn:
    def integers = ..
  ~repl:
    integers.encloses(natural_numbers)
)

Ranges in the forms
@rhombus(#,(@rhombus(x, ~var)) .. #,(@rhombus(y, ~var))),
@rhombus(#,(@rhombus(x, ~var)) ..= #,(@rhombus(y, ~var))),
@rhombus(#,(@rhombus(x, ~var)) ..),
@rhombus(#,(@rhombus(x, ~var)) <.. #,(@rhombus(y, ~var))),
@rhombus(#,(@rhombus(x, ~var)) <..= #,(@rhombus(y, ~var))), or
@rhombus(#,(@rhombus(x, ~var)) <..) satisfy
@rhombus(SequenceRange, ~annot) and can be used as sequences, as
already shown in @secref("for"). Moreover, ranges in the forms
@rhombus(#,(@rhombus(x, ~var)) .. #,(@rhombus(y, ~var))),
@rhombus(#,(@rhombus(x, ~var)) ..= #,(@rhombus(y, ~var))),
@rhombus(#,(@rhombus(x, ~var)) <.. #,(@rhombus(y, ~var))), or
@rhombus(#,(@rhombus(x, ~var)) <..= #,(@rhombus(y, ~var))) also satisfy
@rhombus(ListRange, ~annot) and are listable (see @secref("list")).

@examples(
  ~eval: range_eval
  ~defn:
    def less_than_five = 0..5
    def up_to_five = 0..=5
  ~repl:
    [& less_than_five]
    [& up_to_five]
  ~defn:
    def less_than_five_no_zero = 0 <.. 5
    def up_to_five_no_zero = 0 <..= 5
  ~repl:
    [& less_than_five_no_zero]
    [& up_to_five_no_zero]
)


@(close_eval(range_eval))
