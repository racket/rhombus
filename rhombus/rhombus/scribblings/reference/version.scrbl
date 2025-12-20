#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    scribble/bnf
    meta_label:
      rhombus/version open)

@title{Version Number Comparison}

@docmodule(rhombus/version)

@doc(
  veneer Version
){

 A @tech{veneer} that recognizes valid version strings and adjusts
 comparison operators like @rhombus(<) to compare as versions instead of
 plain strings.

 A valid version following Racket conventions has one of the following
 forms:

 @itemlist(
  @item{@bnf.nt{maj}@litchar{.}@bnf.nt{min}}
  @item{@bnf.nt{maj}@litchar{.}@bnf.nt{min}@litchar{.}@bnf.nt{sub}}
  @item{@bnf.nt{maj}@litchar{.}@bnf.nt{min}@litchar{.}@bnf.nt{sub}@litchar{.}@bnf.nt{rel}}
)

subject to the following constraints:

@itemlist(

 @item{@bnf.nt{maj}, @bnf.nt{min}, @bnf.nt{sub}, and @bnf.nt{rel} are
  all canonical decimal representations of @rhombus(Nat, ~annot)s (i.e.,
  decimal digits with no leading @litchar{0} unless exactly @litchar{0});}

 @item{@bnf.nt{rel} is not @litchar{0};}

 @item{@bnf.nt{sub} is not @litchar{0} unless @bnf.nt{rel} is included;}

 @item{@bnf.nt{min} has no more than two digits;}

 @item{@bnf.nt{sub} and @bnf.nt{rel} have no more than three digits.}

)

 The constraints force version numbers to be in a canonical form. For
 example, a would-be version string @rhombus("4.3.0") must be written
 instead as @rhombus("4.3"), @rhombus("4.3.1.0") must be written instead
 as @rhombus("4.3.1"), and rhombus("4") must be written as
 @rhombus("4.0").

}

@doc(
  method (vers :: Version).to_list() :: List.of(Nat)
){

 Parses a version into four component integers.

}


@doc(
  method (vers :: Version).to_int() :: Nat
){

 Converts a version into a single integer where ordering on the integers
 reflects ordering on the converted versions.

}

@doc(
  method (vers :: Version).is_alpha() :: Boolean
){

 Reports whether a version represents an ``alpha'' version.

 A version number of the form @bnf.nt{maj}@litchar{.}@bnf.nt{min},
 @bnf.nt{maj}@litchar{.}@bnf.nt{min}@litchar{.}@bnf.nt{sub}, or
 @bnf.nt{maj}@litchar{.}@bnf.nt{min}@litchar{.}@bnf.nt{sub}@litchar{.}@bnf.nt{rel},
 is an ``alpha'' version if @bnf.nt{min} is @litchar{90} or more,
 @bnf.nt{sub} is @litchar{900} or more, or @bnf.nt{rel} is @litchar{900}
 or more.

}
