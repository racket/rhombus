#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Errors}

@doc(
  fun error(
    ~exn: exn :: (Function.of_arity(2)
                    || Function.of_arity(3)
                    || Function.of_arity(4))
            = Exn.Fail,
    ~srcloc: srcloc :: maybe(Srcloc) = #false,
    ~who: who :: maybe(error.Who) = #false,
    ~realm: realm :: Symbol = #'rhombus,
    msg :: ReadableString,
    ~details: details :: [List.of(ReadableString)] = [],
    clause :: error.Clause,
    ...
  ) :: None
){

 Throws the result of @rhombus(exn) as an exception, constructing the
 error message from @rhombus(srcloc), @rhombus(who), @rhombus(msg),
 @rhombus(details), and the @rhombus(clause)s in order.

 If @rhombus(who) or @rhombus(srcloc) is not @rhombus(#false), it is
 added to the beginning of the message, and a @litchar{: } separator
 is added in between. If both are not @rhombus(#false),
 @rhombus(srcloc) appears preceding @rhombus(who). The string form of
 @rhombus(who) is used, while a human-readable string form (in the
 sense of @rhombus(Srcloc.to_report_string)) of @rhombus(srcloc) is
 used.

 The @rhombus(msg) part of the error message is meant to fit on a single
 line. If @rhombus(details) is non-empty, then @litchar{;} is added to
 the end of @rhombus(msg), and each element of @rhombus(details) is
 written on its own line with a 1-space prefix.

 The @rhombus(clause)s are added to the end of the error message.
 Construct a clause with functions like @rhombus(error.val),
 @rhombus(error.vals), @rhombus(error.annot), or @rhombus(error.text).
 Each @rhombus(clause) will start on its own line with a 2-space prefix.

 When @rhombus(exn) is called by @rhombus(error), the first argument is
 a string message, and the second argument is
 @rhombus(Continuation.Marks.current()). If @rhombus(exn) accepts at
 least three arguments, then @rhombus(PairList[srcloc]) is provided as the third
 argument if @rhombus(srcloc) is a @rhombus(Srcloc) or @rhombus(PairList[]) otherwise.
 If @rhombus(exn) accepts four arguments, then the list of
 @rhombus(clause)s is provided as the fourth argument.

 Conventions for using @rhombus(error):

 @itemlist(

  @item{Start @rhombus(msg) in lowercase, and keep it short. Use
  @rhombus(details) to elaborate on the initial message, but report any
  values or other specifics in @rhombus(clause)s. When @rhombus(details)
  has multiple sentences, separate then with @litchar{;} and stay in
  lowercase mode.}

  @item{Normally, @rhombus(Exn.Fail.Annot, ~annot) should be used for
  @rhombus(exn) (in place of @rhombus(Exn.Fail, ~annot)) if the exception
  represents an error that could have been prevented (without a race
  condition) by a preceding check.}

  @item{To throw an error that complains about a single
  argument--annotation mismatch (in the same way as @rhombus(::)), use
  @rhombus(~exn: Exn.Fail.Annot), use @rhombus(error.annot_msg) to
  construct @rhombus(message), and use @rhombus(error.annot) to and
  @rhombus(error.val) to constructs @rhombus(clause)s. Provide the same
  label string to @rhombus(error.annot_msg) and @rhombus(error.val) if it
  is useful to be more specific than @rhombus("value").}

 )

 The error message is adjusted using the given @rhombus(realm).
 Typically, Rhombus error messages are identified as being from the
 @rhombus(#'rhombus) realm.

@examples(
  ~repl:
    ~error:
      error("oops")
    ~error:
      error(~who: #'me, "oops")
    ~error:
      error(~who: #'me,
            "oops",
            ~details: ["something has gone wrong;",
                       "see the manual for more information"])
    ~error:
      error(~who: #'me,
            ~exn: Exn.Fail.Annot,
            error.annot_msg("fruit"),
            error.annot("Tropical"),
            error.val(~label: "fruit", #'Apple))
    ~error:
      error(~who: #'me,
            "mismatch between fruit and vegetable",
            error.val(~label: "fruit", [#'Apple, 0]),
            error.val(~label: "vegetable", [#'Lettuce, -1]))
)

}


@doc(
  fun error.message(
    ~srcloc: srcloc :: maybe(Srcloc) = #false,
    ~who: who :: maybe(error.Who) = #false,
    ~realm: realm = #'rhombus,
    msg :: ReadableString,
    ~details: details :: [List.of(ReadableString)] = [],
    clause :: Error.Clause,
    ...
  ) :: String
){

 Like @rhombus(error), but without the @rhombus(~exn) argument, and the
 result is a message string instead of throwing an exception.

@examples(
  ~repl:
    error.message(~who: #'me, "oops")
)

}


@doc(
  annot.macro 'error.Who'
){

 Satisfied by a @rhombus(ReadableString, ~annot),
 @rhombus(Symbol, ~annot), or @rhombus(Name, ~annot).

}


@doc(
  class error.Clause(msg :: String):
    nonfinal
  fun error.text(~label: label :: String,
                 v :: Any)
    :: error.Clause
  fun error.val(~label: label :: String = "value",
                v :: Any)
    :: error.Clause
  fun error.vals(~label: label :: String = "value",
                 v :: Any, ...)
    :: error.Clause
  fun error.annot(~label: label :: String = "annotation",
                  ~realm: realm :: Symbol = #'rhombus,
                  annot_str :: String)
    :: error.Clause
){

 An @rhombus(error.Clause, ~annot) represents a piece of an error
 message that has a label followed by a value or text. The
 @rhombus(error.text) constructor is the most generic one, where the
 argument @rhombus(v) is converted with @rhombus(to_string) to include in
 the message. If the string form of @rhombus(v) spans multiple lines,
 each line will get a 3-space prefix to incorporate it into the message
 string; see also @rhombus(error.reindent).

 The @rhombus(error.val) and @rhombus(error.vals) function convert each
 @rhombus(v) to a string using @rhombus(repr).

 Use @rhombus(error.annot) to report an annotation in an error message,
 where @rhombus(Syntax.to_source_string) may be useful (especially in in
 a macro's implementation) to construct a suitable string form of an
 annotation. The annotation string is adjusted using the given
 @rhombus(realm).

 The @rhombus(msg) field of a @rhombus(error.Clause, ~annot) omits a
 2-space prefix that will be added to the clause by @rhombus(error) or
 @rhombus(error.message), but if it spans multiple lines, then
 @rhombus(msg) will include a 3-space prefix on each line after the first
 one.

@examples(
  error.text(~label: "name", "Alice")
  error.val([1, 2, 3])
  error.val(~label: "list", [1, 2, 3])
  error.vals(1, 2, 3)
  error.annot("List.of(Int)")
)

}


@doc(
  fun error.annot_msg(what :: String = "value")
){

 Constructs the text of a ``does not satisfy annotation'' error, using
 @rhombus(what) as the noun in the message.

}


@doc(
  fun error.reindent(
    s :~ String,
    ~space: space :: String = " ",
    ~tab: tab :: String = "   ",
    ~label: label :: String = "",
    ~max_len: max_len :: Nat
                = 72 - (2 + label.length() + 1 + space.length())
  ) :: String
){

 The indentation function used by @rhombus(error.text) and related
 functions. If @rhombus(s) has multiple lines or if it is longer than
 @rhombus(max_len) characters, then @rhombus(s) is prefixed with a
 newline, and each line of @rhombus(s) is prefixed with @rhombus(tab).
 Otherwise, @rhombus(s) is prefixed with just @rhombus(space).

 The @rhombus(label) argument is used only to determine the default
 value of @rhombus(max_len).

}
