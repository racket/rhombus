#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def bind_eval = macro.make_macro_eval())

@title(~tag: "annotation-convert"){Annotations as Converters}

Unless otherwise specified, an @tech{annotation} is a @deftech{predicate
 annotation}. For example, @rhombus(String, ~annot) and @rhombus(ReadableString, ~annot) are
predicate annotations. When a predicate annotation is applied to a value
with the @rhombus(::) expression operator, the result of the expression
is the operator's left-hand argument (or an exception is thrown).
Similarly, using the @rhombus(::, ~bind) binding operator with a
predicate annotation has no effect on the binding other than checking
whether a corresponding value satisfies the annotation's predicate.

A @deftech{converter annotation} produces a result when applied to a
value that is potentially different than the value. For example,
@rhombus(ReadableString.to_string, ~annot) is a converter annotation that converts a
mutable string to an immutable string. When a converter annotation is
applied to a value with the @rhombus(::) expression operator, the result
of the expression can be a different value that is derived from the
operator's left-hand argument. Similarly, using the @rhombus(::, ~bind)
binding operator with a converter annotation can change the incoming
value that is matched against the pattern on the left-hand side of the
operator.

A converting annotation cannot be used with @rhombus(:~), which skips
the predicate associated with a predicate annotation, because conversion
is not optional. Annotation operators and constructors generally accept
both predicate and converter annotations, and the result is typically a
predicate annotation if all given annotations are also predicate
annotations.

The @rhombus(converting, ~annot) annotation constructor creates a new
converter annotation given three pieces:

@itemlist(

 @item{a binding pattern that is matched to an incoming value;}

 @item{a body that can refer to variable bound in the pattern; and}

 @item{an optional annotation for the result of conversion, which can be
 checked and can supply @tech{static information} about the converted
 result.}

)

Since these are the same pieces that a single-argument @rhombus(fun)
form would have, the @rhombus(converting, ~annot) constructor expects a
@rhombus(fun) ``argument,'' but one that is constrained to have a single
argument binding without a keyword.

For example, the following @rhombus(AscendingIntList) annotation matches
any list of integers, but converts it to ensure that the integers are
sorted.

@examples(
  ~eval: bind_eval
  ~defn:
    annot.macro 'AscendingIntList':
      'converting(fun (ints :: List.of(Int)) :: List:
                    ints.sort())'
  ~repl:
    [3, 1, 2] :: AscendingIntList
    fun descending(ints :: AscendingIntList):
      ints.reverse()
    descending([1, 4, 0, 3, 2])
    ~error:
      [3, 1, 2] :~ AscendingIntList
    [[1, 0], [4, 3, 2]] :: List.of(AscendingIntList)
)

When a converting annotation is used in a position that depends only on
whether it matches, such as with @rhombus(is_a), then the converting
body is not used.@margin_note{When used with @rhombus(is_a), the binding pattern is also used in
match-only mode, so its ``committer'' and ``binder'' steps (as described
in @secref("bind-macro-protocol")) are not used.} When a further
annotation wraps a converting annotation, however, the conversion must
be computed to apply a predicate (even the @rhombus(Any, ~annot)
predicate) or further conversion. The nested-annotation strategy is used
in the following example for @rhombus(UTF8BytesAsString), where is
useful because checking whether a byte string is a UTF-8 encoding might
as well decode it. Annotation constructors like @rhombus(List.of, ~annot)
similarly convert eagerly when given a converting annotation for
elements, rather than checking and converting separately.

@examples(
  ~eval: bind_eval
  ~defn:
    annot.macro 'UTF8BytesAsString_oops':
      'converting(fun (s :: Bytes):
                    Bytes.utf8_string(s))'

  ~repl:
    #"\316\273" :: UTF8BytesAsString_oops
    #"\316" is_a UTF8BytesAsString_oops
    ~error:
      #"\316" :: UTF8BytesAsString_oops

  ~defn:
    annot.macro 'MaybeUTF8BytesAsString':
      'converting(fun (s :: Bytes):
                    try:
                      Bytes.utf8_string(s)
                      ~catch _: #false)'

  ~repl:
    #"\316\273" :: MaybeUTF8BytesAsString
    #"\316" :: MaybeUTF8BytesAsString

  ~defn:
    annot.macro 'UTF8BytesAsString':
      // matches only when `MaybeUTF8BytesAsString` produces a string
      'converting(fun (str :: (MaybeUTF8BytesAsString && String)):
                    str)'
  ~repl:
    #"\316\273" :: UTF8BytesAsString
    #"\316" is_a UTF8BytesAsString
    ~error:
      #"\316" :: UTF8BytesAsString
)


@(close_eval(bind_eval))
