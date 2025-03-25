#lang rhombus/scribble/manual
@(import:
    "racket_names.rkt" open
    meta_label:
      rhombus open)

@(def rhombus_doc = ModulePath'lib("rhombus/scribblings/reference/rhombus-reference.scrbl")')
@(def racket_doc = ModulePath'lib("scribblings/reference/reference.scrbl")')

@title(~tag: "data"){Common Run-Time Representations}

Rhombus and Racket share the same or related representations for many
basic forms of data:

@itemlist(

 @item{Booleans are the same.}

 @item{Numbers are the same, including @tech(~doc: rhombus_doc){fixnums}
 and @tech(~doc: rhombus_doc){flonums}.}

 @item{Characters, strings, and byte strings are the same. Beware,
 however, that Racket strings are mutable by default, which satisfies
 Rhombus's @rhombus(ReadableString, ~annot) but not
 @rhombus(String, ~annot).}

 @item{Symbols and keywords. A keyword is written in shrubbery notation
 for Rhombus with a @litchar{~} prefix, and it is written in S-expression
 notation with a @litchar{#:} prefix, but the representation does not
 include that prefix.

 A Racket identifier with non-alphabetic characters can be written in
 Rhombus using @litchar("#{")…@litchar("}") notation, as in
 @rhombus(#{finish-work}). A Racket keyword with non-alphabetic
 characters can be written in Rhombus using @litchar("~#{")…@litchar("}")
 notation, as in @rhombus(~#{fast?}), or using
 @litchar("#{")…@litchar("}") notation, as in @rhombus(#{#:fast?}), and
 the former is usually preferred.}

 @item{Functions, including functions with keyword arguments are the
 same.

 The @litchar("#{")…@litchar("}") and @litchar("~#{")…@litchar("}")
 notations can be useful for calling Racket functions with keyword
 arguments, as in @rhombus(#{finish-work}(~#{fast?}: #true)).}

 @item{Pairs are the same. Racket @tech(~doc: racket_doc){lists} are
 Rhombus @tech(~doc: rhombus_doc){pair lists}, Rhombus
 @tech(~doc: rhombus_doc){lists} correspond to
 @racketmod_racket_treelist, and Rhombus @tech(~doc: rhombus_doc){mutable
  lists} correspond to @racketmod_racket_mutable_treelist.}

 @item{Racket @tech(~doc: racket_doc){vectors} and Rhombus
 @tech(~doc: rhombus_doc){arrays} are the same, and boxes are the same in
 both.}

 @item{Racket @tech(~doc: racket_doc){hash tables} and Rhombus
 @tech(~doc: rhombus_doc){maps} are the same. Rhombus
 @tech(~doc: rhombus_doc){sets} are unrelated to any Racket data
 structure, although the implementation internally uses Racket hash
 tables.}

 @item{Racket @tech(~doc: racket_doc){sequences} and Rhombus
 @tech(~doc: rhombus_doc){sequences} are the same.}

 @item{The void value and end-of-file object are the same.}

 @item{Rhombus and Racket syntax objects are the same. A Rhombus syntax
 object encodes shrubbery structure within an S-expression syntax object
 as described in
 @secref(~doc: ModulePath'lib("shrubbery/scribblings/shrubbery.scrbl")', "parsed-rep").}

)

In many other cases, the Rhombus representation of an entity is a
wrapper on a Racket representation. For example, a Rhombus
@rhombus(Thread, ~class) object wraps a Racket thread object. In those
case, a common convention within Rhombus is to provide access to the
Racket representation through a @rhombus(handle) property, and sometimes
a @rhombus(from_handle) function is provided to construct a Rhombus
object wrapper for a suitable Racket value.

Rhombus wrappers enable a dynamic @rhombus(.) operation to find methods
and properties. Although dynamic @rhombus(.) works on some kinds of
Racket values without a wrapper, such as strings and lists, it relies on
built-in support by the @rhombus(.) operator. Rhombus has no safe and
composable way to extend that built-in set.
