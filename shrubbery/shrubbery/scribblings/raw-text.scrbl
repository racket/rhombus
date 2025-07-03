#lang scribble/manual
@(require (for-label racket/base
                     racket/contract/base
                     shrubbery/parse
                     shrubbery/property))

@title[#:tag "raw-text"]{Source Locations and Raw-Text Properties}

@defmodule[shrubbery/property]

The result of @racket[parse-all] records source locations in the syntax
object representing a shrubbery form. Source-location information and
other properties for a shrubbery @litchar{()}, @litchar{[]},
@litchar["{}"], or @litchar{''} is associated with the @racket[parens],
@racket[brackets], @racket[braces], or @racket[quotes] identifier in the
representation. Similarly, source-location information and properties
for a @litchar{:} block or @litchar{|} alternatives are recorded on a
@racket[block] or @racket[alts] identifier. For all compound forms,
including @racket[group] and @racket[multi], the source location on the
representation's head identifier spans the compound's form. For an
operator, source-location information and properties are associated to
the operator identifier, and not the wrapper @racket[op] identifier.
Each structuring identifier like @racket[group], @racket[block],
@racket[parens], or @racket[op] has the @racket['identifier-as-keyword]
syntax property as @racket[#t].

The result of @racket[parse-all] has source locations copied to the
S-expression ``parentheses'' of a syntax object representing a compound
form, but only as a convenience; the intent is that information is more
permanently and reliably associated to each compound form's head
identifier. Along similar lines, spanning source locations for compound
forms tend not to be maintained as syntax objects are manipulated, and
the intent is that spanning locations are reconstructed as needed,
especially for @racket[group] and @racket[multi] forms. A useful
convention may be to treat source locations the S-expression
``parentheses'' of a compound form as a cache for spanning information
computed from the form's content.

A syntax object produced by @racket[parse-all] includes @deftech{raw text} properties that
allow the original shrubbery text to be reconstructed from the syntax
object. This raw text information is distributed among
syntax objects in a way that is preserved to a useful degree
on terms as they are rearranged by enforestation and macro expansion.
The @racket[shrubbery-syntax->string] function reconstructs source text
based on raw text properties in a syntax object.
The @racketmodname[shrubbery/property] module exports functions to access and
update properties, which can be helpful to avoid typos that are all too
easy when writing the key directly as a quoted symbol.

Raw-text property values are trees of strings: a string, an empty list,
or a pair containing two trees. The source raw text is reconstructed
through a preorder traversal of the tree. The @racket[parse-all]
function attaches raw text properties only to atom terms and the head
identifiers of compound terms, not counting @racket[multi]. A head
@racket[op], @racket[multi], or @racket[parsed] is normally not
consulted for syntax properties, but @racket[parse-all] associates empty
raw text to a head @racket[op] or @racket[multi]. For a @racket[parsed]
representation, the third component of the @racket[parsed] list is
consulted for @racket['opaque-raw] by functions like
@racket[shrubbery-syntax->string].

The main raw text property key is @racket['raw], but the following list
of all raw text keys is in the order that they contribute to
reconstructed text:

@itemlist[

 @item{@indexed-racket['raw-prefix] (use
  @racket[syntax-raw-prefix-property]): records original text for
  whitespace and comments before a term or group. The @racket[parse-all]
  function uses this property only when raw text cannot instead be
  associated with a preceding term or group as a suffix. When raw text
  must be associated as a prefix but could be attached to either a term or
  its enclosing group (because the term is first within its group),
  @racket[parse-all] attaches the prefix to the group.}

 @item{@indexed-racket['raw-inner-prefix] (use
  @racket[syntax-raw-inner-prefix-property]): like @racket['raw-prefix],
  and after @racket['raw-prefix] and still before @racket['raw], but
  with the intent that the raw text sticks to its term, instead of being
  shifted to a preceding term or enclosing group. The @racket[parse-all] function
  uses this property only to record a @litchar["@"] that appears before a term for
  @seclink["at-notation"].}

 @item{@indexed-racket['raw] (use @racket[syntax-raw-property]): records
  the original text of an atomic term or the opening text for a compound term.
  For example, the input @litchar{0x11} will be parsed as the number
  @racket[17] with a @racket['raw] property value @racket["0x11"]. The
  @racket['raw] property for a @racket[parens] representation is normally
  @racket["("], while @racket['raw] for a @racket[group] representation is
  normally empty. The @racket[parse-all] function also associated an empty
  @racket['raw] on the @racket[op] identifier of an operator
  representation, although that identifier is not normally consulted for
  properties (e.g., by @racket[shrubbery-syntax->string]).}

 @item{@indexed-racket['raw-opaque-content] (use
  @racket[syntax-raw-opaque-content-property]): records raw text to use in
  place of the content of compound form. When the compound form also has
  the @racket['raw] property, @racket['raw-opaque-content] is shown after
  @racket['raw]. A non-compound form can have
  @racket['raw-opaque-content], and it is combined with @racket['raw] in
  that case, too. Note that the @racket['opaque-raw] is a different
  property.}

 @item{@indexed-racket['opaque-raw] (use
  @racket[syntax-opaque-raw-property]): raw text that supersedes
  @racket['raw] and @racket['raw-opaque-content]. Furthermore, this
  property is recognized when present on the S-expression ``parentheses''
  of a compound form or an @racket[op] form, in which case properties on
  the leading identifier and/or compound form's content are ignored.
  Although this property is intended for use on S-expression
  ``parentheses'' in a shrubbery representation, it is also recognized on
  atom terms. When @racket['opaque-raw] is recognized on S-expression
  ``parentheses'', then @racket['raw-prefix], @racket['raw-inner-prefix],
  @racket['raw-inner-suffix], and @racket['raw-suffix] are also recognized
  (and those would otherwise be ignored).}

 @item{@indexed-racket['raw-tail] (use
  @racket[syntax-raw-tail-property]): records original text to appear
  after a term's content. This property is intended for use with compound
  terms to hold the compound form's closer.
  For example, @racket['raw-tail] property for a @racket[parens]
  representation is normally @racket[")"].}

 @item{@indexed-racket['raw-inner-suffix] (use
  @racket[syntax-raw-inner-suffix-property]): analogous to
  @racket['raw-inner-prefix], but for a suffix that should stick with its
  term. The @racket[parse-all] function currently does not use this property.}

 @item{@indexed-racket['raw-suffix] (use
  @racket[syntax-raw-suffix-property]): records original text for
  whitespace and comments after a term or group. When a term is last in
  its group, the @racket[parse-all] function uses @racket['raw-suffix] on the
  group instead of the term.}

]

Each of these properties is normally preserved (in the sense of a true
fourth argument to @racket[syntax-property]), except for
@racket['opaque-raw], which is intended for use in intermediate,
short-term mixtures of shrubbery forms and S-expressions.

@deftogether[(
@defproc*[([(syntax-raw-property [stx syntax?]) any/c]
           [(syntax-raw-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw] preserved @tech{raw text}
 property.

 For example, @racket[parse-all] will parse the input @litchar{0x11} as
 the number @racket[17] with a @racket['raw] property value
 @racket["0x11"]. The input @litchar{"\u3BB"} will be parsed as the
 string @racket["λ"] with a @racket['raw] property value
 @racket["\"\\u3BB\""].

}

@deftogether[(
@defproc*[([(syntax-raw-prefix-property [stx syntax?]) any/c]
           [(syntax-raw-prefix-property [stx syntax?] [val any/c]) syntax?])]
@defproc*[([(syntax-raw-suffix-property [stx syntax?]) any/c]
           [(syntax-raw-suffix-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-prefix] or @racket['raw-suffix]
 preserved @tech{raw text} property.

 For example, @racket[parse-all] will parse the input @litchar{ 1 +  2 // done} into the
 S-expression representation @racket[(multi (group 1 (op +) 2))]. The
 syntax object for @racket[group] will have a @racket['raw-prefix] value
 equivalent to @racket[" "] and a @racket['raw-suffix] value equivalent to
 @racket[" // done"], but possibly within a tree structure instead of a
 single string. The syntax object for @racket[1] and will have a
 @racket['raw-suffix] value equivalent to @racket[" "], while the syntax
 object for @racket[+] and will have a @racket['raw-suffix] value
 equivalent to @racket["  "] (i.e., two spaces).

}

@deftogether[(
@defproc*[([(syntax-raw-inner-prefix-property [stx syntax?]) any/c]
           [(syntax-raw-inner-prefix-property [stx syntax?] [val any/c]) syntax?])]
@defproc*[([(syntax-raw-inner-suffix-property [stx syntax?]) any/c]
           [(syntax-raw-inner-suffix-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-inner-prefix] or
 @racket['raw-inner-suffix] preserved @tech{raw text} property.

 The @racket[parse-all] function will parse the input @litchar["@x"] into
 an S-expression representation @racket[x] with a
 @racket['raw-inner-prefix] property @racket["@"]. The @racket[parse-all]
 function never produces a syntax object with @racket['raw-inner-suffix].

}


@deftogether[(
@defproc*[([(syntax-raw-tail-property [stx syntax?]) any/c]
           [(syntax-raw-tail-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-tail] preserved @tech{raw text}
 property.

 For example, the input @litchar{(1 + 2) * 4 //done} will be parsed into
 the S-expression representation
 @racket[(multi (group (parens (group 1 (op +) 2)) (op *) 4))]. The syntax
 object for the outer @racket[group] will have a @racket['raw-suffix]
 value equivalent to @racket[" // done"]. The syntax object for
 @racket[parens] will have a @racket['raw] value equivalent to
 @racket["("], a @racket['raw-tail] value equivalent to @racket[")"], and
 a @racket['raw-suffix] value equivalent to @racket[" "]. The inner
 @racket[group] syntax object will have no properties or ones with values
 that are equivalent to empty strings.

}

@deftogether[(
@defproc*[([(syntax-raw-opaque-content-property [stx syntax?]) any/c]
           [(syntax-raw-opaque-content-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-opaque-content] preserved
 @tech{raw text} property.

}


@deftogether[(
@defproc*[([(syntax-opaque-raw-property [stx syntax?]) any/c]
           [(syntax-opaque-raw-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['opaque-raw] non-preserved @tech{raw text}
 property. Unlike @racket['raw] and similar properties, this
 one is associated with the S-expression list for a @racket[group],
 @racket[parens], etc., form, and not with the leading tag identifier.

 The @racket['opaque-raw] property is useful in macro expansion to
 record a macro's input to its output---not only in source location, but
 also in source text.

}
