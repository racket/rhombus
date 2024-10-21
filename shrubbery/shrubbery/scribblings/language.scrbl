#lang scribble/manual
@(require "rhm_id.rhm"
          (for-label racket/base
                     racket/contract/base
                     syntax/parse
                     shrubbery/parse
                     shrubbery/write
                     shrubbery/print
                     shrubbery/property))

@title[#:tag "language"]{Language and Parser API}

@defmodulelang[shrubbery]

The @racketmodname[shrubbery] meta-language is similar to the
@racketmodname[s-exp] meta-language. It expects a module name after
@racket[@#,hash-lang[] @#,racketmodname[shrubbery]] to serve as the
language of a Racket @racket[module] form, while the body of the module
after the @hash-lang[] line is parsed as shrubbery notation.

Unlike @racketmodname[s-exp], @racketmodname[shrubbery] also works
without another language listed on the @hash-lang[] line. In that case,
running the module prints the S-expression form of the parsed shrubbery
(see @secref["parsed-rep"]). For example,

@codeblock{
 #lang shrubbery
 1+2
}

prints @racketresult['(multi (group 1 (op +) 2))]. But if @filepath{demo.rkt} contains

@racketmod[#:file "demo.rkt"
 racket/base
 (require (for-syntax racket/base
                      syntax/parse))

 (provide (rename-out [module-begin #%module-begin])
          + - * /)

 (define-syntax (module-begin stx)
   (syntax-parse stx
     #:datum-literals (multi group op)
     [(_ (multi (group n1:number (op o) n2:number)))
      #'(#%module-begin (o 'n1 'n2))]))
 ]

then

@codeblock{
 #lang shrubbery "demo.rkt"
 1+2
}

prints the result @racketresult[3].

A same-line module language for @racketmodname[shrubbery] is determined
by using @racket[parse-all] in @racket['line] mode. As long as the
resulting shrubbery is not empty, it is parsed in the same way that
@racketmodname[rhombus] parses module names for @|rhm-import|.

@section{Parsing API}

@defmodule[shrubbery/parse]

@defproc[(parse-all [in input-port?]
                    [#:source source any/c (object-name in)]
                    [#:mode mode (or/c 'top 'interactive 'line 'text) 'top]
                    [#:start-column start-column exact-nonnegative-integer? 0])
         (or/c eof-object? syntax?)]{

 Parses shrubbery notation from @racket[in] and returns an S-expression
 representation as described in @secref["parsed-rep"] as a syntax object.

 The result syntax object has no scopes, but it has source-location
 information and @tech{raw text} properties. See @secref["raw-text"] for
 more information.

 The default @racket['top] mode reads @racket[in] until an end-of-file,
 and it expects a sequence of groups that are indented consistently
 throughout (i.e., all starting at the same column). The result in
 @racket['top] mode is always a @racket[multi] representation. The
 @racket['text] mode is similar, but it starts in ``text'' mode, as if
 the entire input is inside curly braces of an @litchar["@"] form (see
 @secref["at-notation"]). The result of @racket['text] mode is always a
 @racket[brackets] representation.

 The @racket['interactive] and @racket['line] modes are similar to @racket['top]. They
 are suitable for a read-eval-print loop or reading the continuation of a
 @racket[@#,hash-lang[] @#,racketmodname[shrubbery]] line, respectively.
 In both modes, reading stops when a newline is encountered, unless an
 opener remains to be closed or a @litchar{:} was encountered. If reading
 continues due to a @litchar{:}, then it stops when a blank line is found
 (where a line containing a comment does not count as blank). In
 @racket['line] mode, the result may be empty, while
 @racket['interactive] mode continues past a newline if the result would
 be empty.

 The shrubbery parser directly determines line and column changes for
 the purposes of determining indentation, so it does not require
 @racket[in] to have line counting enabled for that purpose. The
 @racket[start-column] argument supplies a number of characters that
 should be considered before the first character of @racket[in] for
 parsing. Source locations attached to the result syntax objects
 @emph{are} based on positions reported by @racket[in] or line and column
 counting as enabled for @racket[in].

}

@section[#:tag "raw-text"]{Source Locations and Raw-Text Properties}

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

@section{Writing Shrubbery Notation}

@defmodule[shrubbery/write]

@defproc[(write-shrubbery [v any/c]
                          [port output-port? (current-output-port)]
                          [#:pretty? pretty? any/c #f]
                          [#:multi-line? multi-line? any/c #f]
                          [#:armor? armor? any/c #f])
         void?]{

 Prints @racket[v], which must be a valid S-expression representation of
 a shrubbery (see @secref["parsed-rep"]). Reading the
 printed form back in with @racket[parse-all] produces the same
 S-expression representation as @racket[v]. @tech{Raw text} properties
 are not used.

 The default mode with @racket[pretty?] as @racket[#false] prints in a
 simple and relatively fast way (compared to @racket[pretty-shrubbery]).
 Even with @racket[pretty?] as a true value, the output is a single line
 unless @racket[multi-line?] is a true value, while @racket[multi-line?]
 produces newlines eagerly. Use @racket[pretty-shrubbery] to gain more
 control over line choices when printing.

 If @racket[pretty?] is @racket[#false] or @racket[armor?] is a true
 value, then the printed form is @seclink["guillemet"]{line- and
  column-insensitive}.

 Note that @racket[write-shrubbery] expects an S-expression, not a
 syntax object, so it cannot use @seclink["raw-text"]{raw text properties}.
 See also @racket[shrubbery-syntax->string].

}

@defproc[(pretty-shrubbery [v any/c]
                           [#:armor? armor? any/c #f])
         any/c]{

 Produces a description of how to print @racket[v] with newlines and
 indentation. The printed form is @seclink["guillemet"]{line- and
  column-insensitive} if @racket[armor?] is a true value.

 The description is an S-expression DAG (directed acyclic graph) that
 represents pretty-printing instructions and alternatives:

 @itemlist[

 @item{@racket[_string] or @racket[_bytes]: print literally.}

 @item{@racket['nl]: print a newline followed by spaces corresponding to
   the current indentation.}

 @item{@racket[`(seq ,_doc ...)]: print each @racket[_doc] in sequence,
   each with the same indentation.}

 @item{@racket[`(nest ,_n ,_doc)]: print @racket[_doc] with the current
   indentation increased by @racket[_n].}

 @item{@racket[`(align ,_doc)]: print @racket[_doc] with the current
   indentation set to the current output column.}

 @item{@racket[`(or ,_doc ,_doc)]: print either @racket[_doc]; always
   taking the first @racket[_doc] in an @racket['or] will produce
   single-line output, while always taking the second @racket[_doc] will
   print a maximal number of lines.}

]

 The description can be a DAG because @racket['or] alternatives might
 have components in common. In the worst case, a tree view of the
 instructions can be exponentially larger than the DAG representation.

}

@section{Reconstructing Shrubbery Notation}

@defmodule[shrubbery/print]

@defproc[(shrubbery-syntax->string [s syntax?]
                                   [#:use-raw? use-raw? any/c #f]
                                   [#:max-length max-length (or/c #f exact-positive-integer?) #f]
                                   [#:keep-prefix? keep-prefix? any/c #f]
                                   [#:keep-suffix? keep-suffix? any/c #f]
                                   [#:inner? inner? any/c #f]
                                   [#:infer-starting-indentation? infer-starting-indentation? any/c (not keep-prefix?)]
                                   [#:register-stx-range register-stx-range
                                                         (syntax?
                                                          exact-nonnegative-integer?
                                                          exact-nonnegative-integer?
                                                          . -> . any)
                                                         void]
                                   [#:render-stx-hook render-stx-hook
                                                      (syntax? output-port? . -> . any/c)
                                                      (lambda (stx output) #f)])
         string?]{

 Converts a syntax object for an S-expression representation to a string
 form, potentially using @tech{raw text} properties and otherwise falling
 back to @racket[write-shrubbery]. By default, raw text reconstruction is
 used only if raw text is @defterm{consistently available} (as described
 below), but raw text mode can be forced by providing @racket[use-raw?]
 as a true value. When @racket[use-raw?] is true, each syntax object
 without raw text is printed as by @racket[write-shrubbery].

 If @racket[max-length] is a number, the returned string will contain no
 more than @racket[max-length] characters. Internally, conversion to a
 string can take shortcuts once the first @racket[max-length] characters
 have been determined.

 When @racket[keep-suffix?] are @racket[keep-suffix?] are true and raw text mode is used to
 generate the result string, then @racket['raw-prefix] and
 @racket['raw-suffix] text on the
 immediate syntax object are included in the result. Otherwise, prefixes
 and suffixes are rendered only when they appear between @racket['raw]
 text. If @racket[inner?] is true, ``inner'' prefixes and suffixes are
 preserved on the immediate @racket[s] form even if @racket[keep-suffix?]
 and/or @racket[keep-suffix?] are @racket[#false]. If @racket[s] is
 a group or multi-group form, then inner prefixes and suffixes are
 preserved in any case.

 If @racket[infer-starting-indentation?] is true, then a consistent
 amount of leading whitespace is removed from each line of the result
 string.

 The @racket[register-stx-range] and @racket[render-stx-hook] arguments
 provide a hook to record or replace rendering of a syntax object within
 @racket[s]. The @racket[register-stx-range] procedure is called with
 each syntax object in @racket[s] after printing, and the second and
 third arguments report the starting and ending locations in the string
 for the syntax object's printed form. The @racket[render-stx-hook]
 procedure is called before printing each syntax object, and if it
 returns a true value, then printing assumes that the syntax object has
 alerady been rendered to the given output port (which is ultimately
 delivered to a string), and it is not printed in the default way.

 Raw text is @defterm{consistently available} when supplied by
 @racket['raw] syntax properties on all atoms, except that
 @racket['raw-opaque-content] and/or @racket['opaque-raw] properties
 excuse nested atoms from needing @racket['raw] properties. Also,
 a @racket[parsed] form need not have raw text information.

}


@defproc[(shrubbery-syntax->raw [s syntax?]
                                [#:use-raw? use-raw? any/c #f]
                                [#:keep-prefix? keep-prefix? any/c #f]
                                [#:keep-suffix? keep-suffix? any/c #f]
                                [#:inner? inner? any/c #f])
         (values any? any? any?)]{

 Similar to @racket[shrubbery-syntax->string] but delivers raw text
 encodings with the prefix, main, and suffix raw text as separate result
 values. If @racket[keep-prefix?] or @racket[keep-suffix?] is
 @racket[#f], the corresponding result is empty.
            
}


@defproc[(combine-shrubbery-raw [a any/c] [b any/c]) any/c]{

 Combines @racket[a] and @racket[b] with @racket[cons] if they are both
 non-empty, returns the empty list if both are empty, and otherwise
 returns the argument that is non-empty. In addition to normal raw text
 encodings, a @racket[#f] argument counts as empty.

}
