#lang scribble/manual
@(require "rhm_id.rhm"
          (for-label racket/base
                     racket/contract/base
                     syntax/parse
                     shrubbery/parse
                     shrubbery/write
                     shrubbery/print))

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

prints @racketresult['(top (group 1 (op +) 2))]. But if @filepath{demo.rkt} contains

@racketmod[#:file "demo.rkt"
 racket/base
 (require (for-syntax racket/base
                      syntax/parse))

 (provide (rename-out [module-begin #%module-begin])
          + - * /)

 (define-syntax (module-begin stx)
   (syntax-parse stx
     #:datum-literals (top group op)
     [(_ (top (group n1:number (op o) n2:number)))
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
                    [#:source source any/c]
                    [#:mode mode (or/c 'top 'interactive 'line 'text) 'top]
                    [#:start-column start-column exact-nonnegative-integer? 0])
         (or/c eof-object? syntax?)]{

 Parses shrubbery notation from @racket[in] and returns an S-expression
 representation as described in @secref["parsed-rep"] as a syntax object.

 The result syntax object has no scopes, but it has source-location
 information and @seclink["raw-text"]{raw-text properties}.
 Source-location information is never associated with the ``parentheses''
 of the syntax object. Instead, source-location information and other properties
 for a shrubbery @litchar{()}, @litchar{[]}, @litchar["{}"], or
 @litchar{''} is associated with the @racket[parens],
 @racket[brackets], @racket[braces], or @racket[quotes] identifier.
 Similarly, source-location information and properties for a
 @litchar{:} block or @litchar{|} alternatives are
 recorded on a @racket[block] or @racket[alts] identifier. A
 @racket[group] identifier in the representation has only
 @seclink["raw-text"]{raw-text properties} for text before and after the
 group elements. For an operator, source-location information and properties
 are associated to the operator identifier, and not the
 wrapper @racket[op] identifier. Each structuring identifier like
 @racket[group], @racket[block], @racket[parens], or @racket[op]
 has the @racket['identifier-as-keyword] syntax property as @racket[#t].

 The default @racket['top] mode read @racket[in] until an end-of-file,
 and it expects a sequence of groups that are indented consistently
 throughout (i.e., all starting at the same column). The @racket['text]
 mode is similar, but it starts in ``text'' mode, as if the entire input
 is inside curly braces of an @litchar["@"] form (see
 @secref["at-notation"]).

 The @racket['interactive] and @racket['line] modes are similar. They
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

@section[#:tag "raw-text"]{Raw-Text Properties}

@defmodule[shrubbery/property]

A syntax object produced by @racket[parse-all] includes properties that
allow the original shrubbery text to be reconstructed from the syntax
object. Furthermore, this raw-text information is distributed among
syntax objects in a way that helps it stay preserved to a useful degree
on subterms as they are rearranged by enforestation and macro expansion.
The @racketmodname[shrubbery/property] module exports identifiers bound to the
property-key symbols, which can be helpful to avoid a typo in a quoted
symbol.

The property values are trees of strings: a string, an empty list, or a
pair containing two trees. Raw text can be reconstructed through a
preorder traversal of the tree.

@deftogether[(
@defproc*[([(syntax-raw-property [stx syntax?]) any/c]
           [(syntax-raw-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw] preserved property, which records the original
 text of an atomic term, such as a number, string, or identifier.

 For example, the input @litchar{0x11} will be parsed as the number
 @racket[17] with a @racket['raw] property value @racket["0x11"].

 For an identifier such as @racket[parens] that represents groups within
 an opener--closer pair, @racket['raw] text will be attached to the
 identifier for the opener text, while @racket['raw-tail] will be
 attached to the same identifier for the closer text.

 For @racket[op] and @racket[group] wrapper identifiers, an empty
 @racket['raw] property is associated with the identifier. An explicit
 empty property cooperates with inference in
 @racket[shrubbery-syntax->string] for whether raw-text
 properties should be used.

}

@deftogether[(
@defproc*[([(syntax-raw-prefix-property [stx syntax?]) any/c]
           [(syntax-raw-prefix-property [stx syntax?] [val any/c]) syntax?])]
@defproc*[([(syntax-raw-suffix-property [stx syntax?]) any/c]
           [(syntax-raw-suffix-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-prefix] preserved property, which records
 original text that is before a term and not part of a preceding term,
 or adjusts or inspects the @racket['raw-suffix] preserved
 property, which records original text after a term and before the next term. Such text
 is eligible for either the @racket['raw-suffix] property of one term or
 the @racket['raw-prefix] property of a following term;
 @racket['raw-suffix] will be preferred, but @racket['raw-prefix] must be
 used for text (such as whitespace and comments) before a leading term in
 a group---although associated to the group, instead of the leading term.
 Similarly, text after the last term in a group will be associated using
 @racket['raw-prefix] on the group, instead of the last term in the
 group.

 For example, the input @litchar{ 1 +  2 // done} will be parsed into the
 S-expression representation @racket['(top (group 1 (op +) 2))]. The
 syntax object for @racket[group] will have a @racket['raw-prefix] value
 equivalent to @racket[" "] and a @racket['raw-suffix] value equivalent to
 @racket[" // done"], but possibily within a tree structure instead of a
 single string. The syntax object for @racket[1] and will have a
 @racket['raw-suffix] value equivalent to @racket[" "], while the syntax
 object for @racket[+] and will have a @racket['raw-suffix] value
 equivalent to @racket["  "] (i.e., two spaces).

}

@deftogether[(
@defproc*[([(syntax-raw-tail-property [stx syntax?]) any/c]
           [(syntax-raw-tail-property [stx syntax?] [val any/c]) syntax?])]
@defproc*[([(syntax-raw-tail-suffix-property [stx syntax?]) any/c]
           [(syntax-raw-tail-suffix-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['raw-tail] and @racket['raw-tail-suffix]
 preserved properties, which are
 attached to an identifier like @racket[parens] that represents groups
 within an opener--closer pair. The @racket['raw-tail] property holds
 text for the closer, which belongs after the last contained group in the
 @racket[parens] identifier's sequence. The @racket['raw-tail-suffix]
 property is analogous to @racket['raw-suffix], but goes after the closer
 that is recorded as @racket['raw-tail].

 For example, the input @litchar{(1 + 2) * 4 //done} will be parsed into
 the S-expression representation
 @racket['(top (group (parens (group 1 (op +) 2)) (op *) 4))]. The syntax
 object for the outer @racket[group] will have a @racket['raw-suffix]
 value equivalent to @racket[" // done"]. The syntax object for
 @racket[parens] will have a @racket['raw] value equivalent to
 @racket["("], a @racket['raw-tail] value equivalent to @racket[")"], and
 a @racket['raw-tail-suffix] value equivalent to @racket[" "]. The inner
 @racket[group] syntax object will have no properties or ones with values
 that are equivalent to empty strings.

}

@deftogether[(
@defproc*[([(syntax-opaque-raw-property [stx syntax?]) any/c]
           [(syntax-opaque-raw-property [stx syntax?] [val any/c]) syntax?])]
)]{

 Adjusts or inspects the @racket['opaque-raw] non-preserved property,
 which is like @racket['raw], but intended to be used in place of
 whatever raw-form information is reported for nested syntax object
 within @racket[stx].

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
 S-expression representation as @racket[v].

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
 syntax object, so it cannot use @seclink["raw-text"]{raw-text properties}.
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
                                   [#:keep-suffix? keep-suffix? any/c #f]
                                   [#:infer-starting-indentation? infer-starting-indentation? any/c #t]
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
 form, potentially using @seclink["raw-text"]{raw-text properties}. By
 default, raw-text reconstruction is used only if all atomic terms in
 @racket[s] have a @racket['raw] property value, but raw-text mode can be
 forced by providing @racket[use-raw?] as a true value. When
 @racket[use-raw?] is true, each syntax object without a @racket['raw]
 property is treated as if the property value is empty.

 If @racket[max-length] is a number, the returned string will contain no
 more than @racket[max-length] characters. Internally, conversion to a
 string can take shortcuts once the first @racket[max-length] characters
 have been determined.

 When @racket[keep-suffix?] is true and raw-text mode is used to
 generate the result string, then @racket['raw-prefix],
 @racket['raw-suffix], and @racket['raw-tail-suffix] text on the
 immediate syntax object are included in the result. Otherwise, prefixes
 and suffixes are rendered only when they appear between @racket['raw]
 text.

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

}
