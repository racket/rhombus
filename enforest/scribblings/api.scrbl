#lang scribble/manual

@title{Enforestation API}

The Rhombus expander's primary API consists of three Racket structure
types and one macro:

@itemlist[

 @item{A @racket[name-root] structure with one field:

  @itemlist[

        @item{@racket[proc]: a transformer procedure, which takes an introducer
        funciton and a syntactic list of tokens and returns two values: a new
        head identifier or operator token, and a new tail syntactic list of
        tokens. The introducer function corresponds to the space that is
        otherwise used to lookup identifiers, which the transformer procedure
        might take into account.}

      ]

   A @racket[prop:name-root] structure type property is also provided, and
   @racket[name-root?] refers to implementations of @racket[prop:name-root]. The
   property value must be a function that takes a structure
   implementing the property and returns a @racket[name-root] instance. The
   @racket[name-root] structure implements @racket[prop:name-root] with a function that
   results the structure instance itself.}

 @item{An @racket[operator] structure type with @racket[infix-operator]
  and @racket[prefix-operator] structure subtypes. An @racket[operator]
  structure has

  @itemlist[
            
      @item{@racket[name]: an identifier syntax object}

      @item{@racket[precs]: an association list from identifiers
        to @racket['stronger], @racket['weaker], @racket['same], @racket['same-on-left], or
        @racket['same-on-right], indicating that the
        @racket[operator] instance's precedence is stronger than, weaker
        than, or the same as (when on the indicated side of) the
        associated identifier; @racket['default] can
        be used instead of an identifier to specify a default
        relationship}

      @item{@racket[protocol]: @racket['macro] or @racket['automatic]}

      @item{@racket[proc]: a transformer procedure, where the arguments and
        results depend on whether the operator is prefix or infix,
        macro or automatic}

   ]

   A @racket[prefix-operator] has no additional fields.

   An @racket[infix-operator] has one additional field:

   @itemlist[

      @item{@racket[assoc]: @racket['left], @racket['right], or @racket['none]}

   ]
   
   These structure types are provided by the @racket[enforest/operator]
   library.}

 @item{A @racket[define-enforest] macro that parameterizes the enforestation
   algorithm over the following, which are all optional:

   @itemlist[

   @item{@racket[#:enforest]: the name to bind as an enforest function
      (defaults to a fresh name), which is used to start enforestation
      of a group. It takes a list of S-expressions for parsed
      shrubberies that were in a group, and it returns a parsed form.
      The enforest function drives a loop that calls the
      enforest-step function.}

   @item{@racket[#:enforest-step]: the name to bind as an enforest-step function
      (defaults to a fresh name), which continues an enforestation. It
      takes two arguments, which is the list of renaming terms in a
      group and the current operator. The result is two values: a
      parsed form and the remaining sequence of terms (starting with
      an infix operator that has lower precedence than the input
      operator).}

   @item{@racket[#:syntax-class]: the name of a syntax class (see
      @racket[syntax-parse]) to bind (defaults to a fresh name), which
      matches a @racket[group] shrubbery representation and enforests it. A
      match has an @racket[parsed] attribute representing the enforestation
      result.}

  @item{@racket[#:relative-precedence]: an optional name to bind as a
      function that compares the precedence of two operators. The
      function takes four arguments: @racket['infix] or
      @racket['prefix] for the left operator's mode, the left
      operator's identifier, @racket['infix] or @racket['prefix] for
      the right operator's mode, and the right operator's identifier.
      The result is either @racket['stronger] (left takes precedence),
      @racket['weaker] (right takes precedence), or an error results:
      @racket[#f] (no relation), @racket['inconsistent-prec],
      @racket['inconsistent-assoc], @racket['same] (error because no
      associativity), @racket['same-on-left] (error because on right),
      @racket['unbound] (one of the operators is not bound).}

   @item{@racket[#:prefix-more-syntax-class] and @racket[#:infix-more-syntax-class]:
      names of syntax classes (defaulting to fresh names) that take
      an operator-name argument and match a group that's intended to represent
      the tail of a group. The syntax class continues
      enforestation based on the precedence and associatvity of the given operator.
      A match has a @racket[parsed] attribute for the parsed
      result and a @racket[tail] attribute for the remaining terms in
      a group.}

    @item{@racket[#:desc] and @racket[#:operator-desc]: strings used in error reporting
      to refer to a form or an operator for a form. The defaults are
      @racket["form"] and @racket["operator"].}

    @item{@racket[#:in-space]: a function that takes an identifier syntax object
      and adds a space scope if the enforesting context uses a space.
      The default is @racket[values].}

    @item{@racket[#:prefix-operator-ref] and @racket[#:infix-operator-ref]: functions
      that take a compile-time value and extract an instance of
      @racket[prefix-operator] or @racket[infix-operator], respectively, if the
      value has one suitable for the context, returning @racket[#f]
      otherwise. Normally, these functions use structure-property
      accessors. The defaults are @racket[prefix-operator-ref] and
      @racket[infix-oerator-ref].}

    @item{@racket[#:name-path-op]: an operator name that is recognized after a
      name-root identifier for hierarhical name references. The
      default is @racket['|.|].}

    @item{@racket[#:name-root-ref]: a function that takes a
      compile-time value and extracts an instance of
      @racket[name-root], used only on the compile-time value of an
      identifier that is followed by the @racket[#:name-path-op]
      operator.}

    @item{@racket[#:check-result]: a function that takes the result of an
      operator and checks whether the result is suitable for the
      context, used for earlier detection of errors; the
      @racket[check-result] function should either raise an exception or
      return its argument (possibly adjusted). A second argument to
      the function is the procedure that produced the result, which
      can be used for error checking. The default function checks that
      its first argument is a syntax object and returns it.}

    @item{@racket[#:make-identifier-form]: a function that takes an identifier an
      produces a suitable parsed form. If a context does not have a
      meaning for unbound identifiers, @racket[make-identifier-form] can
      report a syntax error. The default is @racket[values].}

    @item{@racket[#:make-operator-form]: a function that takes an operator an
      produces a suitable parsed form, or @racket[#f] to have an error
      tiggered for an unbound operator in an expression position.}

    @item{@racket[#:juxtapose-implicit-name]: a symbol for the implicit infix
      form used on an identifier after a parsed term with no infix
      operator in between. The default is @racket['#%juxtapose].}

    @item{@racket[#:select-implicit-prefix]: a function that takes a term within
      a group that is not an operator and does not follow an infix
      operator. The result should be two values: a symbol for an
      implicit prefix form name and a syntax object whose lexical
      context is added to the symbol to look up the implement binding.
      The default is described in @secref["implicit-ops"].}

    @item{@racket[#:select-implicit-infix]: a function that takes a term within a
      group that is not an operator and follows a parsed term. The
      result should be two values: a symbol for an implicit infix form
      name and a syntax object whose lexical context is added to the
      symbol to look up the implement binding. The default is
      described in @secref["implicit-ops"].}

    ]

   The @racket[define-enforest] macro is provided by the @racket[enforest] library.}

 ]

To support simple contexts that have only prefix transformers and name
roots, the Rhombus expander API provides an additional structure type
and macro:

@itemlist[

 @item{A @racket[transformer] structure type with one field:

    @itemlist[
              
    @item{@racket[proc]: a procedure that takes a list of terms and returns a
      parsed term}
   ]
    
   The @racket[transformer] structure type is provided by the
   @racket[enforest/transform] library.}

 @item{A @racket[define-transform] macro that defines a syntax class to
   trigger parsing given the following, which are all optional:

   @itemlist[

    @item{@racket[#:syntax-class]: the name of the syntax class to define
      (defaults to a fresh name), which matches a @racket[group] shrubbery
      representation and parses it. A match has an @racket[parsed] attribute
      representing the parsed result.}

    @item{@racket[#:desc]: string used in error reporting to refer to a form. The
      default is @racket["form"].}

    @item{@racket[#:transformer-ref]: function that takes a compile-time value
      and extract an instance of @racket[transformer], if the value has one
      suitable for the context, returning @racket[#f] otherwise. Normally,
      these functions use structure-property accessors. The default is
      @racket[transformer-ref].}

    @item{@racket[#:name-path-op]: an operator name that is recognized after a
      name-root identifier for hierarhical name references. The
      default is @racket['|.|].}

    @item{@racket[#:name-root-ref]: analogous to @racket[define-enforest].}

    @item{@racket[#:check-result]: a function that takes the result of an
      transformer and checks whether the result is suitable for the
      context, used for earlier detection of errors; the
      @racket[check-result] function should either raise an exception or
      return its argument (possibly adjusted). A second argument to
      the function is the procedure that produced the result, which
      can be used for error checking. The default function checks that
      its first argument is a syntax object and returns it.}

    ]

   The @racket[define-transform] function is provided by the
   @racket[enforest/transform] library.}

 ]
