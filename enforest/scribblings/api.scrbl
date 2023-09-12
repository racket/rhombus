#lang scribble/manual
@(require (for-label enforest
                     enforest/name-root
                     enforest/operator
                     enforest/transformer
                     racket/base
                     racket/contract/base
                     syntax/parse))

@title{Enforestation API}

@section{Name Roots}

@defmodule[enforest/name-root]

@defproc[(name-root [proc procedure?])
         name-root?]{

 Returns an implementation of @racket[prop:name-root] with a function
 that results in the structure instance itself.

 The transformer procedure @racket[proc] takes an introducer funciton
 and a syntax list of terms and returns two values: a new head
 identifier or operator, and a syntax list of remaining terms. The
 introducer function corresponds to the space that is otherwise used
 to lookup identifiers, which the transformer procedure might take
 into account.

}

@defproc[(name-root-proc [v any/c])
         procedure?]{

 Extracts the transformer procedure from @racket[v], which must be an
 instance of @racket[name-root].

}

@defthing[prop:name-root struct-type-property?]{

 A structure type property whose value must be a function that takes a
 structure implementing the property and returns an instance of
 @racket[name-root].

}

@defproc[(name-root? [v any/c])
         boolean?]{

 Returns @racket[#t] if @racket[v] implements @racket[prop:name-root],
 @racket[#f] otherwise.

}

@defproc[(name-root-ref [v any/c])
         any/c]{

 Returns an instance of @racket[name-root] if @racket[v] implements
 @racket[prop:name-root], @racket[#f] otherwise.

}

@section{Operators}

@defmodule[enforest/operator]

@defstruct*[operator ([name identifier?]
                      [precs (listof (cons/c (or/c identifier? 'default)
                                             (or/c 'stronger 'weaker
                                                   'same 'same-on-left 'same-on-right)))]
                      [protocol (or/c 'macro 'automatic)]
                      [proc procedure?])
            #:omit-constructor]{

 The @racket[name] identifier is the name of the operator.

 The @racket[precs] list is an association list from identifiers to
 @racket['stronger], @racket['weaker], @racket['same],
 @racket['same-on-left], or @racket['same-on-right], indicating that
 the operator's precedence is stronger than, weaker than, or the same
 as (when on the indicated side of) the associated identifier;
 @racket['default] can be used instead of an identifier to specify a
 default relationship.

 The @racket[protocol] is either @racket['macro] for
 @tech{macro protocol} or @racket['auto] for
 @tech{automatic protocol}.

 The @racket[proc] transformer procedure depends on the mode (prefix
 or infix) as well as the @racket[protocol] for its arguments and
 results.

 This structure type should not be used directly. Instead, use its
 subtypes @racket[prefix-operator] and @racket[infix-operator].

}

@defstruct*[(prefix-operator operator) ()]{

 If the @racket[protocol] is @racket['macro], @racket[proc] takes a
 syntax list of terms consisting of all remaining terms (including the
 operator term) in the enclosing group and returns two values: a
 parsed form, and a syntax list of remaining terms.

 If the @racket[protocol] is @racket['auto], @racket[proc] takes two
 arguments: a parsed right-hand form and the operator name, and
 returns a further parsed form.

}

@defproc[(prefix-operator-ref [v any/c])
         any/c]{

 Returns @racket[v] if it is an instance of @racket[prefix-operator],
 @racket[#f] otherwise.

}

@defstruct*[(infix-operator operator) ([assoc (or/c 'left 'right 'none)])]{

 The @racket[assoc] is either @racket['left] for left-associativity,
 @racket['right] for right-associativity, or @racket['none] for
 non-associativity.

 If the @racket[protocol] is @racket['macro], @racket[proc] takes two
 arguments: a parsed left-hand form and a syntax list of terms
 consisting of all remaining terms (including the operator term) in
 the enclosing group, and returns two values: a parsed form, and a
 syntax list of remaining terms.

 If the @racket[protocol] is @racket['auto], @racket[proc] takes three
 arguments: a parsed left-hand form, a parsed right-hand form, and the
 operator name, and returns a further parsed form.

}

@defproc[(infix-operator-ref [v any/c])
         any/c]{

 Returns @racket[v] if it is an instance of @racket[infix-operator],
 @racket[#f] otherwise.

}

@section{Parameterized Enforestation}

@defmodule[enforest]

@(define prefix-operator-ref/default @racket[prefix-operator-ref])
@(define infix-operator-ref/default @racket[infix-operator-ref])
@(define name-root-ref/default @racket[name-root-ref])
@defform[(define-enforest option ...)
         #:grammar ([option
                     (code:line #:enforest enforest)
                     (code:line #:enforest-step enforest-step)
                     (code:line #:syntax-class syntax-class)
                     (code:line #:relative-precedence relative-precedence)
                     (code:line #:prefix-more-syntax-class prefix-more-syntax-class)
                     (code:line #:infix-more-syntax-class infix-more-syntax-class)
                     (code:line #:desc desc)
                     (code:line #:operator-desc operator-desc)
                     (code:line #:in-space in-space)
                     (code:line #:prefix-operator-ref prefix-operator-ref)
                     (code:line #:infix-operator-ref infix-operator-ref)
                     (code:line #:name-path-op name-path-op)
                     (code:line #:name-root-ref name-root-ref)
                     (code:line #:check-result check-result)
                     (code:line #:make-identifier-form make-identifier-form)
                     (code:line #:make-operator-form make-operator-form)
                     (code:line #:juxtapose-implicit-name juxtapose-implicit-name)
                     (code:line #:select-implicit-prefix select-implicit-prefix)
                     (code:line #:select-implicit-infix select-implicit-infix)
                     ])]{

 The @racket[enforest] name (defaulting to a fresh name) is bound to
 an enforest function, which is used to start enforestation of a
 group. It takes a syntax list of S-expressions for parsed shrubberies
 in a group and returns a parsed form. The enforest function drives a
 loop that calls the enforest-step function.

 The @racket[enforest-step] name (defaulting to a fresh name) is bound
 to an enforest-step function that continues an enforestation. It
 takes two arguments: a syntax list of all remaining terms in a group
 and the current operator, and returns two values: a parsed form and a
 syntax list of remaining terms (starting with an infix operator that
 has lower precedence than the input operator).

 The @racket[syntax-class] name (defaulting to a fresh name) is bound
 to a syntax class (see @racket[define-syntax-class]) that matches and
 enforests a @racket[group] shrubbery representation. A match has a
 @racket[parsed] attribute for the enforestation result.

 The @racket[relative-precedence] name, if present, is bound to a
 function that compares the precedence of two operators. The function
 takes four arguments: @racket['infix] or @racket['prefix] for the
 left operator's mode, the left operator's identifier, @racket['infix]
 or @racket['prefix] for the right operator's mode, and the right
 operator's identifier. The result is either @racket['stronger] (left
 takes precedence), @racket['weaker] (right takes precedence), or an
 error result: @racket[#f] (no precedence relation),
 @racket['inconsistent-prec] (inconsistent precedence),
 @racket['inconsistent-assoc] (inconsistent associativity),
 @racket['same] (same precedence but non-associativity),
 @racket['same-on-left] (same precedence but on the wrong side),
 @racket['unbound] (one of the operators is unbound).

 The @racket[prefix-more-syntax-class] and
 @racket[infix-more-syntax-class] names (defaulting to fresh names)
 are bound to syntax classes that take an operator-name argument, and
 match and enforest a group that's intended to represent the tail of a
 group. The enforestation is based on the precedence and associatvity
 of the given operator. A match has a @racket[parsed] attribute for
 the enforestation result and a @racket[tail] attribute for the
 remaining terms in a group.

 The @racket[desc] (defaulting to @racket["form"]) and
 @racket[operator-desc] (defaulting to @racket["operator"]) strings
 are used in error reporting to refer to a form or an operator for a
 form.

 The @racket[in-space] function (defaulting to the identity function)
 takes an identifier syntax object and adds a space scope if the
 enforesting context uses a space.

 The @racket[prefix-operator-ref] (defaulting to
 @prefix-operator-ref/default) and @racket[infix-operator-ref]
 (defaulting to @infix-operator-ref/default) functions take a
 compile-time value and extract an instance of
 @racket[prefix-operator] or @racket[infix-operator], respectively, if
 the value has one suitable for the context, returning @racket[#f]
 otherwise. Normally, these functions use structure-property
 accessors.

 The @racket[name-path-op] symbol (defaulting to @racket['|.|]) is an
 operator name that is (symbolically) recognized after a name-root
 identifier for hierarhical name references.

 The @racket[name-root-ref] function (defaulting to
 @name-root-ref/default) takes a compile-time value and extracts an
 instance of @racket[name-root]. The compile-time value is resolved
 from an identifier that is followed by an operator recognized by
 @racket[name-path-op].

 The @racket[check-result] function (defaulting to a function that
 checks whether its first argument is a syntax object, returning it
 as-is if so, raising an exception otherwise) takes an enforestation
 result and checks whether the result is suitable for the context. It
 should either raise an exception for earlier detection of errors or
 return its first argument (possibly adjusted). A second argument to
 the function is the procedure that produced the result, which can be
 used for error reporting (e.g., through @racket[object-name]).

 The @racket[make-identifier-form] function (defaulting to the
 identity function) takes an identifier an produces a suitable parsed
 form. If a context does not have a meaning for unbound identifiers, a
 syntax error can be reported.

 The @racket[make-operator-form] function, if non-@racket[#f], takes
 an operator and produces a suitable parsed form. Absence or
 @racket[#f] means that an error is reported for an unbound operator
 in an expression position.

 The @racket[juxtapose-implicit-name] symbol (defaulting to
 @racket['#%juxtapose] as described in @secref["implicit-ops"]) is the
 name for the implicit infix operator between two immediately adjacent
 expressions.

 The @racket[select-implicit-prefix] function (defaulting to a
 function that selects implicit prefix operators as described in
 @secref["implicit-ops"]) takes a term within a group that is not an
 operator and does not follow an infix operator. The result should be
 two values: a symbol as the name for the implicit prefix operator and
 a syntax object whose lexical context is added to the symbol to look
 up the operator binding.

 The @racket[select-implicit-infix] function (defaulting to a
 function that selects implicit infix operators as described in
 @secref["implicit-ops"]) takes a term within a group that is not an
 operator and follows a parsed term. The result should be two values:
 a symbol as the name for the implicit infix operator and a syntax
 object whose lexical context is added to the symbol to look up the
 operator binding.

}

@section{Prefix Transformers}

@defmodule[enforest/transformer]

@defstruct*[transformer ([proc procedure?])]{

 The @racket[proc] transformer procedure takes a syntax list of all
 terms (including the resolved name) in a group and returns a parsed
 term.

}

@defproc[(transformer-ref [v any/c])
         any/c]{

 Returns @racket[v] if it is an instance of @racket[transformer],
 @racket[#f] otherwise.

}

@(define transformer-ref/default @racket[transformer-ref])
@defform[(define-transform option ...)
         #:grammar ([option
                     (code:line #:syntax-class syntax-class)
                     (code:line #:desc desc)
                     (code:line #:transformer-ref transformer-ref)
                     (code:line #:name-path-op name-path-op)
                     (code:line #:name-root-ref name-root-ref)
                     (code:line #:check-result check-result)])]{

 The @racket[syntax-class] name (defaulting to a fresh name) is bound
 to a syntax class that matches and parses a @racket[group] shrubbery
 representation. A match has a @racket[parsed] attribute for the
 parsed result.

 The @racket[desc] string (defaulting to @racket["form"]) is used in
 error reporting to refer to a form.

 The @racket[transformer-ref] function (defaulting to
 @transformer-ref/default) takes a compile-time value and extracts an
 instance of @racket[transformer], if the value has one suitable for
 the context, returning @racket[#f] otherwise. Normally, this function
 uses a structure-property accessor.

 See @racket[define-enforest] for the meanings of @racket[desc],
 @racket[name-path-op], @racket[name-root-ref], and
 @racket[check-result].

}
