#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "examples"){Evaluation and Examples}

@doc(
  expr.macro 'examples($option, ...,
                       $chunk,
                       ...,
                       $repl,
                       ...)'

  grammar option:
    ~label: $content_expr
    ~eval: $evaluator_expr
    ~indent: $indentation_expr
    ~spacer_info_box: $box_expr
    ~escape: $op

  grammar chunk:
    ~defn:
      $repl
      ...
    ~repl:
      $repl
      ...
    ~result:
      $repl
      ...
    ~hidden:
      $repl
      ...
    ~version_and_later $version_string:
      $chunk
      ...

  grammar repl:
    $form
    ~error:
      $form
    ~check:
      $form
      ~is $expect_form
    ~fake:
      $typeset_form
      $result_form
    ~blank
){

 Evaluates @rhombus(form)s, rendering (by default) both the
 @rhombus(form) itself and the result of evaluation.

 The @rhombus(form)s to evaluate are grouped into @rhombus(chunk)s,
 where extra space is rendered between different chunks. When
 @rhombus(repl)s are provided directly to @rhombus(examples) after any
 @rhombus(chunks)s, they are implicitly grouped into a final
 @rhombus(~repl) chunk.

 The keyword for a @rhombus(chunk) determines its rendering:

@itemlist(

  @item{@rhombus(~defn): Each form to evaluate and its results are shown
  without a leading @litchar{>} prompt. Output and error output are also
  collected and rendered in between the input form and result. As the
  keyword suggests, this kind of chunk makes the most sense for
  definitions, which have no result.}

  @item{@rhombus(~repl): Each form to evaluate is shown with a leading
  @litchar{>} prompt, but no prompt is shown for the evaluation's output
  or result.}

  @item{@rhombus(~result): Only output and the result of evaluation are
  shown, and the form that was evaluated is not itself rendered.}

  @item{@rhombus(~hidden): Neither the form to evaluate nor its output
  and result are show. Forms are evaluated only for their side effects.}

  @item{@rhombus(~version_and_later): Splices the contained
  @rhombus(chunks) as long as the runtime system version is at least
  @rhombus(version_string), omits the @rhombus(chunk)s otherwise.}

)

 Each @rhombus(repl) within a @rhombus(chunk) has one of several forms:

@itemlist(

 @item{A plain @rhombus(form): The @rhombus(form) both typeset and
  evaluated. Evaluation must not throw an (uncaught) exception.}

 @item{Using @rhombus(~error): The @rhombus(form) both typeset and
  evaluated, and it must throw an (uncaught) exception. The exception
  message is printed as error output.}

 @item{Using @rhombus(~check): Like a plain @rhombus(form), but
  @rhombus(expect_form) is also evaluated, and the two results must be
  @rhombus(==).}

 @item{Using @rhombus(~fake): The @rhombus(typeset_form) is rendered,
  but not evaluated. The @rhombus(result_form) is not typeset, but it is
  evaluated for its output and result to be rendered.}

 @item{A @rhombus(~blank): No form is typeset or evaluated. Instead, an
  empty line is rendered.}

)

 By default, a fresh evaluator is created with
 @rhombus(make_rhombus_eval) to evaluate the expressions, and the
 evaluator is closed afterward. Supply the @rhombus(~eval) option to use
 a different evaluator.

 The result of a @rhombus(~label) option's expression is used to label
 the examples. When the result is @rhombus(#false), the default, then no
 label is shown.

 The @rhombus(~indent) option can provide an expression to produce a
 nonnegative exact integer. That number of additional spaces is used to
 indent the example. The default is @rhombus(0).

 The @rhombus(~spacer_info_box) option can provide a box the hold spacer
 binding information for linking methods after a @rhombus(.). By default,
 an box specific to the evaluator is created or found (if the @rhombus(~eval)
 option is present) in a weak table.

 The @rhombus(~escape) option selects an alternate escape for rendering
 expression---needed, for example, if the example itself involves a
 @rhombus(#,) escape.

}


@doc(
  fun make_rhombus_eval(~lang: lang :: ModulePath = ModulePath 'rhombus',
                        ~attach: attach :: Any = #true)
  fun close_eval(evaluator)
){

 The @rhombus(make_rhombus_eval) function creates an evaluator for a
 Rhombus-based language specified by @rhombus(lang). The evaluator does
 not necessary need to be closed explicitly, but closing with
 @rhombus(close_eval) may improve memory use.

 If @rhombus(attach) is a true value, then a @rhombus(lang) module
 instance is attached to the namespace where the enclosing document is
 run, and the new evaluator uses that instance in a fresh top-level
 namespace. Using an attached instance can be much faster than freshly
 loading a language in an evaluator namespace, but any state in the
 language ends up shared among evaluators.

}
