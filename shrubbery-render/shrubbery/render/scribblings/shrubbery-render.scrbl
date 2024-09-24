#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      shrubbery/render open
      shrubbery/render/define open)

@title{Shrubbery Rendering}

The @rhombusmodname(shrubbery/render) library provides core support for
rendering forms within a program to external documents, such as
documentation or slide presentations.

@table_of_contents()

@section{Creating Rendering Functions}

@docmodule(shrubbery/render)

@doc(
  fun make(~render: render :: Function.of_arity(2),
           ~render_in_space: render_in_space :: Function.of_arity(3),
           ~render_whitespace: render_whitespace :: Function.of_arity(1),
           ~render_indentation: render_indentation :: Function.of_arity(5),
           ~render_line: render_line :: Function.of_arity(1),
           ~render_one_line: render_one_line :: Function.of_arity(1),
           ~render_lines: render_lines :: Function.of_arity(1),
           ~rendered_shape: rendered_shape :: Function.of_arity(1),
           ~is_rendered: is_rendered :: Function.of_arity(1))
){

 Returns two functions:

@itemlist(

  @item{@rhombus(render_line): renders a group syntax object without
  comments and with simplified whitespace}

  @item{@rhombus(render_block): renders a syntax object with multiple
  groups preserving original comments and whitespace}

)

 These functions are specialized to a rendered form bu the arguments to
 @rhombus(make).

}

@section{Defining Rendering Macros}

@docmodule(shrubbery/render/define)

@doc(
  defn.macro 'macros ($rhombus,
                      $rhombusblock,
                      $rhombusblock_etc):
                ~render_line: $render_line
                ~render_block: $render_block
                ~escape: $escape
                ~result: $Result'
){

 Defines @rhombus(rhombus), @rhombus(rhombusblock), and
 @rhombus(rhombusblock_etc) as syntactic forms that bridge to
 @rhombus(render_line) and @rhombus(render_line).

}
