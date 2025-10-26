#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/shrubbery)

@title(~tag: "Shrubbery"){Shrubbery Input}

@docmodule(rhombus/shrubbery)

@doc(
  fun shrubbery.read(in :: Port.Input,
                     ~mode: mode :: shrubbery.ReadMode = #'top,
                     ~start_column: start_column :: Int = 0)
    :: Syntax || Port.eof
  enum shrubbery.ReadMode:
    top
    interactive
    line
    text
){

 Reads a shrubbery form from @rhombus(in). The result can be
 @rhombus(Port.eof) only in @rhombus(#'interactive) or @rhombus(#'line)
 modes.

@itemlist(

  @item{@rhombus(#'top) mode reads all shrubbery forms until an
  end-of-file, and it produces a term, group, or multi-group sequence.}

  @item{@rhombus(#'interactive) mode reads until an end-of-line, unless
  nothing has been read so far, an opener remains unclosed, or a
  @rhombus{:} was encountered that not within an open-closer sequence. If
  reading continues due to a @litchar{:}, then it stops when a blank line
  is found (where a line containing a comment does not count as blank), as
  long as stopping does not create an empty block.}

  @item{@rhombus(#'line) mode is like @rhombus(#'interactive) mode,
  except that it can stop and produce an empty term sequence if a newline
  is found without having read anything.}

  @item{@rhombus(#'text) mode reads until an end-of-file, but starting as
  if inside @litchar|{@{}}|, and the result is a @brackets form.}

)

@examples(
  ~defn:
    import rhombus/shrubbery
  ~repl:
    shrubbery.read(Port.Input.open_string("fun (x):\n  x + 1"))
)

}
