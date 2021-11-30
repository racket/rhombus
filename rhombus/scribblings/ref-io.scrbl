#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Input and Output}

@doc[
  fun print(v, out = current_output_port()) :: Void
]{

 Prints @rhombus[v] to @rhombus[out].

}

@doc[
  fun println(v, out = current_output_port()) :: Void
]{

 Prints @rhombus[v] to @rhombus[out], then prints a newline.

}

@doc[
  fun display(v, out = current_output_port()) :: Void
]{

 Displays @rhombus[v] to @rhombus[out].

 Strings, symbols, and keywords display as their character content. A
 byte string displays as its raw byte content. Any other kind of value
 displays the same way that it prints.

}

@doc[
  fun displayln(v, out = current_output_port()) :: Void
]{

 Displays @rhombus[v] to @rhombus[out] like @rhombus[display], then
 prints a newline.

}

@doc[
  fun current_output_port(),
  fun current_output_port(out),
]{

 A parameter for the current output port.

}