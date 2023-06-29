#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Input and Output}

@doc(
  fun print(v, out = current_output_port()) :: Void
){

 Prints @rhombus(v) to @rhombus(out).

}

@doc(
  fun println(v, out = current_output_port()) :: Void
){

 Prints @rhombus(v) to @rhombus(out), then prints a newline.

}

@doc(
  fun display(v, out = current_output_port()) :: Void
){

 Displays @rhombus(v) to @rhombus(out).

 Strings, symbols, and keywords display as their character content. A
 byte string displays as its raw byte content. A @tech{syntax object}
 displays as unquoted. Any other predefined kind of value displays the
 same way that it prints, but a class can implemented @rhombus(Printable)
 so that its instances display and print differently.

}

@doc(
  fun displayln(v, out = current_output_port()) :: Void
){

 Displays @rhombus(v) to @rhombus(out) like @rhombus(display), then
 prints a newline.

}

@doc(
  def current_input_port :: Parameter
  fun current_input_port()
  fun current_input_port(in)
){

 A @tech{context parameter} for the current input port.

}

@doc(
  def current_output_port :: Parameter
  fun current_output_port(),
  fun current_output_port(out),
){

 A @tech{context parameter} for the current output port.

}


@doc(
  def current_error_port :: Parameter
  fun current_error_port(),
  fun current_error_port(out),
){

 A @tech{context parameter} for the current error output port.

}

@doc(
  interface Printable
){

@provided_interface_only()

 An interface that a class can implement (publicly or privately) to
 customize the way its objects print. The interface has two methods:

@itemlist(

 @item{@rhombus(#,(@rhombus(print, ~datum))(#,(@rhombus(output_port, ~var))))
   --- takes an output port and prints to the port; this
   method is abstract in the interface, so it must be overridden.}

 @item{@rhombus(#,(@rhombus(display, ~datum))(#,(@rhombus(output_port, ~var))))
   --- takes an output port and displays to the port; this
  method has a default implementation that calls the @rhombus(print, ~datum) method.}

)

}
