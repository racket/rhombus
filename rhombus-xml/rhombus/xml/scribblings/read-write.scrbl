#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      xml)

@title(~tag: "read-write"){Reading and Writing XML}

@doc(
  fun xml.read(
    ~in: inp :: Port.Input = Port.Input.current(),
    ~read_post_misc: read_post_misc = #true
  ) :: xml.Document
  fun xml.write(
    doc :: xml.Document || xml.Element,
    ~out: outp :: Port.Output = Port.Output.current(),
    ~indentation: indentation :: xml.Indentation = #'none
  ) :: Void
){

 Reads an XML document into a @rhombus(xml.Document, ~class) object or
 writes a @rhombus(xml.Document, ~class) object as an XML document.
 Supplying a @rhombus(xml.Element, ~class) object to @rhombus(xml.write)
 is a shortcut for supplying a document that contains just the element.

 The @rhombus(xml.read) function normalizes input when the value of
 @rhombus(xml.current_read_normalize) is @rhombus(#true). In that case,
 multiple consecutive @rhombus(xml.Text, ~class) objects in content are
 collapsed into a single @rhombus(xml.Text, ~class) object that has the
 @rhombus(xml.Text.WriteMode.default) write mode.

 By default, @rhombus(xml.write) does not add newlines or other
 whitespace that is not explicitly present in @rhombus(doc). If
 @rhombus(indentation) is @rhombus(#'scan), then whitespace is added
 within each element unless any content within the element (potentially
 nested in other elements) is a @rhombus(xml.PCData, ~class) or
 @rhombus(xml.Entity, ~class). If @rhombus(indentation) is
 @rhombus(#'peek), then printing is like @rhombus(#'scan), except that
 only the immediate content of an element is checked, and nested
 @rhombus(xml.PCData, ~class) or @rhombus(xml.Entity, ~class) do not
 disable whitespace.

}

@doc(
  fun xml.from_string(str :: String) :: xml.Document
  fun xml.from_bytes(bstr :: Bytes) :: xml.Document
  fun to_string(
    doc :: xml.Document || xml.Element,
    ~indentation: indentation :: Indentation = #'none
  ) :: String
  fun xml.to_bytes(
    doc :: xml.Document || xml.Element,
    ~indentation: indentation :: Indentation = #'none
  ) :: Bytes
){

 Shortcuts for @rhombus(xml.read) and @rhombus(xml.write) to read or
 write XML documents as strings or byte strings.

}


@doc(
  Parameter.def xml.current_read_normalize :: Any.to_boolean:
    #true
){

 Then this parameter's value is @rhombus(#true), @rhombus(xml.read)
 collapses consecutive text content to a @rhombus(xml.Text, ~class) object.

}

@doc(
  Parameter.def xml.current_read_comments :: Any.to_boolean:
    #false
){

 Unless this parameter's value is @rhombus(#true), @rhombus(xml.read)
 discards comments.

}

@doc(
  Parameter.def xml.current_read_processing_instructions :: Any.to_boolean:
    #true
){

 When this parameter's value is @rhombus(#false), @rhombus(xml.read)
 discards processing instructions.

}

@doc(
  Parameter.def xml.current_read_count_bytes :: Any.to_boolean:
    #false
){

 If this parameter's value is @rhombus(#true), @rhombus(xml.read) counts
 by bytes for columns and offsets in source locations. If the parameter's
 value if @rhombus(#false), @rhombus(xml.read) counts by characters,
 instead.

}

@doc(
  Parameter.def xml.current_read_collapse_whitespace :: Any.to_boolean:
    #false
){

 If this parameter's value is @rhombus(#true), @rhombus(xml.read)
 collapses consecutive whitespace characters to a single space.

}

@doc(
 Parameter.def xml.current_write_empty_shorthand :: Any.to_boolean:
   #false
){

 If this parameter's value is @rhombus(#true), @rhombus(xml.write)
 prints elements with empty content using @litchar{<}@italic{tag}…@litchar{/>}.

}

@doc(
  enum xml.Indentation:
    none
    peek
    scan
){

 Indentation modes for @rhombus(xml.write).

}
