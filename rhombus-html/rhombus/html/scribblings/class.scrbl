#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      html)

@title(~tag: "class"){HTML Document Representation}

@doc(
  enum html.Content
  | ~is_a html.Element
  | ~is_a String
  | ~is_a html.Comment
){

 An enumeration of representations used for the content of HTML
 documents: an @rhombus(html.Element, ~annot) for content that is written
 in HTML roughly as,
 @litchar{<}@italic{name}@litchar{>}…@litchar{<}/@italic{name}@litchar{>}
 as contains nested content, a @rhombus(String, ~annot) for textual
 content, or @rhombus(html.Comment, ~annot) for a comment written as
 @litchar{<!--}…@litchar{-->}. This content is within
 @rhombus(html.Document, ~annot) to represent an overall HTML document.

}

@doc(
  class html.Document(
    ~content: content :: List.of(html.Content) = [],
    ~doctype: doctype :: maybe(html.DocumentType) = html.DocumentType(),
    ~quirks_mode: quirks_mode :: html.Document.QuirksMode = #'no_quirks
  )

  class html.DocumentType(
    ~name: name :: String = "html",
    ~public: public :: maybe(String) = #false,
    ~system: system :: maybe(String) = #false
  )

  enum html.Document.QuirksMode
  | no_quirks
  | quirks
  | limited_quirks
){

 Represents an HTML document.

 The @rhombus(content) field is normally the most interesting component.

 The @rhombus(doctype) field is a @rhombus(html.DocumentType, ~annot) to
 represent an HTML document that starts @litchar{<!DOCTYPE}…@litchar{>}.
 The @rhombus(name) field in @rhombus(html.DocumentType, ~annot) is the
 name after @litchar{DOCTYPE}, which is normally @litchar{html}. The
 @rhombus(public) and @rhombus(system) fields will be values other than
 @rhombus(#false) only for legacy specifications within @litchar{DOCTYPE}.

 The @rhombus(quirks_mode) fields of @rhombus(html.Document, ~annot) is
 based on the @rhombus(doctype) field, and it indicates certain aspects
 of an input document was parsed.

}

@doc(
  class html.Element(
    ~name: name :: String = "div",
    ~attributes: attributes :: List.of(html.Attribute) = [],
    ~content: content :: List.of(html.Content) = [],
    ~is_template: is_template :: Boolean = #false,
    ~is_self_closing: is_self_closing :: Boolean = #false,
    ~namespace: namespace :: Symbol = #'html
  )

  class html.Attribute(
    ~name: name :: String,
    ~value: value :: String
  )
){

 Represents an element within an HTML document that is written roughly
 as
 @litchar{<}@rhombus(name)…@litchar{>}…@litchar{<}/@rhombus(name)@litchar{>},
 optionally with attributes withing the opening tag and/or content
 between the opening and closing tag.

 When parsing HTML with @rhombus(html.read), @rhombus(name) is
 case-folded, but @rhombus(html.write) uses @rhombus(name) verbatim; in
 particular, it assumes that the name is valid for an HTML element.
 Similarly, @rhombus(html.read) case-folds @rhombus(name) for an
 attribute, and @rhombus(html.write) assumes that the name is valid. When
 no value is supplied for an attribute, @rhombus(html.read) uses
 @rhombus("") as the attribute's @rhombus(value).

 The @rhombus(is_template) field will have @rhombus(#true) when the
 element is parsed from a HTML document using @litchar("element") as the
 element name and @rhombus(#'html) as the namespace. HTML template content
 is parsed specially.

 The @rhombus(is_self_closing) field is @rhombus(#true) when the element
 is---or should be, per the HTML specification---parsed from
 @litchar{<}@rhombus(name)…@litchar{/>}. The @rhombus(html.write)
 function uses this field to print similarly, as long as the
 @rhombus(content) field has @rhombus([]).

 The @rhombus(namespace) field indicates the namespace for
 @rhombus(name). It is normally @rhombus(#'html), but @rhombus(#'svg) and
 @rhombus(#'mathml) are other possibilities.

}

@doc(
  class html.Comment(~data: data :: String = "")
){

 Represents a comment within an HTML document, which is written as
 @litchar{<!--}…@litchar{-->}.

}

@doc(
  class html.Inject(~text: text :: String)
){

 Represents literal text to be written as-is within an HTML document (or
 a document that is otherwise valid HTML).

 Use @rhombus(html.Inject, ~class) to create output that does not adhere
 to the HTML specification or to create valid HTML in a format other than
 the one that @rhombus(html.write) would use.

}
