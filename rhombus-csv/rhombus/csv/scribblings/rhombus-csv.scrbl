#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      rhombus/csv)

@title{Rhombus CSV Reading and Writing}

@docmodule(rhombus/csv)

The @rhombusmodname(rhombus/csv) module provides functions for reading
and writing @as_indexed{comma-separated value (CSV)} files and
@as_indexed{tab-separated value (TSV)} files.

CSV is a weakly standardized format, so @rhombus(csv.Reader, ~class) and
@rhombus(csv.Writer) support a variety of configurations. The default
@rhombus(csv.Reader, ~class) configuration parses files that follow the
@hyperlink("https://www.rfc-editor.org/rfc/rfc4180"){RFC 4180} standard,
but it will also accept variants, such as files that use a linefeed (LF)
terminator instead of carriage return plus linefeed (CRLF). The default
@rhombus(csv.Writer) configuration conforms to RFC 4180.

@section{CSV Reader}

@doc(~include rhombus/csv:
       csv.Reader){

 Creates a CSV reader (or TSV reader, depending on the configuration),
 where fields configure the reader's behavior. Call the
 @rhombus(csv.Reader.tsv) method to get a reader whose configuration is
 adjusted for tab-separated values.

@itemlist(

 @item{@rhombus(line_mode): Configures how line terminators are
  recognized in the same way as for @rhombus(Port.Input.read_line). An
  end-of-file is always treated as a line terminator, too, when it follows
  input that does not have a terminator.}

 @item{@rhombus(separator_chars): Configures a set of characters that
  are treated as field separators within a line.}

 @item{@rhombus(quote_chars): Configures a set of characters that are
  treated as quoting a field when one appears at the beginning of a
  field.}

 @item{@rhombus(quote_doubling_escapes): Configures whether a quote
  character twice in a row escapes the character within a quoted field.}

 @item{@rhombus(newlines_in_quotes): Configures whether a quoted field
  can contain a newline, where the notion of a ``newline'' depends on
  @rhombus(line_mode). If newlines are not allowed in a quoted field, then
  an exception is thrown when one is encountered.}

 @item{@rhombus(space_after_quotes): Configures whether a quoted field
  can be followed by one or more space characters before the end of the
  field (as determined by a separator character or the end of the line).
  An error is reported when the reader encounters extra non-space
  characters after a close quote or if it encountered a space character
  when @rhombus(space_after_quotes) is @rhombus(#false).}

 @item{@rhombus(comment_chars): Configures a set of characters that
  comment out a line of input when one appears at the start of the line.}

 @item{@rhombus(trim): Configures a function for adjusting unquoted
  fields (but not quoted fields). For example, @rhombus(String.trim) might
  be a suitable trimming function.}

)

}

@doc(~include rhombus/csv:
       csv.Reader.read
       csv.Reader.read_line){

 The @rhombus(csv.Reader.read) method reads lines of a CSV file. It
 returns a list where each element is a line, and each line is
 represented by a list that has a string for each field.

 The @rhombus(csv.Reader.read_line) method reads a single line of a CSV
 file and returns a list for a singe line, or it returns
 @rhombus(Port.eof) if @rhombus(inp) reports an immediate end-of-file.
 The @rhombus(csv.Reader.read_line) method can read multiple lines of
 input from @rhombus(inp) if the reader is configured to recognize
 comments, and in that case, the result is @rhombus(Port.eof) is an
 end-of-file is encountered after comment lines.

}

@doc(~include rhombus/csv:
       csv.Reader.tsv){

 Returns a @rhombus(csv.Reader, ~annot) that is like @rhombus(reader),
 but with the separator configuration changed to @rhombus({ Char"\t" }).

}

@section{CSV Writer}

@doc(~include rhombus/csv:
       csv.Writer){

 Creates a CSV writer (or TSV writer, depending on the configuration),
 where fields configure the writer's behavior. Call the
 @rhombus(csv.Writer.tsv) method to get a writer whose configuration is
 adjusted for tab-separated values.

@itemlist(

 @item{@rhombus(newline): Configures the string used to terminate lines.}

 @item{@rhombus(separator_char): Configures the string used to separate
  fields. When this character appears in a field, the field is quoted.}

 @item{@rhombus(quote_char): Configures the string used to quote fields.
  When this character appears in a field, the field is quoted, and the
  quote character is used twice in the quoted form to represent itself as
  a character in the field.}

 @item{@rhombus(extra_quote_chars): Configures additional characters
  that are treated as quotes, a field must be quoted (using
  @rhombus(quote_char)) when it contains one of these characters, and the
  character is doubled within the quoted field to represent itself as
  a character in the field.}

 @item{@rhombus(extra_special_chars): Configures additional characters
  that trigger quoting of a field when they appear in the field, but they
  can represent themselves (without doubling) in a quoted field.}

)

}

@doc(~include rhombus/csv:
       csv.Writer.write
       csv.Writer.write_line){

 The @rhombus(csv.Writer.write) method writes lines of a CSV file. It
 accepts a list where each element is a line, and each line is a list
 that has a string for each field.

 The @rhombus(csv.Writer.write_line) method reads a single line of a CSV
 file. It accepts a list for a singe line.

}

@doc(~include rhombus/csv:
       csv.Writer.tsv){

 Returns a @rhombus(csv.Writer, ~annot) that is like @rhombus(writer),
 but with the separator configuration changed to @rhombus({ Char"\t" }).

}
