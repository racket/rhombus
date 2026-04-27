#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/shrubbery)

@title(~tag: "linecol"){Counting Positions, Lines, and Columns}

By default, Rhombus keeps track of the @deftech{position} in a port as
the number of bytes that have been read from or written to the port
(independent of the read/write position, which is accessed or changed
with @rhombus(Port.Position)). Optionally, however, Rhombus can track
the position in terms of characters (after UTF-8 decoding), instead of
bytes, and it can track @deftech{line locations} and @deftech{column
 locations}; this optional tracking must be specifically enabled for a
port via @rhombus(Port.locations_enabled) or the
@rhombus(Port.current_enable_locations) parameter. Position, line, and
column locations for a port are used by various reading and parsing
operations, such as @rhombus(shrubbery.read). Position and line
locations are numbered from @math{1}; column locations are numbered from
@math{0}.

When counting lines, Rhombus treats linefeed, return, and
return-linefeed combinations as a line terminator and as a single
position (on all platforms). Each tab advances the column count to one
before the next multiple of @math{8}. When a sequence of bytes in the
range 128 to 253 forms a UTF-8 encoding of a character, the
position/column is incremented once for each byte, and
then decremented appropriately when a complete encoding sequence is
discovered. See also @secref("encoding") for more information on UTF-8
decoding for ports.

A position is known for any port as long as its value can be expressed
as a @rhombus(Fixnum, ~annot) (which is more than enough tracking for
realistic applications in, say, syntax-error reporting). If the position
for a port exceeds the value of the largest @rhombus(Fixnum, ~annot),
then the position for the port becomes unknown, and line and column
tacking is disabled. Return-linefeed combinations are treated as a
single character position only when line and column counting is enabled.
