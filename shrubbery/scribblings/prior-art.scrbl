#lang scribble/rhombus/manual

@title{Prior Art}

Indentation-sensitive parsing and the use of @litchar{:} is obviously
informed by Python.

Sampling notation's rules relating indentation, lines, @litchar{;}, and
@litchar{:} are originally based on the
@hyperlink["https://github.com/tonyg/racket-something"]{#lang something}
reader, which also targets an underlying expander that
further groups tokens. Shrubbery notation evolved away from using
@litchar{{}} for blocks, however, because @litchar{:} was nearly always
preferred in experiments with the notation. For the very rare case that
explicit grouping is needed for a block, @litchar{«} and @litchar{»} can
be used. Freeing @litchar{{}} from use for blocks, meanwhile, allows its
use for set and map notations.

Shrubbery notation is also based on
@hyperlink["https://github.com/jeapostrophe/racket2-rfcs/blob/lexpr/lexpr/0004-lexpr.md"]{Lexprs},
particularly its use of @litchar{|}. Lexprs uses mandatory @litchar{:} and @litchar{|} tokens
as a prefix for indentation, and it absorbs an additional line after
an indented section to allow further chaining of the group. Although
@litchar{«»} can be used to form multiple subgroups within a shrubbery group,
the notation discourages that style in favor of further nesting (or,
in the case of @litchar{if}, in favor of @litchar{|} notation like other
conditionals).

Shrubbery notation is in some sense a follow-up to
@hyperlink["https://github.com/mflatt/racket2-rfcs/blob/sapling/sapling/0005-sapling.md"]{sapling notation}.
The primary difference is that shrubbery notation is
indentation-sensitive, while sapling notation is
indentation-insensitive. Indentation sensitivity and block conventions
in shrubbery notation avoid some delimiters and blank lines that are
needed in sapling notation.

More generally, shrubbery notation takes inspiration from
S-expressions and alternative S-expression notations. The idea that,
even in an S-expression-like setting, some parsing can be deferred a
later parser has many precedents, including Clojure's choice of where
to put parentheses and notations that use something like @litchar{$} to escape
to infix mode.
