Internal Implementation Notes
=============================

Binding
-------

The expression binding space corresponds to the default space. By
using the default space for expressions, a local expression binding
shadows (in a sense) a binding in any other space from an encloing
scope. That's why

  namespace p:
    export x
    def x = 0
  begin:
    class Posn(x, y)
    def p = Posn(1, 2)
    p.x

unambiguously accesses the `x` field of the posn `p`, and does not
access the `x` field of the namespace `p`. Similarly,

  def [x, ...] = [1, 2, 3]
  begin:
    def x = 1
    [x, ...]

reports an error about iteration depth instead of accessing the outer
repetition `x`.

Export in the default space *only* for expression, definition, and
declaration forms, or for internal bindings that are not visible from
Rhombus. If you bind other things in the default space, they will be
found (because it's the default space), but attempting to filter
imports based on the space will not work right.

For non-expression spaces, use forms like `bind-quote` to create a
constant syntax object in that space, and use functions like
`in-binding-space` as needed to move an identifier in the right space
(i.e., to add that space's scope). Generally, represent identifier
other that fully expanded terms without adding a space scope, and add
the space scope at the last minute. Avoid `#:literals` in
`syntax-parse`, even for the expression space, because it's easy to
slip into not thinking about which space the comparison should use;
it's difficult to forget when using something like `expr-quote`.

Although `in-expression-space` exists, it's just the identity
function. Don't bother using `in-expression-space` to add a scope just
in case some scope is used in the future; it's noisy, it's difficult
to keep up with adding the identity function in the right places, and
that possible change has been tried and is really unlikely to ever be
a good idea. Do prefer `expr-quote` to just `quote-syntax` or `#'`,
though, since it's useful documentation (and a reminder) without being
much more noisy.

For matching tags, prefer syntax classes like `:parens` over
`(~datum parens)` or `#:datum-literals (parens)`; the syntax classes
provide better error message, and it's less easier to mess up than
manually adding entries to `#:datum-literals`.
