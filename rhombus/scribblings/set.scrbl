#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@(def set_eval = make_rhombus_eval())

@title(~tag: "set"){Sets}

When @litchar("{")...@litchar("}") is used with elements that do not
have @rhombus(:) to separate a key and value, then @litchar("{")...@litchar("}") creates a set. (If
a set-element expression uses @rhombus(:), then it will need to be in
parentheses to avoid being parsed as a key–value pair.) A set can serve
as a map, where the set's elements act as keys and each key's value is
@rhombus(#true). There's a @rhombus(Set) constructor that's analogous to
@rhombus(Map), but @rhombus(Set) accepts just values to include in the
set. The @rhombus(++) operator effectively unions sets.

@(demo:
    ~eval: set_eval
    ~defn:
      def friends = {"alice", "bob", "carol"}
    ~repl:
      if friends["alice"] && friends["carol"]
      | "I know both"
      | "Who are they?"
    ~defn:
      def new_friends = friends ++ {"david"}
    ~repl:
      new_friends["david"]
      friends["david"]
  )

Using @rhombus(Set) explicitly before @litchar("{")...@litchar("}")
disables the special treatment of @rhombus(:) to indicate a map, and
each element within @litchar("{")...@litchar("}") is simply an
expression. The @rhombus(Set) constructor can also be used like a
function with @litchar("(")...@litchar(")") instead of
@litchar("{")...@litchar("}").

@rhombus(Set.of) and @rhombus(MutableSet) work as you'd expect. When
@litchar{[}...@litchar{]} with @rhombus(:=) is used to modify a mutable
set, the ``key'' is removed from the set if the assigned value is
@rhombus(#false), otherwise the ``key'' is added to the set.

Within a set construction using @litchar("{")...@litchar("}"), a
@rhombus(&) form splice a set into the constructed set, analogous to the
way @rhombus(&) works for list constructions.

@(demo:
    ~eval: set_eval
    {"dennis", & friends}
  )

Also similar to maps, a repetition can be used to construct a set.

@(demo:
    ~defn:
      def [elem, ...] = ["a", "b", "a", "c", "c"]
    ~repl:
      {elem, ...}
  )
