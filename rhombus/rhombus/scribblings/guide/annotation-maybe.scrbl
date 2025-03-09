#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "../macro.rhm")

@(def ann_eval = macro.make_macro_eval())

@title(~tag: "annotation-maybe"){Annotations for Optional Values}

Sometimes, a function accepts an optional value, and it makes sense to
allow @rhombus(#false) to indicate ``nothing.'' A function could
annotate such an option with @rhombus(|| False, ~annot).

@examples(
  ~eval: ann_eval
  ~defn:
    fun name(first :: String, last :: String || False):
      first ++ (if last | " " ++ last | "")
  ~repl:
    name("Homer", "Simpson")
    name("Stu", #false)
)

A drawback of using @rhombus(||, ~annot) is that static information is
lost in the case of a non-@rhombus(#false) value. In the preceding
example, @rhombus(" " ++ last) is not statically resolved to string
concatenation.

The @rhombus(maybe, ~annot) annotation constructor is a shorthand for
@rhombus(|| False, ~annot), but it also cooperates with operators such
as @rhombus(!!), which returns its argument only if it is
non-@rhombus(#false). If the argument to @rhombus(!!) has static
information from @rhombus(maybe, ~annot), then the result from
@rhombus(!!) has the static information of @rhombus(maybe, ~annot)'s
argument.

@examples(
  ~eval: ann_eval
  ~defn:
    fun name(first :: String, last :: maybe(String)):
      use_static
      first ++ (if last | " " ++ (last!!) | "")
  ~repl:
    name("Homer", "Simpson")
    name("Stu", #false)
)

The @rhombus(?.) operator is like the @rhombus(.) operator, but it
produces @rhombus(#false) without accessing a field or method if the
expression before @rhombus(?.) produces @rhombus(#false). It also
recognizes @rhombus(maybe, ~annot) static information for the expression
before @rhombus(?.), which enables static dispatch for a
non-@rhombus(#false) value.

@examples(
  ~eval: ann_eval
  ~defn:
    fun len(str :: maybe(String)) :: Int:
      use_static
      str?.length() || 0
  ~repl:
    len("apple")
    len(#false)
)
