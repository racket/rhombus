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
@rhombus(|| False, ~annot), but it also cooperates with operators like
@rhombus(!!), which returns its argument only if it is
non-@rhombus(#false) and throws an exception otherwise. If the argument to @rhombus(!!) has static
information from @rhombus(maybe(#,(@rhombus(annot, ~var))), ~annot), then the result from
@rhombus(!!) has the static information of @rhombus(annot, ~var).

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

The @rhombus(||) operator similarly cooerates with @rhombus(maybe, ~annot) for
the left-hand argument to @rhombus(||). Since a @rhombus(#false) result
from the left-hand argument is never used as the result, if it has static
information from @rhombus(maybe(#,(@rhombus(annot, ~var))), ~annot), then it can be
treated as having the static information of @rhombus(annot, ~var).

@examples(
  ~eval: ann_eval
  ~defn:
    fun len(str :: maybe(String)) :: Int:
      use_static
      (str || "").length()
  ~repl:
    len("apple")
    len(#false)
)

Along similar lines, the static information of a @rhombus(&&) expression
corresponds to the static information of its right-hand argument, but
adjusted in the same way as by @rhombus(maybe, ~annot).

@margin_note_block{The @rhombus(&&) operator can serve the same role as
 the elvis operator @litchar{?:} that is provided some other languages.}

@examples(
  ~eval: ann_eval
  ~defn:
    fun ms(name :: maybe(String)) :: maybe(String):
      use_static
      (name && "Ms. ")?.append(name)
  ~repl:
    ms("Smith")
    ms(#false)
)
