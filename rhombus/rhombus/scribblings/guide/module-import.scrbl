#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "module-require"){Imports: @rhombus(import)}

The @rhombus(import) form imports from another module. An
@rhombus(import) form can appear within a module, in which case it
introduces bindings from the specified module into the importing
module. An @rhombus(import) form can also appear at the top level (in
a @tech{REPL}), in which case it both imports bindings and
@deftech{instantiates} the specified module; that is, it evaluates the
body definitions and expressions of the specified module, if they
have not been evaluated already.

A single @rhombus(import) form can specify multiple imports at once
using a block of @rhombus(import_clause, ~var)s:

@rhombusblock(
  import:
    #,(@rhombus(import_clause, ~var))
    #,(more_args)
)

Specifying multiple @rhombus(import_clause, ~var)s in a single
@rhombus(import) is essentially the same as using multiple
@rhombus(import) forms, each with a single @rhombus(import_clause, ~var).

The simplest @rhombus(import_clause, ~var) is just a
@tech{module path}. By default, an @rhombus(import) clause that names
a module binds a prefix derived from the last element of the module
path; imports are then accessed using that prefix, @litchar{.}, and
the imported name.

@rhombusblock(
  module m ~lang #,(@rhombuslangname(rhombus)):
    export:
      color
    def color = "blue"

  module n ~lang #,(@rhombuslangname(rhombus)):
    export:
      size
    def size = 17

  import:
    self!m
    self!n

  [m.color, n.size]
)

The @rhombus(import_clause, ~var) shape can be adjusted by attaching
@rhombus(modifier, ~var)s to it.

@doc(
  ~nonterminal:
    id: block
  grammar_case "import":
    $module_path #,(@rhombus(as, ~impo)) $id
){

 The @rhombus(as, ~impo) modifier changes the prefix that is bound for
 the import, instead of using the prefix derived from the module path.

 @margin_note_block{See @secref("Modules") for @rhombus("f2c.rhm").}

@rhombusblock(
  import:
    "f2c.rhm" as convert

  convert.fahrenheit_to_celsius(convert.fahrenheit_freezing)
)

 Using @rhombus(as ~none, ~impo) suppresses the prefix entirely, so that
 only @rhombus(expose, ~impo)d names are bound.

}

@doc(
  grammar_case "import":
    $module_path #,(@rhombus(open, ~impo))
){

 The @rhombus(open, ~impo) modifier imports without a prefix, so each
 imported binding can be referenced by its bare name.

 @margin_note_block{See @secref("Modules") for @rhombus("f2c.rhm").}

@rhombusblock(
  import:
    "f2c.rhm" open

  fahrenheit_to_celsius(fahrenheit_freezing)
)

 Opening an import is sometimes called ``namespace dumping'' and is
 discouraged in some cases. When @rhombus(module_path) provides macros or
 a sublanguage, however, it may be intended for use with
 @rhombus(open, ~impo). The documentation for each Rhombus module either
 shows @rhombus(open, ~impo) to suggest that the module is imported that
 way, or it is documented without @rhombus(open, ~impo).

}

@doc(
  ~nonterminal:
    id: block
    local_id: block id
  grammar_case "import":
    $module_path #,(@rhombus(expose, ~impo)):
      $id_or_rename_as
      ...
  grammar id_or_rename_as
  | $id
  | $id #,(@rhombus(as, ~impo)) $local_id
){

 The @rhombus(expose, ~impo) modifier exposes specific names without a
 prefix, while leaving other imported bindings accessible only through
 the prefix. The exposed bindings are imported with the prefix as well as
 without.

 @margin_note_block{See @secref("Modules") for @rhombus("f2c.rhm").}

@rhombusblock(
  import:
    "f2c.rhm" expose:
      fahrenheit_to_celsius

  fahrenheit_to_celsius(f2c.fahrenheit_freezing)
)

 Combining @rhombus(as ~none, ~impo) and @rhombus(expose, ~impo) is
 similar to Racket's @rhombus(only-in, ~datum), where only the listed
 names are bound.

}

@doc(
  ~nonterminal:
    id: block
    op: block
    id_or_op: block
  grammar_case "import":
    $module_path . $name
  grammar_case "import":
    $module_path . $name #,(@rhombus(as, ~impo)) $id_or_op
  grammar name
  | $id
  | ($op)
  | $id . $name
){

 Following a @rhombus(module) path with a dotted name imports only the
 @rhombus(id) or parenthesized @rhombus(op) at the end of the dotted
 name. That's similar to using @rhombus(expose, ~impo) with one name, but
 no intermediate names are bound. If @rhombus(as, ~impo) is present, the
 imported name has the given name locally.

 @margin_note_block{See @secref("Modules") for @rhombus("f2c.rhm"),
  while @rhombuslangname(rhombus) in this example refers to the same
  module as the @rhombuslangname(rhombus) language, which exports a
  @rhombus(bits, ~datum) namespace that exports @rhombus(bits.(<<)).}

@rhombusblock(
  import:
    "f2c.rhm".fahrenheit_to_celsius
    "f2c.rhm".fahrenheit_freezing as freezing
    rhombus.bits.(<<)

  fahrenheit_to_celsius(freezing) << 1
)

}

@doc(
  ~nonterminal:
    id: block
    local_id: block id
  grammar_case "import":
    $module_path #,(@rhombus(rename, ~impo)):
      $id #,(@rhombus(as, ~impo)) $local_id
      ...
){

 The @rhombus(rename, ~impo) modifier supports renaming individual
 imports. The renamed bindings are still imported through the same prefix
 (or no prefix, if @rhombus(open, ~impo) is also applied), but under a
 new name.

@rhombusblock(
  module m ~lang #,(@rhombuslangname(rhombus)):
    export:
      tastes_great
      less_filling
    def tastes_great = #true
    def less_filling = #true

  import:
    self!m rename:
      less_filling as lite

  [m.tastes_great, m.lite]
)

}

@doc(
  ~nonterminal:
    id: block
    local_id: block id
  grammar_case "import":
    $module_path #,(@rhombus(only, ~impo)):
      $id_or_rename_as
      ...
  grammar id_or_rename_as
  | $id
  | $id #,(@rhombus(as, ~impo)) $local_id
){

 The @rhombus(only, ~impo) modifier limits the imports to a specified set
 of names, optionally renaming.

@rhombusblock(
  import:
    self!m only:
      tastes_great

  m.tastes_great
)

}

@doc(
  ~nonterminal:
    id: block
  grammar_case "import":
    $module_path #,(@rhombus(except, ~impo)):
      $id ...
      ...
){

 The @rhombus(except, ~impo) modifier is the complement of
 @rhombus(only, ~impo): it excludes specific bindings from the import.

@rhombusblock(
  import:
    self!m except:
      tastes_great

  m.less_filling
)

}

The @rhombus(as, ~impo), @rhombus(open, ~impo), @rhombus(expose, ~impo),
@rhombus(rename, ~impo), @rhombus(only, ~impo), and
@rhombus(except, ~impo) modifiers can be combined within a single
@rhombus(import) clause to implement more complex manipulations of
imported bindings. For example,

@rhombusblock(
  import:
    self!m as my_m except:
      tastes_great
)

imports all bindings that @rhombus(m, ~datum) exports, except for
@rhombus(tastes_great), and with local names accessed through the
prefix @rhombus(my_m, ~datum).
