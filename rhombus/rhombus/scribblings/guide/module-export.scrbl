#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "module-provide"){Exports: @rhombus(export)}

By default, all of a module's definitions are private to the module.
The @rhombus(export) form specifies definitions to be made available
where the module is @rhombus(import)ed.

@rhombusblock(
  export:
    #,(@rhombus(export_clause, ~var))
    #,(more_args)
)

An @rhombus(export) form can only appear in a module body or a
@rhombus(namespace) body. Specifying multiple
@rhombus(export_clause, ~var)s in a single @rhombus(export) is the
same as using multiple @rhombus(export) forms each with a single
@rhombus(export_clause, ~var).

Each identifier can be exported at most once from a module across all
@rhombus(export)s within the module. More precisely, the external name
for each export must be distinct; the same internal binding can be
exported multiple times under different external names.

The simplest @rhombus(export_clause, ~var) is just an identifier that
names a binding within the module. The binding can be from either a
local definition or from an import.

@rhombusblock(
  export:
    color
    pi

  def color = "blue"
  def pi = 3.14
)

The @rhombus(export) form also can be combined with a definition to
export the names defined by the definition:

@rhombusblock(
  export def color = "blue"

  export fun greet(who):
    "Hello, " ++ who
)

When a class or namespace is exported, bindings introduced by the
class or namespace---such as the constructor, accessors, predicates,
and methods, or the bindings inside a namespace---are exported along
with it. So, for example,

@rhombusblock(
  export:
    Posn

  class Posn(x, y)
)

exports @rhombus(Posn), @rhombus(Posn.x), @rhombus(Posn.y), and so on.


@doc(
  ~nonterminal:
    export_id: block id
    orig_id: block id
  grammar_case "export":
    $orig_id #,(@rhombus(as, ~impo)) $export_id
){

 A @rhombus(rename, ~expo) form is similar to just specifying an
 identifier, but the exported binding @rhombus(orig_id, ~var) is given a
 different name, @rhombus(export_id, ~var), to importing modules.

@rhombusblock(
  export:
    rename:
      hue as color

  def hue = "blue"
)

}

@doc(
  ~nonterminal:
    namespace_id: block id
  grammar_case "export":
    #,(@rhombus(all_from, ~expo))($module_name)
  grammar_case "export":
    #,(@rhombus(all_from, ~expo))(. $namespace_id)
){

 The @rhombus(all_from, ~expo) form with a @rhombus(module_name, ~var)
 exports all bindings in the module that were imported and exposed from
 that module. This form normally makes sense with a module that is
 imported with @rhombus(open, ~impo) or @rhombus(expose, ~impo).

@rhombusblock(
  import:
    "f2c.rhm" open
  export:
    all_from("f2c.rhm")
)

 The @rhombus(all_from, ~expo) form with @rhombus(.namespace_id)
 re-exports everything that is exported by @rhombus(namespace_id),
 independent of the bindings that are imported or exposed from the
 namespace. A module import without @rhombus(open, ~impo) binds a
 namespace (based on the imported module's name), so this form of
 @rhombus(all_from, ~expo) can be used to re-export all of the bindings
 of another module.

@rhombusblock(
  import:
    "f2c.rhm"
  namespace extras:
    export: freezing_f
    def freezing_f = 32
  export:
    all_from(.f2c)
    all_from(.extras)
)


}

@doc(
  grammar_case "export":
    #,(@rhombus(except, ~expo)) $export_clause
  grammar_case "export":
    #,(@rhombus(except, ~expo)):
      $export_clause
      ...
){

 The @rhombus(except, ~expo) modifier inverts the meaning of an
 @rhombus(export_clause) to exclude the names that it describes, instead
 of including them. The omitted names are the external names of the
 bindings, and @rhombus(except, ~expo) attaches to another
 @rhombus(export_clause) either through the block under the clause or as
 a prefix.

@rhombusblock(
  import:
    "f2c.rhm" open

  export:
    all_from("f2c.rhm"):
      except fahrenheit_freezing
)

}
