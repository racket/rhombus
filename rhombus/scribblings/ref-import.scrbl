#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Import}

@doc[
  decl.macro '(import:
                 $module_path:
                   $import_modifier
                   ...
                 ...),
  
  grammar module_path:
    $collection_module_path
    $string
    lib($string)
    file($string),

  grammar collection_module_path:
    $identifier
    $identifier / $collection_module_path

]{

 Imports into the enclosing module.

 By default, each clause with a @rhombus[module_path] binds a prefix
 name that is derived from the @rhombus[module_path]'s last element.
 Imports from the module are then accessed using the prefix, @litchar{.},
 and the provided-provided name.

 A @rhombus[module_path] clause can be be adjusted through one or more
 @rhombus[import_modifier]s. The set of modifiers is extensible, but
 includes @rhombus[no_prefix, ~impmod], @rhombus[rename, ~impmod], and
 @rhombus[expose, ~impmod].

 A @rhombus[module_path] references a module in one of several possible
 forms:

 @itemlist[

 @item{@rhombus[collection_module_path]: refers to an installed
   collection library, where the @rhombus[/] operator acts as a path
   separator. Each @rhombus[identifier] in the path is constrained to
   contain only characters allowed in a @rhombus[string] module path, with
   the additional constraint that @litchar{.} is disallowed.},

 @item{@rhombus[string]: refers to a module using @rhombus[string] as a
   relative path. The string can contain only the characters
   @litchar{a}-@litchar{z}, @litchar{A}-@litchar{Z},
   @litchar{0}-@litchar{9}, @litchar{-}, @litchar{+}, @litchar{_}, and
   @litchar{/}, @litchar{.}, and @litchar{%}. Furthermore, a @litchar{%} is
   allowed only when followed by two lowercase hexadecimal digits, and the
   digits must form a number that is not the ASCII value of a letter,
   digit, @litchar{-}, @litchar{+}, or @litchar{_}.},

 @item{@rhombus[lib(string)]: refers to an installed collection library,
   where @rhombus[string] is the library name. The same constraints apply
   to @rhombus[string] as when @rhombus[string] is used as a relative path
   by itself, with the additional constraint that @litchar{.} and
   @litchar{..} directory indicators are disallowed.},

 @item{@rhombus[file(string)]: refers to a file through a
   platform-specific path with no constraints on @rhombus[string].}
 
]

}

@doc[
  imp.modifier '(prefix $identifier)
]{

 Modifies an @rhombus[import] clause to bind the prefix
 @rhombus[identifier], used to access non-exposed imports, instead of
 inferring a prefix identifier from the module name.

}

@doc[
  imp.modifier '(no_prefix)
]{

 Modifies an @rhombus[import] clause so that no prefix (normally based on the
 module name) is bound, so all imports are exposed.

}

@doc[
  imp.modifier '(expose:
                   $identifier ...
                   ...)
]{

 Modifies an @rhombus[import] clause so that the listed
 @rhombus[identifier]s are imported without a prefix. The exose
 identifiers remain accessible though the import's prefix, too.

}

@doc[
  imp.modifier '(rename:
                   $identifier ~to $local_identifier
                   ...)
]{

 Modifies an @rhombus[import] clause so that @rhombus[local_identifier]
 is used in place of the imported identifier name @rhombus[identifier].
 The new name @rhombus[local_identifier] applies to modifiers after the
 @rhombus[rename] modifier.
  
}

@doc[
  imp.modifier '(only:
                   $identifier ...
                   ...)
]{

 Modifies an @rhombus[import] clause so that only the listed
 @rhombus[identifier]s are imported.

}

@doc[
  imp.modifier '(except:
                   $identifier ...
                   ...)
]{

 Modifies an @rhombus[import] clause so that the listed
 @rhombus[identifier]s are @emph{not} imported.

}

@doc[
  imp.modifier '(for_meta),
  imp.modifier '(for_meta phase)
]{

 Modifies an @rhombus[import] clause so that the imports are shifted by
 @rhombus[phase] levels, where @rhombus[phase] defaults to @rhombus[1].

}

@doc[
  imp.modifier '(for_label)
]{

 Modifies an @rhombus[import] clause so that only the imports that would
 be at phase @rhombus[0] are imported, and they are imported instead to
 the label phase.

}