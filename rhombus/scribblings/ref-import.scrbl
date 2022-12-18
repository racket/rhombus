#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Import}

@doc(
  defn.macro 'import:
                $import_clause
                ...',

  defn.macro 'import $import_clause',

  grammar import_clause:
    $module_path
    $module_path:
      $modifier
      ...
    $module_path $modifier
    $module_path $modifier:
      $modifier
      ...
    $modifier:
      $import_clause
      ...,
  
  grammar module_path:
    $collection_module_path
    $string
    $$(@rhombus(lib, ~impmod))($string)
    $$(@rhombus(file, ~impmod))($string)
    $module_path $$(@rhombus(!, ~impmod)) $identifier
    $$(@rhombus(., ~impmod)) $identifier
    $module_path $$(@rhombus(., ~impmod)) $identifier,

  grammar collection_module_path:
    $identifier
    $identifier $$(@rhombus(/, ~impmod)) $collection_module_path

){

 Imports into the enclosing module or block. An @rhombus(import) form
 with a single immediate @rhombus(import_clause) is a shorthand for an
 @rhombus(import) form that has a block containing the single
 @rhombus(import_clause).

 The @rhombus(import_clause) variant @rhombus(module_path) or
 @rhombus(module_path: modifier; ...) are the canonical forms. The other
 @rhombus(import_clause) forms are converted into a canonical form:

@itemlist(

 @item{@rhombus(module_path modifier) is the same as
   @rhombus(module_path: modifier), where @rhombus(modifier) might include
   a block argument. This form is handy when only one modifier is needed.},

 @item{@rhombus(module_path modifier: modifier; ...) is the same as
   @rhombus(module_path: modifier; modifier; ...) where the initial
   @rhombus(modifier) does not accept a block argument. This form is
   especially handy when the initial @rhombus(modifier) is @rhombus(open)
   or @rhombus(as $$(@rhombus(identifier,~var))) and additional modifiers
   are needed.},

 @item{@rhombus(modifier: import_clause; ....) is the same as the
   sequence of @rhombus(import_clause)s with @rhombus(modifier) add to the
   @emph{end} of each @rhombus(import_clause). This form is especialy handy
   when @rhombus(modifier) is @rhombus(meta).}

)

 By default, each clause with a @rhombus(module_path) binds a prefix
 name that is derived from the @rhombus(module_path)'s last element.
 Imports from the module are then accessed using the prefix, @litchar{.},
 and the provided-provided name.

 A @rhombus(module_path) clause can be be adjusted through one or more
 @rhombus(import_modifier)s. The set of modifiers is extensible, but
 includes @rhombus(as, ~impmod), @rhombus(rename, ~impmod), and
 @rhombus(expose, ~impmod).

 A @rhombus(module_path) references a module in one of several possible
 forms:

 @itemlist(

 @item{@rhombus(collection_module_path): refers to an installed
   collection library, where the @rhombus(/, ~impmod) operator acts as a path
   separator. Each @rhombus(identifier) in the path is constrained to
   contain only characters allowed in a @rhombus(string) module path, with
   the additional constraint that @litchar{.} is disallowed. A module path
   of this form refers to a file with the @filepath{.rhm} suffix.},

 @item{@rhombus(string): refers to a module using @rhombus(string) as a
   relative path. The string can contain only the characters
   @litchar{a}-@litchar{z}, @litchar{A}-@litchar{Z},
   @litchar{0}-@litchar{9}, @litchar{-}, @litchar{+}, @litchar{_}, and
   @litchar{/}, @litchar{.}, and @litchar{%}. Furthermore, a @litchar{%} is
   allowed only when followed by two lowercase hexadecimal digits, and the
   digits must form a number that is not the ASCII value of a letter,
   digit, @litchar{-}, @litchar{+}, or @litchar{_}.},

 @item{@rhombus($$(@rhombus(lib, ~impmod))(string)): refers to an installed collection library,
   where @rhombus(string) is the library name. The same constraints apply
   to @rhombus(string) as when @rhombus(string) is used as a relative path
   by itself, with the additional constraint that @litchar{.} and
   @litchar{..} directory indicators are disallowed. When @rhombus(string)
   does not end with a file suffix, @filepath{.rhm} is added.},

 @item{@rhombus($$(@rhombus(file, ~impmod))(string)): refers to a file through a
   platform-specific path with no constraints on @rhombus(string).},

 @item{@rhombus(module_path $$(@rhombus(!, ~impmod)) identifier):
  refers to submodule of another module. The submodule name
  @rhombus(identifier) is used as the default import prefix.},

 @item{@rhombus($$(@rhombus(., ~impmod))identifier): refers to a namespace
  @rhombus(identifier), which might be predefined like @rhombus(List), or
  might be bound by @rhombus(namespace) or as a prefix with @rhombus(import).},

 @item{@rhombus(module_path$$(@rhombus(.,~impmod))identifier): a shorthand for importing only
  @rhombus(identifier) from @rhombus(module_path) path and then importing
  with @rhombus(.identifier). The last @rhombus(identifier) in a dotted
  sequence is allowed to be an export that is not a namespace, in which
  case the dotted form is a shorthand for just importing
  @rhombus(identifier) from @rhombus(module_path).}

)

}

@doc(
  impo.macro '$identifier / $collection_module_path'
){

  As a module-path operator, combines @rhombus(identifier) and
  @rhombus(collection_module_path) to build a longer collection-based
  module path.

}

@doc(
  impo.macro '. $identifier',
  impo.macro '$collection_module_path . $identifier'
){

  As an module-path operator, a prefix @rhombus(., ~impmod) refers
  to an import prefix or a namespace @rhombus(identifier) in the enclosing
  environment, and an infix @rhombus(., ~impmod) refers to an
  @rhombus(identifier) provided by @rhombus(collection_module_path).

}


@doc(
  impo.macro 'lib($string)'
){

 Refers to an installed collection library, where @rhombus(string) is
 the library name. See @rhombus(import) for more information.

}


@doc(
  impo.macro 'file($string)'
){

 Refers to a file through a platform-specific path, where
 @rhombus(string) is the library name. See @rhombus(import) for more
 information.

}


@doc(
  impo.macro '$module_path ! $identifier'
){

 Refers to a submodule name @rhombus(identifier) of the module
 referenced by @rhombus(module_path). See @rhombus(import) for more
 information.

}


@doc(
  impo.modifier 'as $identifier'
){

 Modifies an @rhombus(import) clause to bind the prefix
 @rhombus(identifier), used to access non-exposed imports, instead of
 inferring a prefix identifier from the module name.

}

@doc(
  impo.modifier 'open'
){

 Modifies an @rhombus(import) clause so that no prefix (normally based on the
 module name) is bound, so all imports are exposed.

}

@doc(
  impo.modifier 'expose:
                   $identifier ...
                   ...'
){

 Modifies an @rhombus(import) clause so that the listed
 @rhombus(identifier)s are imported without a prefix. The exose
 identifiers remain accessible though the import's prefix, too.

}

@doc(
  impo.modifier 'rename:
                   $identifier $$(@rhombus(as, ~impmod)) $local_identifier
                   ...'
){

 Modifies an @rhombus(import) clause so that @rhombus(local_identifier)
 is used in place of the imported identifier name @rhombus(identifier).
 The new name @rhombus(local_identifier) applies to modifiers after the
 @rhombus(rename) modifier.
  
}

@doc(
  impo.modifier 'only:
                   $identifier ...
                   ...'
){

 Modifies an @rhombus(import) clause so that only the listed
 @rhombus(identifier)s are imported.

}

@doc(
  impo.modifier 'except:
                     $identifier ...
                     ...'
){

 Modifies an @rhombus(import) clause so that the listed
 @rhombus(identifier)s are @emph{not} imported.

}

@doc(
  impo.modifier 'meta',
  impo.modifier 'meta $phase'
){

 Modifies an @rhombus(import) clause so that the imports are shifted by
 @rhombus(phase) levels, where @rhombus(phase) defaults to @rhombus(1).

 This modifier is valid only for module paths that refer to modules,
 as opposed to @rhombus(namespace) bindings, and it is not currently
 supported for module paths that use the @rhombus(., ~impmod) operator.

}

@doc(
  impo.modifier 'meta_label'
){

 Modifies an @rhombus(import) clause so that only the imports that would
 be at phase @rhombus(0) are imported, and they are imported instead to
 the label phase.

 This modifier is valid only for module fies that refer to modules,
 as opposed to @rhombus(namespace) bindings, and it is not currently
 supported for module paths that use the @rhombus(., ~impmod) operator

}