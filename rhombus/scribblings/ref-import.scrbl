#lang scribble/rhombus/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open)

@title{Import}

@doc(
  defn.macro 'import:
                $import_clause
                ...'

  defn.macro 'import $import_clause'

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
      ...
  
  grammar module_path:
    $collection_module_path
    $string
    #,(@rhombus(lib, ~impo))($string)
    #,(@rhombus(file, ~impo))($string)
    $module_path #,(@rhombus(!, ~impo)) $id
    #,(@rhombus(., ~impo)) $id
    $module_path #,(@rhombus(., ~impo)) $id
    $module_path #,(@rhombus(., ~impo)) ($op)
    #,(@rhombus(self, ~impo))
    #,(@rhombus(parent, ~impo))

  grammar collection_module_path:
    $id
    $id #,(@rhombus(/, ~impo)) $collection_module_path

  grammar modifier:
    #,(@rhombus(as, ~impo)) $id
    #,(@rhombus(open, ~impo))
    #,(@rhombus(expose, ~impo)) $expose_decl
    #,(@rhombus(rename, ~impo)) $rename_decl
    #,(@rhombus(only, ~impo)) $only_decl
    #,(@rhombus(except, ~impo)) $except_decl
    #,(@rhombus(meta, ~impo)) $meta_decl
    #,(@rhombus(meta_label, ~impo))
    #,(@rhombus(only_space, ~impo)) $only_space_decl
    #,(@rhombus(except_space, ~impo)) $except_space_decl
    $other_modifier

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
   or @rhombus(as #,(@rhombus(id,~var))) and additional modifiers
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
 includes @rhombus(as, ~impo), @rhombus(rename, ~impo), and
 @rhombus(expose, ~impo).

 A @rhombus(module_path) references a module in one of several possible
 forms:

 @itemlist(

 @item{@rhombus(collection_module_path): refers to an installed
   collection library, where the @rhombus(/, ~impo) operator acts as a path
   separator. Each @rhombus(id) in the path is constrained to
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

 @item{@rhombus(#,(@rhombus(lib, ~impo))(string)): refers to an installed collection library,
   where @rhombus(string) is the library name. The same constraints apply
   to @rhombus(string) as when @rhombus(string) is used as a relative path
   by itself, with the additional constraint that @litchar{.} and
   @litchar{..} directory indicators are disallowed. When @rhombus(string)
   does not end with a file suffix, @filepath{.rhm} is added.},

 @item{@rhombus(#,(@rhombus(file, ~impo))(string)): refers to a file through a
   platform-specific path with no constraints on @rhombus(string).},

 @item{@rhombus(module_path #,(@rhombus(!, ~impo)) id):
  refers to submodule of another module. The submodule name
  @rhombus(id) is used as the default import prefix.},

 @item{@rhombus(#,(@rhombus(., ~impo))id): refers to a namespace
  @rhombus(id), which might be predefined like @rhombus(List), or
  might be bound by @rhombus(namespace) or as a prefix with @rhombus(import).},

 @item{@rhombus(module_path#,(@rhombus(.,~impo))id): a shorthand for importing only
  @rhombus(id) from @rhombus(module_path) path and then importing
  with @rhombus(.id). The last @rhombus(id) in a dotted
  sequence is allowed to be an export that is not a namespace, in which
  case the dotted form is a shorthand for just importing
  @rhombus(id) from @rhombus(module_path).}

 @item{@rhombus(module_path#,(@rhombus(.,~impo))(op)): the same
  shorthand, but for operators.}
 
 @item{@rhombus(self, ~impo): refers to the enclosing module itself,
  usually combined with @rhombus(!, ~impo) to refer to a submodule of the
  enclosing module.}

 @item{@rhombus(parent, ~impo): refers to the parent of the enclosing
  submodule, somtimes combined with @rhombus(!, ~impo) to refer to a
  sibling submodule.}

)

}

@doc(
  ~nonterminal:
    collection_module_path: import
  impo.macro '$id / $collection_module_path'
){

  As a module-path operator, combines @rhombus(id) and
  @rhombus(collection_module_path) to build a longer collection-based
  module path.

}

@doc(
  ~nonterminal:
    collection_module_path: import
  impo.macro '. $id'
  impo.macro '$collection_module_path . $id'
  impo.macro '$collection_module_path . ($op)'
){

  As an module-path operator, a prefix @rhombus(., ~impo) refers
  to an import prefix or a namespace @rhombus(id) in the enclosing
  environment, and an infix @rhombus(., ~impo) refers to an
  @rhombus(id) or @rhombus(op)  provided by
  @rhombus(collection_module_path).

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
  ~nonterminal:
    module_path: import
  impo.macro '$module_path ! $id'
){

 Refers to a submodule name @rhombus(id) of the module
 referenced by @rhombus(module_path). See @rhombus(import) for more
 information.

}

@doc(
  impo.macro 'self'
  impo.macro 'parent'
){

 Refers to the module itself to reference a submodule of the enclosing
 module, or refers to the paren module of a submodule. See
 @rhombus(import) for more information.

}


@doc(
  impo.modifier 'as $id'
){

 Modifies an @rhombus(import) clause to bind the prefix
 @rhombus(id), used to access non-exposed imports, instead of
 inferring a prefix id from the module name.

}

@doc(
  impo.modifier 'open'
){

 Modifies an @rhombus(import) clause so that no prefix (normally based on the
 module name) is bound, so all imports are exposed.

}

@doc(
  impo.modifier 'expose $id'
  impo.modifier 'expose:
                   $id ...
                   ...'
){

 Modifies an @rhombus(import) clause so that the listed
 @rhombus(id)s are imported without a prefix. The exposed
 ids remain accessible though the import's prefix, too.

}

@doc(
  ~nonterminal:
    local_id: block id
  impo.modifier 'rename $id #,(@rhombus(as, ~impo)) $local_id'
  impo.modifier 'rename:
                   $id #,(@rhombus(as, ~impo)) $local_id
                   ...'
){

 Modifies an @rhombus(import) clause so that @rhombus(local_id)
 is used in place of the imported id name @rhombus(id).
 The new name @rhombus(local_id) applies to modifiers after the
 @rhombus(rename) modifier.
  
}

@doc(
  impo.modifier 'only $id'
  impo.modifier 'only:
                   $id ...
                   ...'
){

 Modifies an @rhombus(import) clause so that only the listed
 @rhombus(id)s are imported.

}

@doc(
  impo.modifier 'except $id'
  impo.modifier 'except:
                     $id ...
                     ...'
){

 Modifies an @rhombus(import) clause so that the listed
 @rhombus(id)s are @emph{not} imported.

}

@doc(
  impo.modifier 'meta',
  impo.modifier 'meta $phase'
){

 Modifies an @rhombus(import) clause so that the imports are shifted by
 @rhombus(phase) levels, where @rhombus(phase) defaults to @rhombus(1).

 This modifier is valid only for module paths that refer to modules,
 as opposed to @rhombus(namespace) bindings, and it is not currently
 supported for module paths that use the @rhombus(., ~impo) operator.

}

@doc(
  impo.modifier 'meta_label'
){

 Modifies an @rhombus(import) clause so that only the imports that would
 be at phase @rhombus(0) are imported, and they are imported instead to
 the label phase.

 This modifier is valid only for module fies that refer to modules,
 as opposed to @rhombus(namespace) bindings, and it is not currently
 supported for module paths that use the @rhombus(., ~impo) operator

}

@doc(
  impo.modifier 'only_space $id'
  impo.modifier 'only_space: $id ...'
  impo.modifier 'except_space $id'
  impo.modifier 'except_space: $id ...'
){

 Modifies an @rhombus(import) clause to include bindings only in the
 specifically listed @tech{spaces} or only in the spaces not specifically
 listed.

}
