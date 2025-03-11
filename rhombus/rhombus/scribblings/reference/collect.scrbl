#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/collect)

@title{Module Collection Paths}

@docmodule(rhombus/collect)

@doc(
  annot.macro 'collect.CollectionString'
){

 Satisfied by a @rhombus(String) that conforms to the syntax of a
 collection path: containing only the characters @litchar{a}-@litchar{z},
 @litchar{A}-@litchar{Z}, @litchar{0}-@litchar{9}, @litchar{-},
 @litchar{+}, @litchar{_}, @litchar{/}, or @litchar{%}. A @litchar{%} is
 allowed only when followed by two lowercase hexadecimal digits, and the
 digits must form a number that is not the ASCII value of a letter,
 digit, @litchar{-}, @litchar{+}, or @litchar{_}.

}

@doc(
  fun collect.file_path(
    ~collect: coll :: collect.CollectionString,
    ~file: file ::  PathString.to_path && Path.Relative,
    ~infer_from_compiled: infer :: Any.to_boolean
  ) :: maybe(Path)
){

 Reports a path for @rhombus(file) within the collection named by
 @rhombus(coll). Note that the location of @rhombus(coll) is itself is
 not necessarily unique, since multiple packages may supply modules for
 the collection, but each @rhombus(file) within a collection is meant to
 be unique (and the package system rejects conflicts).

}

@doc(
  fun collect.path(coll :: collect.CollectionString) :: maybe(Path)
){

 Reports a directory path for the collection named by @rhombus(coll).
 Beware that @rhombus(coll) is not necessarily unique, and is
 @rhombus(collect.file_path) is normally a better choice.

}
