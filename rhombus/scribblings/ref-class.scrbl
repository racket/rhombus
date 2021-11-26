#lang scribble/rhombus/manual
@(import: "common.rhm": no_prefix)

@title{Classes}

@doc[
  ~literal: ::,
  defn.macro '(class identifier(field, ...)),
  
  grammar field:
    identifier
    identifier :: annotation
]{

 Binds @rhombus[identifier] as a class name, which serves several roles:

@itemlist[

 @item{a constructor function, which takes as many arguments as the
   supplied @rhombus[field]s and returns an instance of the class;},

 @item{an annotation, which is satisfied by any instance of the class;},

 @item{a pattern constructor, which takes as many patterns as the
   supplied @rhombus[field]s and matches an instance of the class where the
   fields match the corresponding patterns;},

 @item{a dot povider to access accessor functions @rhombus[identifier.field]},

 @item{a dot povider to access an annotation constructor @rhombus[identifier.of],
   which takes as many annotation arguments as supplied @rhombus[field]s.}

]

}
