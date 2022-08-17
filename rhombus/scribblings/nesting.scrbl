#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title{Nested Definitions}

A module import as @rhombus(convert.fahrenheit_to_celsius) or class
field accessors as @rhombus(Posn.x) and @rhombus(Posn.y) demonstrate
some hierarchical names. Some other hierarchical names are provided by
@rhombusmodname(rhombus), such as @rhombus(List.length) and
@rhombus(List.cons) via @rhombus(List) (where lists will discussed more
in @secref("list")):

@(rhombusblock:
    List.length(["a", "b", "c"])  // prints 3
  )

Use the @rhombus(nest) form to create a nesting level without creating a
sepaarte module. The identifier after @rhombus(nest) is bound as a
nesting name, and @rhombus(export) provide forms within the
@rhombus(nest) body determine the bindings that can be accessed from the
nested name with @rhombus(.).

@(rhombusblock:
   nest Math:
     export:
       tau
       Complex
     val pi: 3.14
     val tau: 2 * pi
     class Complex(real, imag)

   Math.tau                   // prints 6.28
   // Math.pi                 // would be an error
   Math.Complex(0, Math.tau)  // prints Complex(0, 6.28)
)

A name defined with @rhombus(nest) can be used with @rhombus(import),
but the name must be prefixed with @rhombus(.) to distinguish it from a
module path Also, @rhombus(import) can be used in nested blocks
generally, such as a block created with @rhombus(begin) or
@rhombus(val):

@(rhombusblock:
   begin:
     import: .Math open
     Complex(0, tau)  // prints Complex(0, 6.28)

   val also_pi:
     import: .Math open
     tau / 2
   also_pi            // prints 3.14
  )

Naturally, nested names can be nested further, either by exporting an
existing nested name or by nesting @rhombus(nest) forms.

@(rhombusblock:
   nest Subject:
     export:
       Math
       English
     nest English:
       val greeting: "Hello"

   Subject.English.greeting  // prints "Hello"
   Subject.Math.tau          // prints 6.28

   begin:
     import: .Subject open
     Math.tau                // prints 6.28
             
   )

A @rhombus(.) can be used in n @rhombus(import) form as a shorthand to
reach a nested binding without making intemediate bindings visible.

@(rhombusblock:
   begin:
     import: rhombus.List open
     length(["a", "b", "c"])  // prints 3
  )
