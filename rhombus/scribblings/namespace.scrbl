#lang scribble/rhombus/manual
@(import:
    "util.rhm" open
    "common.rhm" open)

@title(~tag: "namespaces-overview"){Namespaces}

A dotted module import as @rhombus(convert.fahrenheit_to_celsius) or class
field accessors as @rhombus(Posn.x) and @rhombus(Posn.y) demonstrate
the use of hierarchical names. Other hierarchical names provided by
@rhombusmodname(rhombus) include @rhombus(List.length) and
@rhombus(List.cons) via @rhombus(List) (where lists will discussed more
in @secref("list")):

@(rhombusblock:
    List.length(["a", "b", "c"])  // prints 3
  )

Use the @rhombus(namespace) form to create a namespace without creating a
sepaarte module. The identifier after @rhombus(namespace) is bound as a
namespace, and @rhombus(export) provide forms within the
@rhombus(namespace) body determine the bindings that can be accessed from the
name with @rhombus(.).

@(rhombusblock:
   namespace math:
     export:
       tau
       Complex
     val pi: 3.14
     val tau: 2 * pi
     class Complex(real, imag)

   math.tau                   // prints 6.28
   // math.pi                 // would be an error
   math.Complex(0, math.tau)  // prints Complex(0, 6.28)
)

A name defined with @rhombus(namespace) can be used with @rhombus(import),
but the name must be prefixed with @rhombus(.) to distinguish it from a
module path. Also, @rhombus(import) can be used in nested blocks
generally, such as a block created with @rhombus(begin) or
@rhombus(val):

@(rhombusblock:
   begin:
     import: .math open
     Complex(0, tau)  // prints Complex(0, 6.28)

   val also_pi:
     import: .math open
     tau / 2
   also_pi            // prints 3.14
  )

Naturally, namespaces can be nested further, either by exporting an
existing namespace or by nesting @rhombus(namespace) forms.

@(rhombusblock:
   namespace subject:
     export:
       math
       english
     namespace english:
       val greeting: "Hello"

   subject.english.greeting  // prints "Hello"
   subject.math.tau          // prints 6.28

   begin:
     import: .subject open
     math.tau                // prints 6.28
             
   )

A @rhombus(.) can be used in n @rhombus(import) form as a shorthand to
reach a nested binding without making intemediate bindings visible.

@(rhombusblock:
   begin:
     import: rhombus.List open
     length(["a", "b", "c"])  // prints 3
  )
