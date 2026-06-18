#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "module-set"){Assignment and Redefinition}

The use of @rhombus(:=) on variables defined within a module is
limited to the body of the defining module. That is, a module is
allowed to change the value of its own definitions, and such changes
are visible to importing modules. However, an importing context is
not allowed to change the value of an imported binding.

@rhombusblock(
  module m ~lang #,(@rhombuslangname(rhombus)):
    export:
      counter
      increment

    def mutable counter = 0

    fun increment():
      counter := counter + 1

  import self!m open

  counter       // prints 0
  increment()
  counter       // prints 1
  counter := -1 // not allowed
)

As the above example illustrates, a module can always grant others
the ability to change its exports by providing a mutator function,
such as @rhombus(increment).

The prohibition on assignment of imported variables helps support
modular reasoning about programs. For example, in the module,

@rhombusblock(
  module m ~lang #,(@rhombuslangname(rhombus)):
    export:
      rx_fish
      fishy_string

    def rx_fish = #'rx"fish"

    fun fishy_string(s):
      RX.match(rx_fish, s) && #true
)

the function @rhombus(fishy_string) will always match strings that
contain ``fish'', no matter how other modules use the
@rhombus(rx_fish) binding. For essentially the same reason that it
helps programmers, the prohibition on assignment to imports also
allows many programs to be executed more efficiently.

Along the same lines, when a module's binding is not declared
@rhombus(mutable, ~bind), then the binding is considered a
@defterm{constant} that cannot be changed---not even by re-declaring
the module.

Consequently, re-declaration of a module is not generally allowed.
For file-based modules, simply changing the file does not lead to a
re-declaration in any case, because file-based modules are loaded on
demand, and the previously loaded declarations satisfy future
requests. It is possible to re-declare a module reflectively,
however, and non-file modules can be re-declared in the @tech{REPL};
in such cases, the re-declaration may fail if it involves the
re-definition of a previously constant binding.

@(
  rhombusblock_etc():
    module m ~lang #,(@rhombuslangname(rhombus)):
      def pie = 3.141597

    import self!m open

    module m ~lang #,(@rhombuslangname(rhombus)):
      def pie = 3
    // re-declaration error
)