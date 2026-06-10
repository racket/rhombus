#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    pict open
    meta_label:
      rhombus/date
      rhombus/scribble/manual:
        expose:
          docmodule
    "module-hier.rkt".#{module-hierarchy})

@(def pkg_doc = ModulePath'lib("pkg/scribblings/pkg.scrbl")')

@title(~tag: "module-basics"){Module Basics}

Each Rhombus module typically resides in its own file. For example,
suppose the file @filepath{cake.rhm} contains the following module:

@filebox(
  "cake.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    export:
      print_cake

    // draws a cake with `n` candles
    fun print_cake(n :: Int):
      layer("   ", n, Char".", "   ")
      layer(" .-", n, Char"|", "-. ")
      layer(" | ", n, Char" ", " | ")
      layer("---", n, Char"-", "---")

    fun layer(pre, n, ch, post):
      println(pre ++ String.make(n, ch) ++ post)
  )
)

Then, other modules can import @filepath{cake.rhm} to use the
@rhombus(print_cake) function, since the @rhombus(export) declaration in
@filepath{cake.rhm} explicitly exports the definition
@rhombus(print_cake). The @rhombus(layer) function is private to
@filepath{cake.rhm} (i.e., it cannot be used from other modules),
since @rhombus(layer) is not exported.

The following @filepath{random_cake.rhm} module imports
@filepath{cake.rhm}:

@filebox(
  "random_cake.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))

    import:
      "cake.rhm" open

    print_cake(math.random(30))
  )
)

The relative reference @rhombus("cake.rhm") with @rhombus(import) works
if the @filepath{cake.rhm} and @filepath{random_cake.rhm} modules are in
the same directory. Unix-style relative paths are used for relative
module references on all platforms, much like relative URLs in HTML
pages.

@// ----------------------------------------
@section(~tag: "module-org"){Organizing Modules}

The @filepath{cake.rhm} and @filepath{random_cake.rhm} example
demonstrates the most common way to organize a program into modules:
put all module files in a single directory (perhaps with
subdirectories), and then have the modules reference each other
through relative paths. A directory of modules can act as a
project, since it can be moved around on the filesystem or copied to
other machines, and relative paths preserve the connections among
modules.

As another example, if you are building a candy-sorting program, you
might have a main @filepath{sort.rhm} module that uses other modules
to access a candy database and a control sorting machine. If the
candy-database module itself is organized into sub-modules that handle
barcode and manufacturer information, then the database module could
be @filepath{db/lookup.rhm} that uses helper modules
@filepath{db/barcodes.rhm} and @filepath{db/makers.rhm}.  Similarly,
the sorting-machine driver @filepath{machine/control.rhm} might use
helper modules @filepath{machine/sensors.rhm} and
@filepath{machine/actuators.rhm}.

@centered(Pict.from_handle(#{module-hierarchy}))

The @filepath{sort.rhm} module uses the relative paths
@filepath{db/lookup.rhm} and @filepath{machine/control.rhm} to import
from the database and machine-control libraries:

@filebox(
  "sort.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      "db/lookup.rhm"
      "machine/control.rhm"
    #,(more_args)
  )
)

The @filepath{db/lookup.rhm} module similarly uses paths relative to
its own source to access the @filepath{db/barcodes.rhm} and
@filepath{db/makers.rhm} modules:

@filebox(
  "db/lookup.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      "barcodes.rhm"
      "makers.rhm"
    #,(more_args)
  )
)

Ditto for @filepath{machine/control.rhm}:

@filebox(
  "machine/control.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      "sensors.rhm"
      "actuators.rhm"
    #,(more_args)
  )
)

Racket tools all work automatically with relative paths. For example,

@nested(~style: #'inset){@exec{rhombus sort.rhm}}

on the command line runs the @filepath{sort.rhm} program and
automatically compiles and loads imported modules. You can also run with
@exec{racket} instead of @rhombus(rhombus), but @exec{racket} does not
automatically recompile as needed or save compiled modules unless you
supply the @exec_flag{-y} flag.

To compile without running so that a later use of @exec{rhombus} or
@exec{racket} will not need to compile, use
@seclink("make", ~doc: raco_doc){@exec{raco make}}:

@nested(~style: #'inset){@exec{raco make sort.rhm}}

@// ----------------------------------------
@section{Library Collections}

A @deftech{collection} is a hierarchical grouping of installed library
modules.  A module in a @tech{collection} is referenced through an
unquoted, suffixless path. For example, the following module refers to
the @filepath{date.rhm} library that is part of the
@filepath{rhombus} @tech{collection}:

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus))

  import:
    #,(@rhombusmodname(rhombus/date))

  println("Today is " +& date.Date.now())
)

Technically, @rhombus(/, ~impo) is an operator that joins the
@rhombus(rhombus) and @rhombus(date) identifiers to form the module
path @rhombusmodname(rhombus/date).

When you search the Rhombus documentation, search results
indicate the module that provides each binding. Alternatively, if you
reach a binding's documentation by clicking on hyperlinks, you can
hover over the binding name to find out which modules provide it.

A module reference like @rhombusmodname(rhombus/date) looks like a
division expression, but it is not treated as an expression. Instead,
when @rhombus(import) sees a module reference that is unquoted, it
converts the reference to a collection-based module path:

@itemlist(

 @item{First, if the unquoted path contains no @rhombus(/, ~impo), then
 @rhombus(import) automatically adds a @rhombus(/main, ~impo) to the
 end. For example, @rhombus(import slideshow) is equivalent to
 @rhombus(import slideshow/main).}

 @item{Second, @rhombus(import) implicitly adds a @filepath{.rhm} suffix
 to the path while treating it as a @rhombus(lib, ~impo) form, so
 @rhombus(import slideshow/main) is equivalent to
 @rhombus(import lib("slideshow/main.rhm")). }

 @item{Finally, @rhombus(import) resolves a @rhombus(lib, ~impo) module
 path by searching among installed @tech{collections}, instead of
 treating the path as relative to the enclosing module's path.}

)

To a first approximation, a @tech{collection} is implemented as a
filesystem directory. For example, the @filepath{racket} collection is
mostly located in a @filepath{racket} directory within the
installation's @filepath{collects} directory.

The installation's @filepath{collects} directory, however, is only one
place that @rhombus(import) looks for collection directories. Other
places include a subdirectory of the user-specific directory reported by
@rhombus(system.path(#'addon_dir)) and directories configured through
the @envvar{PLTCOLLECTS} search path. Finally, and most typically,
collections are found through installed @tech{packages}.

@// ----------------------------------------
@section(~tag: "packages-and-collections"){Packages and Collections}

A @deftech{package} is a set of libraries that are installed through
the Racket package manager (or included as pre-installed in a Racket
or Rhombus distribution). For example, the @rhombusmodname(gui, ~indirect) library is
provided by the @filepath{rhombus-gui} package, while
@rhombusmodname(draw, ~indirect) is provided by the
@filepath{rhombus-draw} package.@margin_note{More precisely,
 @rhombusmodname(gui, ~indirect) is provided by @filepath{rhombus-gui-lib},
 @rhombusmodname(draw, ~indirect) is provided by
 @filepath{rhombus-draw-lib}, and the @filepath{rhombus-gui} and
 @filepath{rhombus-draw} packages extend @filepath{rhombus-gui-lib} and
 @filepath{rhombus-draw-lib} with documentation.}

Rhombus programs do not refer to @tech{packages} directly. Instead,
programs refer to libraries via @tech{collections}, and adding or
removing a @tech{package} changes the set of collection-based libraries
that are available. A single package can supply libraries in multiple
collections, and two different packages can supply libraries in the
same collection (but not the same libraries, and the package manager
ensures that installed packages do not conflict at that level).

The distinction between packages and modules is unlike some package
managers for specific programming languages, but it is similar to many
operating-system package managers. A package provides a set of library
modules, but there's no requirement that the modules have a name that is
based on the package name. A package can add to multiple
@tech{collections}, and multiple packages can add to the same
collection. For example, the @pkg{rhombus-ssl-lib} package provides a
@rhombusmodname(net/ssl, ~indirect) module, even though @litchar{net} is not in
the package name. The @pkg{rhombus-url-lib} package similarly provides
@rhombusmodname(net/url, ~indirect), also adding to the @filepath{net}
collection. The package manager ensures that full library names do not
conflict. Much like operating-system packages, updates to a package are
generally intended to be backward-compatible, and API changes are
reflected by increasing a number that is part of the package name.

For more information about packages, see @docref(pkg_doc).

@// ----------------------------------------
@section(~tag: "link-collection"){Adding Collections}

Looking back at the candy-sorting example of @secref("module-org"),
suppose that modules in @filepath{db/} and @filepath{machine/} need a
common set of helper functions. Helper functions could be put in a
@filepath{utils/} directory, and modules in @filepath{db/} or
@filepath{machine/} could access utility modules with relative paths
that start @filepath{../utils/}. As long as a set of modules work
together in a single project, it's best to stick with relative paths.
A programmer can follow relative-path references without knowing about
your Rhombus configuration.

Some libraries are meant to be used across multiple projects, so that
keeping the library source in a directory with its uses does not make
sense. In that case, the best option is to add a new
@tech{collection}. After the library is in a collection, it can be
referenced with an unquoted path, just like libraries that are
included with the Rhombus distribution.

You could add a new collection by placing files in the Racket
installation. Alternatively, you could add to the list of searched
directories by setting the @envvar{PLTCOLLECTS} environment
variable.@margin_note{If you set @envvar{PLTCOLLECTS}, include an empty
 path by starting the value with a colon (Unix and Mac OS) or semicolon
 (Windows) so that the original search paths are preserved.} The best
option, however, is to add a @tech{package}.

Creating a package @emph{does not} mean that you have to register with
a package server or perform a bundling step that copies your source
code into an archive format. Creating a package can simply mean using
the package manager to make your libraries locally accessible as a
collection from their current source locations.

For example, suppose you have a directory @filepath{/usr/molly/bakery}
that contains the @filepath{cake.rhm} module (from the
@seclink("module-basics"){beginning} of this section) and other related
modules. To make the modules available as a @filepath{bakery}
collection, either

@itemlist(

 @item{Use the @exec{raco pkg} command-line tool:

       @nested(~style: #'inset){@exec{raco pkg install --link /usr/molly/bakery}}

       where the @exec_flag{--link} flag is not actually needed when
       the provided path includes a directory separator.}

 @item{Use DrRacket's @onscreen{Package Manager} item from the
       @onscreen{File} menu. In the @onscreen{Do What I Mean} panel,
       click @onscreen{Browse...}, choose the
       @filepath{/usr/molly/bakery} directory, and click
       @onscreen{Install}.}

)

Afterward, @rhombus(import: bakery/cake) from any module will import
the @rhombus(print_cake) function from
@filepath{/usr/molly/bakery/cake.rhm}.

By default, the name of the directory that you install is used both as
the @tech{package} name and as the @tech{collection} that is provided
by the package.  Also, the package manager normally defaults to
installation only for the current user, as opposed to all users of a
Racket installation. See @docref(pkg_doc) for more information.

If you intend to distribute your libraries to others, choose
collection and package names carefully. The collection namespace is
hierarchical, but top-level collection names are global, and the
package namespace is flat. Consider putting one-off libraries under
some top-level name like @filepath{molly} that identifies the
producer.  Use a collection name like @filepath{bakery} when producing
the definitive collection of baked-goods libraries.

After your libraries are put in a @tech{collection} you can still use
@exec{raco make} to compile the library sources, but it's better and
more convenient to use @seclink("setup", ~doc: raco_doc){@exec{raco
  setup}}. The @exec{raco setup} command takes a collection name (as
opposed to a file name) and compiles all libraries within the
collection. In addition, @exec{raco setup} can build documentation for
the collection and add it to the documentation index, as specified by an
@filepath{info.rkt} module in the collection. See
@secref("setup", ~doc: raco_doc) for more information on @exec{raco
 setup}. Note that you will have to use Racket syntax for some of those
pieces; collection information is always in @filepath{info.rkt}, and
@filepath{info.rhm} is not recognized.

@// ----------------------------------------
@section(~tag: "intracollection"){Module References Within a Collection}

When a module within a collection references another module within the
same collection, either a relative path or a collection path could
work. For example, a @filepath{sort.rhm} module that references
@filepath{db/lookup.rhm} and @filepath{machine/control.rhm} modules
within the same collection could be written with relative paths as in
@secref("module-org"):

@filebox(
  "sort.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      "db/lookup.rhm"
      "machine/control.rhm"
    #,(more_args)
  )
)

Alternatively, if the collection is named @filepath{candy}, then
@filepath{sort.rhm} could use collection paths to import the two
modules:

@filebox(
  "sort.rhm",
  @rhombusblock(
    #,(@hash_lang()) #,(@rhombuslangname(rhombus))
    import:
      candy/db/lookup
      candy/machine/control
    #,(more_args)
  )
)

For most purposes, these choices will work the same, but there are
exceptions. When writing documentation with
@seclink("top", ~doc: ModulePath'lib("scribblings/scribble/scribble.scrbl")', ~indirect: #true){Scribble},
you must use a collection path with @rhombus(docmodule) and similar
forms; that's partly because documentation is meant to be read by
client programmers, and so the collection-based name should appear.
Meanwhile, for @rhombus(import), using relative paths for references
within a collection tends to be the most flexible approach, but with
caveats.

Relative-path references work much like relative URL references: the
reference is expanded based on the way the enclosing module is
accessed. If the enclosing module is accessed through a filesystem
path, then a relative path in @rhombus(import) is combined with that
filesystem path to form a new filesystem path. If the enclosing module
is accessed through a collection path, then a relative path in
@rhombus(import) is combined with that collection path to form a new
collection path. A collection path is, in turn, converted to a
filesystem path, and so the difference between starting with a
filesystem or collection path does not usually matter. Unfortunately,
inherent complexities of path resolution can create differences in
some situations:

@itemlist(

 @item{Through soft links, multiple mount points, or case-insensitive
       filesystems (on an operating system that does not implicitly
       case-normalize paths), there may be multiple filesystem paths
       that refer to the same module file.

       For example, when the current directory is the @filepath{candy}
       collection's directory, the current-directory path that
       @exec{rhombus} receives on startup may cause @exec{rhombus
        sort.rhm} to use a different filesystem path than @exec{racket
        -l candy/sort} finds through the library-collection search
       path. In that case, if @filepath{sort.rhm} leads to some
       modules through both relative-path references and
       collection-based references, it's possible that those resolve
       to different instances of the same source module, creating
       confusion through multiple instantiations.}

 @item{When @seclink("exe", ~doc: raco_doc, ~indirect: #true){@exec{raco exe}}
       plus @seclink("exe-dist", ~doc: raco_doc, ~indirect: #true){@exec{raco distribute}}
       are used to create an executable to run on a different machine,
       the paths of the current machine are likely unrelated to paths
       on the target machine. The @exec{raco exe} tool treats modules
       that are referenced via filesystem paths differently than
       modules referenced via collection paths, because only the
       latter make sense to access through reflective operations at
       run time.

       For example, if @exec{raco exe sort.rhm} creates an executable
       that uses @rhombus(dynamic_import(ModulePath'candy/db/lookup', #'all)) at run
       time, then that @rhombus(dynamic_import) will fail in the case
       that @filepath{db/lookup.rhm} is resolved relative to the
       filesystem path @filepath{sort.rhm} at executable-creation
       time.}

)

Using only collection-based paths (including using shell commands like
@exec{racket -l candy/sort} and not like @exec{rhombus sort.rhm}) can
avoid all problems, but then you must only develop modules within an
installed collection, which is often inconvenient. Using relative-path
references consistently tends to be the most convenient while still
working in most circumstances.
