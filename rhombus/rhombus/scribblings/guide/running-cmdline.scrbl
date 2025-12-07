#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "racket_fragment.rkt" open)

@title(~tag: "running-cmdline"){Running Rhombus Programs}

A programming environment such as DrRacket (see @secref("editor")) can
run Rhombus programs. To run a module @filepath{prog.rhm} at the command
line, you can use

@nested(~style: #'inset){@exec{rhombus prog.rhm}}

or

@nested(~style: #'inset){@exec{racket prog.rhm}}

You can get a @tech{REPL} with just

@nested(~style: #'inset){@exec{rhombus}}

or

@nested(~style: #'inset){@exec{racket -I rhombus}}

If you installed the Rhombus package after installing Racket, then the
@exec{rhombus} executable may be in a user-specific place, instead of
the same place as the @exec{racket} executable. In case that place is
not clear, DrRacket has a @onscreen{Configure Command Line for
 Racket...} item in the @onscreen{Help} menu, which may help you set up
your path configuration. Alternatively, start @exec{racket} and evaluate
@racket_require_setup_dirs followed by @racket_find_bin_dir to see the
user-specific path for executables that are installed by packages.

Using the @exec{rhombus} executable is like using the @exec{racket}
executable with @exec_flag{-y}, which caches compilation of a module to
a @filepath{compiled} a subdirectory (and the same for any dependency of
the module to run).

Using @exec{racket} or using @exec{rhombus} with the @exec_flag{-u} flag
skip a compilation check, but it still uses a cached compilation if
available. Skipping the compilation check for a module and its
dependencies may significantly reduce start-up time, especially all
modules are already compiled. Meanwhile, a module's compilation does not
have to be cached, so always using just @exec{racket} or always using
@exec_flag{-u} is allowed. Beware, however, of using a cached
compilation when a dependency has changed, because the cached
compilation may not be compatible with the changed module, and
compatibility checks are limited at that level.
