#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "running-cmdline"){Running Rhombus Programs}

A programming environment such as DrRacket (see @secref("editor")) can
run Rhombus programs. To run a module @filepath{prog.rhm} at the command
line, you can use

@nested(~style: #'inset){@exec{rhombus prog.rhm}}

or

@nested(~style: #'inset){@exec{racket prog.rhm}}

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
