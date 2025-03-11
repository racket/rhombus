#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "standalone"){Standalone Rhombus Programs}

Racket tools generally work on Rhombus programs, and that includes tools
for creating and distributing standalone executables.

To generate a standalone executable @exec{prog} from @exec{prog.rhm}, use
@seclink("exe", ~doc: raco_doc, ~indirect: #true, @exec{raco exe}):

@nested(~style: #'inset){@exec{raco exe prog.rhm}}

That's only the first step to creating an executable that can run on
other machines, however. The resulting @exec{prog} works on the machine
used to build it, but it still relies on a Racket and Rhombus
installation. To create a distribution that can be installed on another
machine, use
@seclink("test", ~doc: raco_doc, ~indirect: #true, @exec{raco dist}):

@nested(~style: #'inset){@exec{raco dist prog_dist prog}}

which assembles all needed files in @filepath{prog_dist}.

By default, a standalone executable and distribution simply packages up
all of the support needed to run a program the same as when running
@exec{rhombus} or @exec{racket} directly. If a program does not use
dynamic compilation facilities such as @rhombus(eval), then
demodularizing and removing unneeded code can produce a smaller
implementation that starts up more quickly. The
@seclink("test", ~doc: raco_doc, ~indirect: #true, @exec{raco demod})
tool performs that kind of pruning:

@nested(~style: #'inset){@exec{raco demod -g --work /tmp/work -o prog_small.zo prog.rhm}}

The output @filepath{prog_small.zo} can then be converted to an
executable and/or distribution.

For cross compilation, see
@seclink("top",
         ~indirect: #true,
         ~doc: ModulePath'lib("raco/private/cross/raco-cross.scrbl")'){@exec{raco cross}}.
