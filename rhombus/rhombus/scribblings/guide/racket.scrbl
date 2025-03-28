#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      lib("racket/base.rkt") open:
        only:
          #{path-string?}
          #{open-output-file})

@title(~tag: "racket"){Using Racket Tools and Libraries}

The DrRacket programming environment supports Rhombus, and (as noted in
@secref("running")) Racket tools more generally work on Rhombus
programs. Racket tools work because Rhombus is built on Racket's
@hash_lang() mechanism.

The @exec{rhombus} executable is a fairly thin wrapper around
@exec{racket}. The @exec{rhombus} command-line accepts module paths
using Rhombus syntax instead of Racket syntax, and it starts an
interactive Rhombus session if no starting module is provided. Also,
@exec{rhombus} checks and caches compiled versions of modules by
default, whereas @exec{racket} checks and caches only when the
@exec_flag{-y} flag is used.

Racket provides many libraries and packages that (at the time of
writing) have not yet been ported or wrapped for convenient use in
Rhombus. A Racket module can be @rhombus(import)ed into a Rhombus
module, and because Racket and Rhombus share many of the same data
representations---including functions---the boundary can be relatively
seamless, in some cases.

Here are some key techniques for using Racket libraries in Rhombus:

@itemlist(

 @item{To @rhombus(import) a collection-based Racket module, use
 @rhombus(lib, ~impo) and make the @filepath{.rkt} file extension or
 @filepath{main.rkt} file name explicit. For example,
 @rhombus(lib("racket/base.rkt"), ~impo) imports from
 @rhombuslangname(racket/base), while
 @rhombus(lib("racket/main.rkt"), ~impo) imports from
 @rhombuslangname(racket).}

 @item{Use @s_exp_braces or @s_exp_kw_braces to refer to Racket identifiers or keywords that do not fit
 the syntax of Rhombus identifiers or keywords. For example,
 @rhombus(#{path-string?}) is the Racket predicate that corresponds to
 the Rhombus @rhombus(PathString, ~annot) annotation. Similarly,
 @rhombus(~#{replace-permissions?}) is a keyword argument for the Racket
 function @rhombus(#{open-output-file}).}

 @item{Racket lists are @rhombus(PairList, ~annot)s, not
 @rhombus(List, ~annot)s. Racket strings are mutable by default, so use
 @rhombus(to_string) to convert a Racket string result to a
 Rhombus-friendly string.}

)

Racket syntactic forms cannot be used from Rhombus, but a Rhombus macro
can expand to a use of a Racket syntactic form via
@rhombus(expr_meta.pack_s_exp) and similar.

For more information, see
@docref(ModulePath'lib("rhombus/scribblings/rhombus-racket/rhombus-racket.scrbl")',
        ~indirect: @elem{Rhombus and Racket Interoperability}).
