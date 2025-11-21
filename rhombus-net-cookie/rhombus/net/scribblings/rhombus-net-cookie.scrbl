#lang rhombus/scribble/manual
@(import:
    "net-cookie/common.rhm" open
    meta_label:
      rhombus open)

@title(~style: #'toc){Rhombus Cookies: HTTP State Manegement}

The @rhombusmodname(net/cookie), @rhombusmodname(net/cookie/server), and
@rhombusmodname(net/cookie/user_agent) libraries provide functions for
managing cookies as specified in @RFC6265.

The implementation and documentation here are based on
@seclink("top",
         ~doc: ModulePath'lib("net/cookies/scribblings/cookies.scrbl")',
         ~indirect: #true){the Racket library by Jordan Johnson}.

@table_of_contents()

@include_section("net-cookie/cookie.scrbl")
@include_section("net-cookie/server.scrbl")
@include_section("net-cookie/user_agent.scrbl")
