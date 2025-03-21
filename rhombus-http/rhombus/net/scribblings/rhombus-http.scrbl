#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~style: #'toc){Rhombus HTTP Client}

@docmodule(net/http)

The @rhombusmodname(net/http) library provides functions for sending
HTTP requests as a client.

@table_of_contents()

@include_section("http/get.scrbl")
@include_section("http/session.scrbl")
@include_section("http/response.scrbl")
@include_section("http/auth.scrbl")
@include_section("http/payload.scrbl")
@include_section("http/timeout.scrbl")
@include_section("http/pool.scrbl")
@include_section("http/proxy.scrbl")
@include_section("http/literal_url.scrbl")
@include_section("http/exn.scrbl")
