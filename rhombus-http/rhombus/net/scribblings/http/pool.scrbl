#lang rhombus/scribble/manual
@(import:
    meta_label:
      rhombus open
      net/http open
      net/url)

@title(~tag: "pool"){Connection Pool Configuration}


@doc( 
  class PoolConfig():
    constructor (
      ~max_size: max_size :: PosInt || matching(#inf) = 128,
      ~idle_timeout_seconds: idle_timeout :: maybe(PosReal) = 600
    )
){

 Represents a connection pool configuration for use in creating
 a @rhombus(Session).

}
