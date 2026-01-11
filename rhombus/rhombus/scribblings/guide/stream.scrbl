#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def stream_eval = make_rhombus_eval())

@title(~tag: "stream"){Streams}

A @tech(~doc: ref_doc){stream} is like a sequence, but it is stateless
and supports functional ``first'' and ``rest'' operations like a linked
list. Lists and sequence ranges can be used as streams. Any sequence can
be turned into a stream using @rhombus(Sequence.to_stream), which
instantiates the sequence an caches results so that the same first value
can be provided from a stream as many times as it is requested. An
element can be accessed from a stream using @brackets, which is
equivalent to accessing the @rhombus(Stream.rest) property as many times
as the index in @brackets, and then using @rhombus(Stream.first) to get
the element.

The @rhombus(Stream.cons) expression form is similar to
@rhombus(List.cons), but it constructs a lazy stream: the expressions
for the first and rest of the stream are evaluated on demand. A classic
example for lazy streams is creating an infinite stream of numbers.
Although ranges can also represent streams of numbers, they cannot
represent, say, a stream of prime numbers.

@examples(
  ~eval: stream_eval
  ~defn:
    fun naturals_from(i :: Int) :: Stream:
      Stream.cons(i, naturals_from(i+1))
  ~defn:
    :
      def naturals = naturals_from(0) // equivalent to `(0 ..)`
  ~repl:
    naturals.first
    naturals.rest.first
    naturals[0]
    naturals[11]
  ~defn:
    fun remove_multiples(s :: Stream, n :: Int) :: Stream:
      match s
      | Stream.cons(fst, rst):
          if fst mod n == 0
          | remove_multiples(rst, n)
          | Stream.cons(fst, remove_multiples(rst, n))
  ~defn:
    def odds = remove_multiples(naturals, 2)
  ~repl:
    odds[0]
    odds[11]
  ~defn:
    fun primes_from(s :: Stream) :: Stream:
      match s
      | Stream.cons(fst, rst):
          Stream.cons(fst, primes_from(remove_multiples(rst, fst)))
  ~defn:
    def primes = primes_from(naturals.rest.rest)
  ~repl:
    primes[0]
    primes[11]
)

@(close_eval(stream_eval))
