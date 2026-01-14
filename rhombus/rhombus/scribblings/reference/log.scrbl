#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/bnf
    meta_label:
      rhombus/thread.Evt)

@(def log_eval = make_rhombus_eval())

@examples(
  ~eval:
    log_eval
  ~hidden:
    import rhombus/thread open
    Logger.current(Logger(~parent: #false))
    def def_r = LogReceiver(~logger: log)
)

@title{Logging}

A @deftech{logger} created with @rhombus(Logger) accepts events that
contain information to be logged for interested parties. A @deftech{log
 receiver} created with @rhombus(LogReceiver) represents an interested
party that receives logged events asynchronously. Each event has a
@defterm{topic} and @defterm{level} of detail, and a log receiver
subscribes to logging events at a certain level of detail (and lower)
for a specific topic or for all topics. The levels (see
@rhombus(Logger.Level, ~annot)) in increasing order of detail are
@rhombus(#'none), @rhombus(#'fatal), @rhombus(#'error),
@rhombus(#'warning), @rhombus(#'info), and @rhombus(#'debug). The
@rhombus(#'none) level is intended for specifying receivers, and
messages logged at that level are never sent to receivers.

To help organize logged events, a logger can have a default topic and/or
a @deftech{parent logger}. Every event reported to a logger is normally
propagated to its parent (if any), and the event message is prefixed
with the logger’s topic (if any) if the message doesn't already have a
topic. Events that are propagated from a logger to its parent can be
filtered by level and topic.

@examples(
  ~eval: log_eval,
  ~version_and_later "9.1.0.2":
    def l = Logger(~topic: #'example, ~parent: #false)
    def r = LogReceiver(~logger: l, ~receive: #'warning)
    l.error("something is wrong")
    r.sync()
    :
      l.debug("detailed information") // no receiver interested in debug
    r.sync(~timeout: 0)
)

On start-up, Rhombus creates an initial logger that is used to record
events from the core run-time system. For example, a @rhombus(#'debug)
event is reported for each garbage collection. For this initial
logger, three log receivers are also created: one that writes events
to the process’s original error output port, one that writes events to
the process’s original output port, and one that writes events to the
system log. The level of written events in each case is
system-specific, and the default can be changed through environment
variables:

@itemlist(

 @item{If the @as_indexed{@tt{PLTSTDERR}} environment variable is
 defined, it determines the level of the log receiver that propagates
 events to the original error port.

 The environment variable’s value can be a @bnf.nt{level}:
 @litchar{none}, @litchar{fatal}, @litchar{error}, @litchar{warning},
 @litchar{info}, or @litchar{debug} (from low detail to high detail); all
 events at the corresponding level of detail or lower are printed. In
 addition to a @bnf.nt{level}, the value can contain whitespace-separated
 specifications of the form @bnf.nt{level}@litchar("@")@bnf.nt{topic},
 which prints events whose topics match ‹topic› only at the given
 @bnf.nt{level} or higher (where a @bnf.nt{topic} contains any character
 other than whitespace or @litchar("@"). Leading and trailing whitespace
 is ignored. For example, the value @litchar|{error debug@GC}| prints all
 events at the @rhombus(#'error) level and higher, but prints events for
 the topic @rhombus(#'GC) at the @rhombus(#'debug) level and higher
 (which includes all levels).

 The default is @litchar{error}.}

 @item{If the @as_indexed{@tt{PLTSTDOUT}} environment variable is
 defined, it determines the level of the log receiver that propagates
 events to the original output port. The possible values are the same as
 for @tt{PLTSTDERR}.

 The default is @litchar{none}.}

 @item{If the @as_indexed{@tt{PLTSYSLOG}} environment variable is
 defined and is not overridden by a command-line flag, it determines the
 level of the log receiver that propagates events to the system log. The
 possible values are the same as for @tt{PLTSTDERR}.

 The default is @litchar{none} for Unix or @litchar{error} for Windows
 and Mac OS.}

)

The @rhombus(Logger.current) parameter determines the @deftech{current
 logger}, and @rhombus(log) is a shorthand for
@rhombus(Logger.current()). On start-up, the initial value of the
parameter is the initial logger. The run-time system sometimes uses the
current logger to report events. For example, the compiler sometimes
reports @rhombus(#'warning) events when it detects an expression that
would produce a run-time error if evaluated.

@examples(
  ~eval: log_eval,
  ~version_and_later "9.1.0.2":
    ~fake:
      block:
        log.error("something is wrong") // probably goes to stderr
        #'done
      block:
        log.error("something is wrong")
        def_r.wrap(fun (a :: Array): println(a[0], ~out: stderr)).sync()
        #'done
)

@doc(
  class Logger():
    constructor (
      ~topic: topic :: maybe(Symbol) = #false,
      ~parent: parent :: maybe(Logger) = Logger.current(),
      ~propagate: propagate
                   :: (Logger.Level || Map.of(maybe(Symbol), Logger.Level))
                   = #'debug
    )
){

 Creates a @tech{logger} with @rhombus(parent) as an optional
 @tech{parent logger}.

 The @rhombus(propagate) argument controls which events reported to the
 logger are propagated to @rhombus(parent). If @rhombus(propagate) is an
 immediate @rhombus(Logger.Level, ~annot), then it is a shorthand for
 @rhombus({#false: propagate}). As a map, @rhombus(propagate) maps a
 topic to a maximum level of detail that is propagated for that topic,
 and @rhombus(#false) is mapped to default maximum level of detail that
 applies to any topic not explicitly mapped.

}

@doc(
  Parameter.def Logger.current :: Logger
  expr.macro 'log'
){

 The @rhombus(Logger.current) @tech{context parameter} determines the
 @tech{current logger} that is used by default.

 The @rhombus(log) expression form is a shorthand for
 @rhombus(Logger.current()). For example, @rhombus(log.error("oops"))
 logs the message @rhombus("oops") to the current logger.

 Each time the @rhombus(log) expression form is evaluated or
 @rhombus(Logger.current()) is used to obtain the current logger, the
 resulting object can be different accoring to @rhombus(===), but the
 underlying Racket-level logger (accessible via @rhombus(Logger.handle))
 remains the same if the logger is not changed; two loggers are
 @rhombus(==) when the underlying Racket logger is the same. Adjusting
 the @rhombus(Logger.current) context parameter also adjusts the current
 logger at the Racket level.

}


@doc(
  ~nonterminal_key:
    Logger
  ~nonterminal:
     str_expr: block expr

  dot (lg :: Logger).debug(message)
  dot (lg :: Logger).info($message)
  dot (lg :: Logger).warning($message)
  dot (lg :: Logger).error($message)
  dot (lg :: Logger).fatal($message)

  grammar message
  | str_expr
  | str_expr #,(@tt{,}) #,(@rhombus(~data: expr))
  | #,(@rhombus(~data: expr)) #,(@tt{,}) str_expr
){

 Registers an event to forward to parents and receivers of @rhombus(lg).
 Although these forms look like function calls, they are not calls,
 because @rhombus(str_expr) and @rhombus(expr) and evaluated only if
 @rhombus(lg) has a receiver at the corresponding level of detail and for
 the logger's default topic. (See also @rhombus(Logger.max_at_level).)

 Each event has a string message and an additional data value, where the
 additional data defaults to @rhombus(#false).

}

@doc(
 method (lg :: Logger).message(
   ~level: level :: Logger.Level,
   ~topic: topic :: maybe(Symbol) = lg.topic,
   msg :: String,
   ~data: data = #false,
   ~add_prefix: add_prefix :: Any.to_boolean = #true
 ) :: Void
){

 Registers an event to forward to parents and receivers of @rhombus(lg).

 If @rhombus(add_prefix) is true and @rhombus(topic) is a symbol, then
 @rhombus(topic +& ": " +& msg) is logged as the event's message string,
 instead of just @rhombus(msg).

}

@doc(
  dot (lg :: Logger).show($expr, ...)
){

 Similar to @rhombus(showln(expr, ...)), but ``prints'' to the current
 logger in the same way as @rhombus(Logger.error).

 Use @rhombus(log.show) as an alternative to @rhombus(showln) for
 temporary debugging output. The intent of logging the same as
 @rhombus(Logger.error) is to print at a low enough level of detail that
 a logger is likely to write synchronously to the original error output
 port.

}

@doc(
 method (lg :: Logger).is_at(
   ~level: level :: Logger.Level,
   ~topic: topic :: maybe(Symbol) = #false
 ) :: Boolean

 method (lg :: Logger).max_at_level(
   ~topic: topic :: maybe(Symbol) = #false
 ) :~ maybe(Logger.Level)

 method (lg :: Logger).all_at_levels()
   :~ Map.of(maybe(Symbol), Logger.Level)

 method (lg :: Logger).level_change_evt() :: Evt
){

 The @rhombus(Logger.is_at) method reports whether @rhombus(lg) or its
 parent have any listeners for at least the @rhombus(level) level of
 detail---specifically for @rhombus(topic) if it is a symbol, or for any
 topic if @rhombus(topic) is @rhombus(#false). Form like
 @rhombus(Logger.error) use @rhombus(Logger.is_at) to skip evaluation of
 message and data expressions if no receiver is interested.

 The @rhombus(Logger.max_at_level) method reports the most specific
 @rhombus(Logger.Level) value for which @rhombus(lg.is_at) will
 return @rhombus(#true).

 The @rhombus(Logger.all_at_levels) method summarizes the possible
 results of @rhombus(Logger.is_at) across all topics. Similar to the
 @rhombus(~propagate) argument to the @rhombus(Logger) constructor, a
 @rhombus(#false) key in the result map indicates a level for all topics
 no otherwise mapped.

 The @rhombus(Logger.level_change_evt) method returns a
 @tech{synchronizable event} that becomes ready at a point where the
 result of @rhombus(Logger.all_at_levels) could be different than its
 current result.

}

@doc(
  class LogReceiver():
    implements Evt
    constructor (
      ~logger: logger :: Logger = Logger.current(),
      ~receive: receive
                  :: Logger.Level || Map.of(maybe(Symbol), Logger.Level)
                  = #'debug
    )
){

 Creates @tech{log receiver} receives events from @rhombus(logger).

 The @rhombus(receive) argument is analogous to the @rhombus(~propagate)
 argument to the @rhombus(Logger) constructor, and it determines the
 events that @rhombus(logger) will send to the new receiver. A log
 receiver's events are queued internally.

 An instance of @rhombus(LogReceiver) is a @tech{synchronizable event},
 and it's synchronization result is an array representation of an event.
 For example, use the @rhombus(Evt.sync) to dequeue an event from the
 receiver when one is ready. The synchronization result is an immutable
 array of four values:

@itemlist(

 @item{a @rhombus(Logger.Level, ~annot) symbol for the level of the
  event;}

 @item{a @rhombus(String, ~annot) for the event message;}

 @item{an arbitrary value for the event's data; and}

 @item{a @rhombus(Symbol, ~annot) or @rhombus(#false) for the event's topic.}

)

}

@doc(
  enum Logger.Level
  | fatal
  | error
  | warning
  | info
  | debug
  | none
){

 Levels of detail for logged messages, where @rhombus(#'fatal) is the
 lowest level of detail, and @rhombus(#'none) is the (unattainable)
 highest level of detail.

 For example, if a @tech{log receiver} is interested in events at the
 @rhombus(#'warning) level, it will also receive events at the
 @rhombus(#'error) and @rhombus(#'fatal) levels.

}

@doc(
  property (lg :: Logger).handle
  property (lg :: LogReceiver).handle
  fun Logger.from_handle(handle :: Any) :: Logger
  fun LogReceiver.from_handle(handle :: Any) :: Logger
){

 Converts to and from a Racket-level logger or log receiver.

}

@(close_eval(log_eval))
