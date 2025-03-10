#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    meta_label:
      rhombus/cmdline
      rhombus/cmdline open
      rhombus/cmdline!class open)

@title(~tag: "cmdline"){Command Line Parsing}

The @rhombusmodname(rhombus/cmdline) library provides a
@rhombus(cmdline.parse) form to contain @rhombus(cmdline.flag) and
@rhombus(cmdline.args) forms that together describe parsing for
command-line arguments. A parse result is represented as a map
that (by default) uses the flag names as symbol keys and
@rhombus(#'args) as a key to hold arguments that appear after all flags.

Here's an example for a program that supports @exec_flag{--channel},
@exec_flag{--volume}, @exec_flag{++louder}, and @exec_flag{--quieter} flags:

@rhombusblock(
  import:
    rhombus/cmdline open

  def config:
    parse:
      flag "--channel" name
      flag "--volume" (n :: String.to_int):
        ~init: { #'volume: -20 }
        ~alias: "-v"
      multi:
        flag "++louder":
          state[#'volume] := state[#'volume] + 1
        flag "--quieter":
          state[#'volume] := state[#'volume] - 1

  println(@str{Listen to @(config.get(#'channel, "unknown"))
               at volume @(config[#'volume]).})
)

In the example, the @exec_flag{--channel} flag gets a default implementation
that adds an argument string to the result map. The @exec_flag{--volume} flag
also gets default handling, but adds an initial value for
@rhombus(#'volume) in the map to give it a default value, and supports
the shorthand @exec_flag{-v} in place of @exec_flag{--volume}; also, its argument
is annotated with @rhombus(String.to_int, ~annot) to allow only integer
arguments to be passed on the command line (i.e., strings that can be
converted to integers). Each of @exec_flag{--channel} and @exec_flag{--volume} is
allowed at most once, but @exec_flag{++louder} and @exec_flag{--quieter} are each
allowed any number of times, and they have custom handling to adjust
@rhombus(#'volume) instead of adding @rhombus(#'louder) and
@rhombus(#'quieter) keys to the map. In this example,
@rhombus(cmdline.args) is not used alongside @rhombus(cmdline.flag) and
@rhombus(cmdline.multi), which means that the program does not accept
additional arguments after flags.

Since the example above uses @rhombus(cmdline.parse), then running the
program will always parse command-line arguments. The
@rhombus(cmdline.parser) (with an ``r'' at the end) form has the same
syntax, but produces a @rhombus(Parser) object whose
@rhombus(Parser.parse) method can be called to trigger parsing and get a
result map.

@examples(
  ~hidden:
    import:
      rhombus/cmdline open
  ~defn:
    def config_getter:
      parser:
        flag "--channel" name
        flag "--volume" (n :: String.to_int):
          ~init: { #'volume: -20 }
          ~alias: "-v"
        multi:
          flag "++louder":
            state[#'volume] := state[#'volume] + 1
          flag "--quieter":
            state[#'volume] := state[#'volume] - 1
  ~repl:
    config_getter.parse(~line: ["--volume", "-17",
                                "++louder",
                                "++louder",
                                "--channel", "The 90s"])
    ~error:
      config_getter.parse(~line: ["--volume", "oops"],
                          ~program: "demo")
    config_getter.print_help(~program: "demo")
)
