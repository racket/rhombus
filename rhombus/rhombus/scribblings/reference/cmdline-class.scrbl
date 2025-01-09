#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/cmdline
      rhombus/cmdline!class open)

@title(~tag: "cmdline-class"){Command Line Parsing Classes}

@docmodule(rhombus/cmdline!class)

@doc(
  class Parser():
    constructor (
      ~flags: flags :: List.of(Content),
      ~add_builtin: add_builtin :: Any.to_boolean = #true,
      ~args: args :: Handler,
      ~init: init :: Map = {},
      ~who: who :: maybe(error.Who) = #false
    )
  method (p :: Parser).parse(
    ~program: program :: String || Path = cmdline.current_program(),
    ~line: line :: List.of(String) = cmdline.current_command_line()
  ) :: Map
  method (p :: Parser).print_help(
    ~program: program :: String || Path = cmdline.current_program()
  ) :: Void
  property (p :: Parser).flags :: List.of(Content)
  property (p :: Parser).args :: Handler
  property (p :: Parser).init :: Map
){

 Represents a parser as normally created by @rhombus(cmdline.parser).

}

@doc(
  class Handler(init :: Map,
                args :: List.of(String),
                repeat_last :: Any.to_boolean,
                handler :: (~any) -> Map):
    nonfinal
){

 Represents a handler, either for a flag or for arguments after all
 flags. A handler that represents a flag is more specifically an instance
 of @rhombus(Flag).

 The constructor for @rhombus(Handler) accepts the same arguments as the
 default constructor, but checks that @rhombus(args) contains valid
 flags and that @rhombus(handler) accepts the number of arguments implied
 by @rhombus(args).

}

@doc(
  class Flag(flag_strs :: List.of(String),
             help :: maybe(String)):
    nonfinal
    extends Handler
  class FinalFlag():
    extends Flag
  class Multi(choices :: List.of(Flag || Multi))
  class OnceEach(choices :: List.of(Flag || OnceEach))
  class OnceAny(choices :: List.of(Flag || OnceAny))
  class Text():
    nonfinal
  class EndText():
    extends Text
  enum Content:
    ~is_a Flag
    ~is_a Multi
    ~is_a OnceEach
    ~is_a OnceAny
    ~is_a Text
){

 Flags, flag sets, and help text that can be part of a command-line
 parser constructed with @rhombus(cmdline.parser) or @rhombus(Parser).

 The constructor for @rhombus(Flag) accepts the same arguments as the
 default constructor, but checks that @rhombus(flag_strs) constaints
 valid flags.

}


@doc(
  fun make_builtin_flags(help :: Map -> Map) :~ List.of(Content)
){

 Returns a list of @rhombus(Content) corresponding to flags that are
 normally added to a command-line parser by default, unless
 @rhombus(~no_builtin) is used with @rhombus(cmdline.parser) or
 @rhombus(~add_builtin) is supplied as @rhombus(#false) to
 @rhombus(Parser).

}
