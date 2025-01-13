#lang rhombus/scribble/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm" open
    meta_label:
      rhombus/cmdline
      rhombus/cmdline!class open)

@title(~tag: "cmdline-help"){Command Line Parser Parameters and Annotations}

@(~version_at_least "8.14.0.4")

@doc(
  Parameter.def cmdline.current_program
    :: String || Path
  Parameter.def cmdline.current_command_line
    :: List.of(String)
  Parameter.def cmdline.current_flag_string
    :: maybe(String)
  Parameter.def cmdline.current_containing_flag_string
    :: maybe(String)
){

 Parameters used for command-line parsing.

 The @rhombus(cmdline.current_program) and
 @rhombus(cmdline.current_command_line) parameters provide defaults to
 start parsing, but they are also set by @rhombus(Parser.parse) to match
 supplied arguments in case they are different than the defaults.

 The @rhombus(cmdline.current_flag_string) and
 @rhombus(cmdline.current_containing_flag_string) parameters are set when
 a flag handler is called to report the flag as written by the user. When
 the value of @rhombus(cmdline.current_flag_string) is a short-form flag
 like @rhombus("-h"), then
 @rhombus(cmdline.current_containing_flag_string) may be set to a
 combination flag like @rhombus("-vh") to report more precisely how it
 was written by the user.

}

@doc(
  annot.macro '(cmdline.String.to_lib_module_path)'
){

 A @tech(~doc: guide_doc){converter annotation} that recognizes strings
 that are suitable for a @rhombus(lib, ~modpath) module-path form. Unlike
 a string used directly with @rhombus(lib, ~modpath), the string is
 allowed to contain @litchar{!} separating non-empty submodule names, and
 the result @rhombus(ModulePath, ~annot) rotates the submodule
 selections to outside the @rhombus(lib, ~modpath) part.

 This annotation is particularly intended for use with
 @rhombus(cmdline.flag) and @rhombus(cmdline.args).

@examples(
  ~hidden:
    import rhombus/cmdline
  ~repl:
    def mp :: cmdline.String.to_lib_module_path = "rhombus/pict!private"
    mp
)

}
