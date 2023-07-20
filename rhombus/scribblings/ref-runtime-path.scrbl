#lang scribble/rhombus/manual
@(import:
    "common.rhm" open:
      except: def
    "nonterminal.rhm".body
    meta_label:
      rhombus/runtime_path
      rhombus/runtime_path open)

@title(~style: #'toc){Runtime Paths}

@docmodule(rhombus/runtime_path)

@doc(
  defn.macro 'def $id:
                $body
                ...'
){

 Defines @rhombus(id) to provide an absolute path that refers to the
 file name produced by the @rhombus(body) sequence as interpreted
 relative to the source module.

 An unutual property of @rhombus(runtime_path.def) is that the
 @rhombus(body) sequence is used in both a run-time context and a
 @rhombus(meta) context, so it must be valid for both. The meta
 interpretation is used for tasks like creating a standalone executable
 to ensure that referenced files are accessible at run time.

@examples(
  import:
    rhombus/meta open
    rhombus/runtime_path
  runtime_path.def image_file: "file.png"
)

}
