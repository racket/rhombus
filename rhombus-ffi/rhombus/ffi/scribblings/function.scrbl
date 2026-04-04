#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def ffi_eval = make_rhombus_eval())
@examples(
  ~eval: ffi_eval
  ~hidden:
    import ffi open
)

@title(~tag: "function"){Foreign Functions}

@doc(
  ~nonterminal:
    arg_id: block id
    result_id: block id
    errno_id: block id
    arg_type: * type ~at rhombus/ffi/type
    result_type: * type ~at rhombus/ffi/type
    auto_expr: block expr
    body: block body
  type.macro '$args_tuple -> $result_tuple'
  type.macro '$args_tuple -> $result_tuple:
                $arrow_option
                ...
                $body
                ...'
  grammar args_tuple
  | $arg_type
  | ($arg, ...)
  | ($arg, ..., ~varargs, $arg, ...)
  grammar arg
  | $arg_type
  | $arg_id :: $arg_type
  | $arg_type = $auto_expr
  | $arg_id :: $arg_type = $auto_expr
  grammar result_tuple
  | $result_type
  | ($result)
  | ($result, $errno)
  grammar result
  | $result_type
  | $result_id :: $result_type
  | $result_id :~ $result_type
  grammar errno
  | ~errno
  | ~get_last_error
  | $errno_id :: ~errno
  | $errno_id :: ~get_last_error
  grammar arrow_option
  | ~abi $abi
  | ~atomic
  | ~collect_safe
  | ~allow_callback_exn
  | ~in_original
){

 Describes the type of a function. The C representation of a function
 is an address, while the Rhombus representation is a Rhombus function.

 When a function type is used to convert a C function pointer to
 Rhombus, then calling the Rhombus function is a @deftech{foreign
  callout}. When a function type is use to convert a Rhombus function to a
 C function pointer, then it represents a @deftech{foreign callback}.

 The @rhombus(args_tuple) component of a function type describes the
 argument types for a C procedure. The tuple can be a single type, or it
 can be a parenthesized sequence of argument descriptions. If
 @rhombus(~varargs) appears in the argument sequence, it corresponds to a
 point in the argument sequence where the remainder is represented by
 just @tt{...} in a C function prototype; the Racket representation of
 the function expects additional arguments described after
 @rhombus(~varargs) as required arguments (so, a C function that has
 variable arguments must be converted through a different type for each
 different combination of arguments to be supplied).

 Each argument is represented by an @rhombus(arg_type), an optional
 @rhombus(arg_id), and an optional @rhombus(auto_expr). If an argument
 has an @rhombus(arg_id), it can be referenced by later
 @rhombus(auto_expr)s as well as in @rhombus(result_tuple) and a
 @rhombus(body) sequence. If an argument has a @rhombus(auto_expr), then
 @rhombus(auto_expr) supplies the Rhombus representation of an argument
 to be passed to a C function for a @tech{foreign callout}, while the
 Rhombus representation of a callout omits the argument; an
 @rhombus(auto_expr) is not used for a @tech{foreign callback}.

 The @rhombus(result_tuple) component describes the result protocol for
 the C function. It has at least a @rhombus(result_type), and it may have
 an @rhombus(errno). The C representation of the @rhombus(result_type)
 describes the result from the C function. If an @rhombus(errno) is
 present, then the Rhombus representation for a @tech{foreign callout}
 returns two values (when no @rhombus(body) sequence is present): a
 converted value for the C result and an integer for the value of the C
 library's @tt{errno} or Windows's @tt{GetLastError()} just after the C
 function returns. The C function result can have a @rhombus(result_id),
 and an @rhombus(errno) result can have an optional @rhombus(errno_id);
 those identifiers can be referenced in a @rhombus(body) sequence. An
 @rhombus(errno) component is not used for a @tech{foreign callback}.

 If a non-empty @rhombus(body) sequence is present, then it determines
 the result of the Rhombus representation of the function for a
 @tech{foreign callout}. A @rhombus(body) sequence can refer to the
 original arguments and the results via @rhombus(arg_id)s,
 @rhombus(result_id), and/or @rhombus(errno_id). A @rhombus(body)
 sequence is not used for a @tech{foreign callback}.

 Additional @rhombus(arrow_option)s affects the way a C function is
 called or how a callback is handled:

@itemlist(

 @item{@rhombus(~abi abi): Uses @rhombus(abi) as the @tech{ABI} for a
  @tech{foreign callout} or @tech{foreign callback}. See
  @rhombus(default_abi, ~at rhombus/ffi/abi),
  @rhombus(cdecl_abi, ~at rhombus/ffi/abi), and
  @rhombus(stdcall_abi, ~at rhombus/ffi/abi).}

 @item{@rhombus(~atomic): Adjusts a @tech{foreign callout} to
  potentially improve performance. The foreign function must not invoke
  any callbacks or otherwise reach the Rhombus run-time system, so it can be
  considered an atomic operation from the perspective of Rhombus. This
  option has no effect on @tech{foreign callbacks}.}

 @item{@rhombus(~collect_safe): Adjusts a @tech{foreign callout} to
  allow Rhombus garbage collection concurrent with the call, or adjusts a
  @tech{foreign callback} to re-enable synchronization with the garbage
  collector during the callback (i.e., only collect-safe callbacks are
  allowed to be invoked via a collect-safe procedure call; a collect-safe
  callback can be invoked through a non-collect-safe foreign procedure
  call). Note that a collect-safe call makes sense only when arguments to
  the foreign procedure are not managed by the Rhombus garbage collector
  or are immobile and reliably retained.}

 @item{@rhombus(~allow_callback_exn): Adjusts a @tech{foreign callout}
  so that a foreign callback is allowed to raise an exception that escape
  the foreign-function call. This option has no effect on @tech{foreign
   callbacks} themselves. A foreign callback must never raise an exception
  unless it is invoked via foreign function call using this option.}

 @item{@rhombus(~in_original): Adjusts a @tech{foreign callout} to take
  place in a coroutine thread within Rhombus's main place (which is useful
  if the foreign function is not thread-safe), or adjusts a @tech{foreign
   callback} invocation so that it takes place in a coroutine thread within
  the current place (which can be useful if the callback might otherwise
  run in a thread not created by Rhombus). The callout or callback happens
  in the context of an unspecified Rhombus coroutine thread, so it must not
  raise an exception.}

)

@examples(
  ~eval: ffi_eval
  ~repl:
    def strlen_ptr = Lib.load(#false).find("strlen") // => C library
    strlen_ptr
    (cast (string_t -> int_t)strlen_ptr)("hello")
)


}

@doc(
  ~nonterminal:
    args_tuple: -> ~at rhombus/ffi/type
    result_tuple: -> ~at rhombus/ffi/type
    arrow_option: -> ~at rhombus/ffi/type
    c_id: block id
    lib_expr: block expr
    fail_handler_expr: block expr
    wrapper_expr: block expr
    annot: ::
  defn.macro 'foreign.fun $id $args_tuple $result_tuple $result_annot'
  defn.macro 'foreign.fun $id $args_tuple $result_tuple $result_annot:
                $option
                ...
                $body
                ...'
  grammar result_annot
  | :: $annot
  | :~ $annot
  | :: #,(@rhombus(values, ~annot))($annot, ...)
  | :~ #,(@rhombus(values, ~annot))($annot, ...)
  | ϵ
  grammar option
  | ~lib: $lib_expr
  | ~c_id: $c_id
  | ~fail: $fail_handler_expr
  | ~wrap: $wrapper_expr
  | arrow_option
){

 Defines @rhombus(id) by combining a @rhombus(->)-like description of a
 function type with @rhombus(Lib.find) to extract a function pointer from
 the foreign library represented by the result of @rhombus(lib_expr). The
 export located in the foreign library is the symbol form of
 @rhombus(c_id) if provided, otherwise it is the symbolic form of
 @rhombus(id). The @rhombus(~lib) form of @rhombus(option) is required.

 When a @rhombus(result_annot) is present, it provides an annotation for
 the result of the Racket function defined as @rhombus(id). The result
 value is checked against the annotation when @rhombus(::, ~bind) is
 used (as opposed to @rhombus(:~, ~bind)).

 When @rhombus(~fail) is provided, then @rhombus(fail_handler_expr)
 should produce a function that expects one argument. When a relevant
 export is not found in the result of @rhombus(lib_expr), then the result
 of @rhombus(fail_handler_expr) is called with the name that was not
 found (i.e., @rhombus(c_id) or @rhombus(id)).

 When @rhombus(~wrap) is provided, then @rhombus(wrapper_expr) should
 produce a function that expects one argument. In that case, the value
 that @rhombus(id) would be bound to is passed to the result of
 @rhombus(wrapper_expr), and @rhombus(id) is defined as the result of the
 wrapper.

@examples(
  ~eval: ffi_eval
  ~defn:
    foreign.fun strlen(string_t) :: int_t:
      ~lib: Lib.load(#false) // => C library
  ~repl:
    strlen("hello")
  ~defn:
    foreign.fun verbose_strlen(s :: string_t) :: (len :: int_t) :: String:
      ~lib: Lib.load(#false) // => C library
      ~c_id: strlen
      @str{The string @repr(s) has @len bytes in its UTF-8 encoding}
  ~repl:
    println(verbose_strlen("π day"))
)

}

@doc(
  ~nonterminal:
    args_tuple: -> ~at rhombus/ffi/type
    result_tuple: -> ~at rhombus/ffi/type
    arrow_option: -> ~at rhombus/ffi/type
    c_id: block id
    type: * type ~at rhombus/ffi/type
    lib_expr: block expr
    fail_handler_expr: block expr
  defn.macro 'foreign.def $ref_or_id :: $type'
  defn.macro 'foreign.def $ref_or_id :: $type:
                $option
                ...'
  grammar ref_or_id
  | $id
  | & $id
  grammar option
  | ~lib: $lib_expr
  | ~c_id: $c_id
  | ~fail: $fail_handler_expr
){

 Similar to @rhombus(foreign.fun), but defines @rhombus(id) to the
 Rhombus representation of a foreign-library export.

 If @rhombus(&) appears before @rhombus(id), then @rhombus(id) is
 defined as a @tech{pointer} object with static information to indicate
 @rhombus(type) as the content type.

 If @rhombus(&) does not appear before @rhombus(id), then @rhombus(id)
 is defined as a the conversion of the content at a C library export
 (which is an address) to the Rhombus representation of @rhombus(type).

}

@doc(
  ~nonterminal:
    lib_expr: block expr
    fail_handler_expr: block expr
    wrapper_expr: block expr
  defn.macro 'foreign.linker $id:
                $option
                ...'
  grammar option
  | ~lib: $lib_expr
  | ~default_fail: $fail_handler_expr
  | ~default_wrap: $wrapper_expr
){

 Defines @rhombus(id) to be a namespace that provides a definition form
 @rhombus(id.def) that's like @rhombus(foreign.def), and a definition
 form @rhombus(id.fun) that's like @rhombus(foreign.fun), but using the
 result of @rhombus(lib_expr) automatically. That is, @rhombus(id.def)
 and @rhombus(id.fun) disallow a @rhombus(~lib) option, because it is
 provided to @rhombus(foreign.linker).

 If @rhombus(~default_fail) is specified, then the result of
 @rhombus(fail_handler_expr) is used for a @rhombus(~fail) clause if a
 use of @rhombus(id.def) or @rhombus(id.fun) does not include one.

 If @rhombus(~default_wrap) is specified, then the result of
 @rhombus(wrapper_expr) is used for a @rhombus(~wrap) clause if a
 use of @rhombus(id.fun) does not include one.

}

@doc(
  ~nonterminal:
    key: system_case ~at rhombus/ffi/type
    vals: system_case ~at rhombus/ffi/type
  abi.macro 'default_abi'
  abi.macro 'cdecl_abi'
  abi.macro 'stdcall_abi'
  abi.macro 'system_case $key
             | $vals: $abi
             | ~else: $abi'
){

 An @deftech{ABI} specifies a calling convention to use for a foreign
 call or callback. On most platforms, the only meaningful ABI is
 @rhombus(default_abi, ~at rhombus/ffi/abi), because most platforms have
 only a single standard ABI for C procedures. Windows for 32-bit x86
 defines multiple procedure ABIs, and
 @rhombus(cdecl_abi, ~at rhombus/ffi/abi) and
 @rhombus(stdcall_abi, ~at rhombus/ffi/abi) specify alternative ABIs for
 that platform; on other platforms,
 @rhombus(cdecl_abi, ~at rhombus/ffi/abi) and
 @rhombus(stdcall_abi, ~at rhombus/ffi/abi) are treated the same as
 @rhombus(default_abi, ~at rhombus/ffi/abi).

 The @rhombus(system_case, ~at rhombus/ffi/abi) ABI form is analogous to
 @rhombus_t(system_case) for types.

}

@close_eval(ffi_eval)
