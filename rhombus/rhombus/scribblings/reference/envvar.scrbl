#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "nonterminal.rhm" open
    meta_label:
      rhombus/envvar
      rhombus/envvar open)

@title(~tag: "envvar"){Environment Variables}

@docmodule(rhombus/envvar)

@doc(
  annot.macro 'envvar.StringEnvVarName'
  annot.macro 'envvar.BytesEnvVarName'
){

 Annotations that recognize strings and bytes without null characters or
 @litchar{=} characters, which makes them suitable as
 environment-variable names.

}

@doc(
  fun envvar.getenv(
    name :: StringEnvVarName
  ) :: StringNoNull
  fun envvar.putenv(
    name :: StringEnvVarName,
    value :: StringNoNull
  ) :: Boolean
){

 Convenience functions to get and set environment variables in terms of
 strings.

 The underlying environment-variables object works in terms of byte
 strings. String names and values are converted using the current
 locale’s default encoding, using @rhombus(Char"?") as the replacement
 character for encoding errors.

 The @rhombus(gettenv) and @rhombus(putenv) functions use
 @rhombus(EnvVars.current) to get the current environment variables, and
 then use @rhombus(EnvVars.get) or @rhombus(EnvVars.set) to get or set
 the byte-string encoding of @rhombus(name).

 The @rhombus(putenv) function returns @rhombus(#true) for success and
 @rhombus(#false) for failure, where failure is only possible when
 @rhombus(EnvVars.current) returns the original @rhombus(EnvVars, ~class)
 object for the process.

}

@doc(
  class envvar.EnvVars():
    constructor ({ name :: BytesEnvVarName: value :: BytesNoNull,
                   ... })
  method (envvars :: envvar.EnvVars).copy() :: EnvVars
  Parameter.def envvar.EnvVars.current :: EnvVars
){

 An @rhombus(EnvVars) instance represents a set of environment
 variables. The current object installed in @rhombus(EnvVars.current)
 determines the environment variables that are used for a subprocess
 created via @rhombusmodname(rhombus/subprocess).

 The initial value of @rhombus(EnvVars.current) corresponds to the
 process's environment variables. Setting an environment variable in that
 object adjusts the table of environment variables at the
 operating-system process level.

 Creating a new set of environment variables via @rhombus(EnvVars.copy)
 or the @rhombus(EnvVars) allocates an object with independent state.

}

@doc(
  method (envvars :: envvar.EnvVars).get(
    name :: BytesEnvVarName
  ) :~ maybe(ImmutableBytes)
  method (envvars :: envvar.EnvVars).set(
    name :: BytesEnvVarName,
    val :: maybe(BytesNoNull),
    ~fail: fail :: Function.of_arity(0) = ....
  )
){

 Gets or sets an environment variable in @rhombus(envvars).

 If @rhombus(envvars) is the initial value of @rhombus(EnvVars.current)
 in the process, setting an environment variable adjusts the table of
 environment variables at the operating-system process level.

 Setting an environment variable to @rhombus(#false) removes it from the
 set of environment-variable names and/or unsets it at the process level.

}


@doc(
  method (envvars :: envvar.EnvVars).names()
    :: List.of(BytesEnvVarName && ImmutableBytes)
){

 Returns a list of environment-variable names that are set in
 @rhombus(envvars).

}

@doc(
  property (envvars :: envvar.EnvVars).handle
  fun envvar.EnvVars.from_handle(handle) :: EnvVars
){

 The @rhombus(EnvVars.handle) property accesses an environment-variables
 object's underlying Racket representation. The
 @rhombus(EnvVars.from_handle) function constructs a Rhombus
 environment-variables object from a Racket environment-variables object.

}
