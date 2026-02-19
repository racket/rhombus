#lang rhombus/scribble/manual

@(import:
    "pict_eval.rhm".pict_eval
    "plot.rhm".plot
    pict.bend
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "rebuild"){Pict Rebuilds}

@doc(
  method (pict :: Pict).rebuild(
    ~pre: pre_adjust :: Function.of_arity(1),
    ~post: post_adjust :: Function.of_arity(1),
    ~configure: config_adjust :: Function.of_arity(1)
                  = fun(config): config
  ) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction with @rhombus(pre_adjust) and
 @rhombus(post_adjust) applied to each component pict, and with
 @rhombus(config_adjust) applied to each configuration map of a pict
 created by @rhombus(rebuildable).

 To support @rhombus(Pict.rebuild), the representation of a pict
 effectively records all primitive operations used to construct the pict.
 This recording is limited to @rhombus(Pict, ~annot) and @rhombus(Find, ~annot) objects.
 If, for example, you use @rhombus(Find.in) to obtain a number and then
 construct a pict using that number, the number itself cannot record its
 derivation from the picts used with @rhombus(Find.in). In such cases,
 use @rhombus(rebuildable) or the @rhombus(~children) argument of
 @rhombus(animate) to establish a connection between the input picts and
 the result.

 When traversing the children of @rhombus(pict) to rebuild it, the
 @rhombus(pre_adjust) function is applied to each pict before its own
 children. When @rhombus(pre_adjust) returns a value other than the pict
 that it is given, rebuilding does not recur to its children, and the
 result of @rhombus(pre_adjust) is used immediately as the rebuilt
 replacement for the given pict. When rebuilding does recur, when a
 pict's descendants are unchanged and when @rhombus(config_adjust) (if
 applicable) returns a configuration unchanged, then the original
 construction of the pict is kept. Finally, @rhombus(post_adjust) is applied to either
 the rebuilt pict or so-far-preserved original pict to obtain the rebuilt
 replacement for the original pict. The replacement of a given pict is
 cached, so @rhombus(pre_adjust) and @rhombus(post_adjust) are each
 applied at most once to a pict within a call to @rhombus(Pict.rebuild).

 A rebuilt pict--either @rhombus(pict) itself or a rebuilt
 dependency---shares the identity of the original pict, so the rebuilt
 pict can be located via @rhombus(Find, ~class) objects, replaced via
 @rhombus(Pict.replace), or extracted using @rhombus(Pict.find_rebuilt).
 See also @secref("identity").

@examples(
  ~eval: pict_eval
  def s = square(~size: 20, ~fill: "blue")
  def p = beside(~sep: 10, s, Pict.launder(s))
  p
  p.rebuild(~pre: fun (q): if q == s | s.scale(2) | q)
  p.rebuild(~pre: fun (q): if q == s | s.scale(2) | q,
            ~post: fun (q :~ Pict):
                     rectangle(~around: q.pad(2), ~line: "red"))
)

}

@doc(
  method (pict :: Pict).replace(orig :: Pict,
                                replacement :: Pict) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction to replace each use of @rhombus(orig)
 with @rhombus(replacement).

 This operation is equivalent to a use of @rhombus(Pict.rebuild):

@rhombusblock(
  pict.rebuild(~pre: fun (p):
                       if p == orig
                       | replacement
                       | p)
)

@examples(
  ~eval: pict_eval
  def s = square(~size: 20, ~fill: "blue")
  def c = circle(~size: 15, ~fill: "red")
  def p = beside(~sep: 10, s, s)
  p
  p.replace(s, c)
)

}

@doc(
  method (pict :: Pict).configure(key :: Any, vall :: Any) :: Pict
){

 Returns a @tech{pict} that is like @rhombus(pict), but replays
 @rhombus(pict)'s construction to replace any configuration map entry for
 @rhombus(key) with @rhombus(val).

 This operation is equivalent to a use of @rhombus(Pict.rebuild):

@rhombusblock(
  pict.rebuild(~configure:
                 fun (config :: Map):
                   if config.has_key(key)
                   | config ++ { key: val }
                   | config)
)
}


@doc(
  method (pict :: Pict).find_rebuilt(orig :: Pict) :: maybe(Pict)
){

 Returns a descendant of @rhombus(pict) (by recursively checking
 @tech{findable children}) that has the same identity as @rhombus(orig), or
 returns @rhombus(#false) no such pict can be found.

 See @secref("identity") for more information about pict identity and
 rebuilt picts.

}

@doc(
  fun rebuildable(
    rebuild
      :: (~deps: List.of(Pict)) -> Pict
      || (~config: maybe(Map)) -> Pict
      || (~deps: List.of(Pict), ~config: maybe(Map)) -> Pict,
    ~deps: deps :: List.of(Pict) = [],
    ~config: config :: maybe(Map) = #false
  ) :: Pict
){

 Creates a pict that is the same as the result of
 @rhombus(rebuild(~deps: deps)), @rhombus(rebuild(~config: config)) or
 @rhombus(rebuild(~deps: deps, ~config: config)), but where
 @rhombus(deps) contains picts that can be adjusted via
 @rhombus(Pict.rebuild) or @rhombus(Pict.replace), and where @rhombus(config) can be updated by
 @rhombus(Pict.rebuild) or @rhombus(Pict.configure). When @rhombus(rebuild) is called, its result is
 wrapped as the sole child of a new pict whose identity is the same as
 the result of @rhombus(rebuildable) itself; see also
 @secref("identity").

 The given @rhombus(deps) list determines the @tech{replaceable dependencies} of the
 result pict, but the result from @rhombus(proc) determines the
 @tech{findable children}.

}
