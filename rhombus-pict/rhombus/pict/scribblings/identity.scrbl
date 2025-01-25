#lang rhombus/scribble/manual
@(import:
    pict open
    draw
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "identity"){Pict Identity and Dependencies}

Every predefined pict constructor or update method creates a pict that
has a fresh identity. The @rhombus(==) operation on picts compares them
by this identity. As noted in @secref("static-pict"), pict identity can
be used to find or replace a pict @rhombus(p, ~var) within another pict
@rhombus(q, ~var) when @rhombus(q, ~var) is constructed from
@rhombus(p, ~var). The @deftech{children} of a pict, as reported by the
@rhombus(Pict.children) property, are the immediate picts of
@rhombus(q, ~var) that were used to construct it, and recurring through
@rhombus(Pict.children) properties from @rhombus(q, ~var) reaches all
findable picts used to construct @rhombus(q, ~var).

Sometimes, it's useful to suppress the identity of a pict and all of the
picts used to build that one. The @rhombus(Pict.launder) operation on a
pict produces one that draws the same and has the same bounding box, but
has a fresh identity and that hides the identity of all picts used to
construct it, so @rhombus(Find, ~annot) and similar functions cannot
find them. The @rhombus(Pict.children) property of the result of
@rhombus(Pict.launder) is an empty list.

Potentially distinct from the @tech{children} of a pict are its
@deftech{dependencies}, which are picts that can be discovered and
replaced with @rhombus(Pict.rebuild) or @rhombus(Pict.replace).
Normally, the children and dependencies of a pict are the same, but
@rhombus(rebuildable) creates a pict with only the listed dependencies,
while the result of the @rhombus(proc, ~var) determines its children.
The @rhombus(animate) function similarly creates a pict
@rhombus(p, ~var) with declared depedencies; those dependencies also
serve as the children of @rhombus(p, ~var). When a snapshot of
@rhombus(p, ~var) is taken, the resulting pict has its own children, and
it also has its own dependencies---unless
@rhombus(~rebuild_prompt: #false) is supplied, in which case the
snapshot result has the same dependencies as @rhombus(p, ~var).

A snapshot (via @rhombus(Pict.snapshot)) or rebuild (via
@rhombus(Pict.rebuild)) of a pict has an identity that is the same as
the original pict. This allows a pict @rhombus(p, ~var) to be found
within a snapshot of itself or within a snapshot of a pict
@rhombus(q, ~var) that is built from @rhombus(p, ~var) (i.e., a pict
@rhombus(q, ~var) in which @rhombus(p, ~var) could be found). This rule
also allows the rebuilt version of @rhombus(p, ~var) to be found within
a rebuilt version of @rhombus(q, ~var) in the case that a dependency of
@rhombus(p, ~var) is updated to produce the rebuilt @rhombus(q, ~var).
Also in that case, the @rhombus(Pict.find_rebuilt) method of the rebuilt
@rhombus(q, ~var) finds and returns the rebuilt pict that replaced
@rhombus(p, ~var).

