#lang rhombus/scribble/manual
@(import:
    pict open
    draw
    meta_label:
      rhombus open
      pict open
      draw)

@title(~tag: "identity"){Pict Findable and Replaceable Identity}

Every predefined pict constructor or update method creates a pict that
has a fresh identity. The @rhombus(==) operation on picts compares them
by this identity. As noted in @secref("static-pict"), pict identity can
be used to find or replace a pict @rhombus(p, ~var) within another pict
@rhombus(q, ~var) when @rhombus(q, ~var) is constructed from
@rhombus(p, ~var). The @deftech{findable children} of a pict, as reported by the
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

Like a pict's width and height, it's findable location within another
pict is technically a property of a static pict. A snapshot (via
@rhombus(Pict.snapshot)) of a pict by default has an identity that is the same as
the original pict. Consequently, it is possible and convenient to find
the location of (a snapshot of) an animated pict within the snapshot of
another animated pict. In the same way that @rhombus(Pict.width) on an
animated pict produces a result matching a snapshot of the pict at the
start of its @tech{time box}, finding an animated pict within another
animated more precisely returns the location of a snapshot within a
snapshot.

Potentially distinct from the @tech{findable children} of a pict are its
@deftech{replaceable dependencies}, which are picts that can be
discovered and replaced with @rhombus(Pict.rebuild) or
@rhombus(Pict.replace). Normally, the findable children and replaceable
dependencies of a pict are the same, both determined by the pict's
construction. The @rhombus(rebuildable) function creates a pict with
only the listed dependencies, however, while the result of a
@rhombus(rebuild, ~var) function supplied to @rhombus(rebuildable)
determines its findable children. Similarly, @rhombus(animate) creates
an animated pict @rhombus(p, ~var) with declared dependencies, and
findable children corresponds to a snapshot. Note that the findable
children of a @rhombus(rebuildable) instance or @rhombus(animate)
snapshot can vary over time or configuration; for example, an
@rhombus(animate) snapshot might include some of its dependencies only
at certain times, and a @rhombus(rebuildable) configuration might chose
one or the other of its dependencies based on some comparison of the
dependencies; declaring dependencies explicitly makes replacement
consistent.

Similar to the way that @rhombus(Pict.snapshot) produces a pict with the
same findable and replaceable identity as the snapshotted pict, a
rebuilt pict via @rhombus(Pict.rebuild) similarly gets an identity
matching the original pict. This rule also allows the rebuilt version of
some pict @rhombus(p, ~var) to be found within a rebuilt version of
@rhombus(q, ~var) in the case that a dependency of @rhombus(p, ~var) is
updated to produce the rebuilt @rhombus(q, ~var). Also, in that case,
the @rhombus(Pict.find_rebuilt) method of the rebuilt @rhombus(q, ~var)
finds and returns the rebuilt pict that replaced @rhombus(p, ~var).
