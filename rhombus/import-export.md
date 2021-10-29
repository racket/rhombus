# Import and Export forms

The Rhombus import and export forms provide capabilities similar to
Racket forms, but with a prefix identifier optionally associated with
imports.

## Imports

The general syntax of `import` is

```
import:
  <import-form>:
    <import-modifier>
    ...
  ...
```

where a prefix identifier is inferred from `<import-form>`, but it can
be changed or suppressed through an `<import-modifier>`. If a prefix
identifier is used, the imports are accessed using the prefix.

An `<import-form>` is a module path or a form using an import
transformer or operator. There are currently no predefined import
transformers or operators beyond the ones that form module paths, but
new `<import-form>`s can be defined.

_The future mechanism for adding a new import form will be similar to
the mechanism for macros in other contexts._

A module path is one of the following:

 * an identifier or a sequence of identifiers combined with the `/`
   operator to form a module path;

 * a string to use as a portable, relative module path as in Racket,
   where `/` in the string reflects filesystem subdirectories;

 * a `lib(<string>)` form, where `<string>` names a collection-based
   module as in Racket; or

 * a `file(<string>)` form, where `<string>` is a valid path on the
   current filesystem.

A sequence of `<import-modifier>`s is applied to an `<import-form>`.
To the degree that order matters, the `<import-modifier>`s are
applied in order from first to last. For example, if one
`<import-form>` renames an import, then any later `<import-modifier>`s
uses the new name instead of the old one. The set of
`<import-modifier>` forms is extensible, but it includes the following
forms:

 * `prefix <identifier>`: sets the import prefix to `<identifier>`.

 * `no_prefix`: imports without a prefix, so all of the imports are
   “dumped” into the namespace.

 * `rename` followed by a block with any number of groups of the form
   `<old-name> ~to <new-name>`, where each `<old-name>` or
   `<new-name>` can be an identifier or an operator. The `rename`
   modifier renames imports.

 * `only` followed by a block where each group lists any number of
   names (identifier and/or operators), which limits an import to the
   listed names.

 * `except` is like `only`, but it limits an import to the names _not_
   listed.

 * `expose` followed by a block where each group lists any number of
   names (identifier and/or operators), which makes those names usable
   without the import's prefix (if there is one) in addition to
   through the prefix.

_The future mechanism for adding a new import-modifier forms will be
similar to the mechanism for macros in other contexts._

## Exports

The general syntax of `export` is

```
export:
  <export-form>:
    <export-modifier>
    ...
  ...
```

The set of `<export-form>`s is extensible, but initially includes the following:

 * a name, which is an identifier or operator, that is not bound to an
   export transformer or operator, to export the identifier/operator.
   The name can be hierarchical reference, such as `math.pi`, in which
   case the last component of the reference is the exported name.

 * `names` followed by a block where each group lists any number of
   names (identifier and/or operators), which exports the identifiers
   and/or operators, even if they are bound to export transformers.

 * `rename` followed by a block with any number of groups of the form
   `<old-name> 'to' <new-name>`, where each `<old-name>` or
   `<new-name>` can be an identifier or an operator, and `<old-name>`
   can be a hierarchical reference. The `<old-name>` identifies the
   exported binding, while `<new-name>` is the name seen by importing
   contexts.

 * `all_in(<identifier>)` where `<identifier>` refers to a prefix
   bound by an `import`, possibly an inferred prefix. All names
   accessible through the `<identifier>.` prefix are exported.

 * `all_from(<module-path>)` where `<module-path>` is a module path
   that appears in one or more `import`s. All names from
   `<module-path>` that afe accessible through those imports are
   exported.

 * a juxtaposition of `<export-form>`s, which exports the union of the
   `<export-form>` exports.
                  
The set of `<export-modifier>`s is extensible, but initially includes
the following:

 * `except` followed by a block of more export specifications (usually
   just names). A set of exports is modified by removing the exports
   that are indicated by the additional export specifications.

_Why not 'rename' as an export modifier, in addition to the export
form? Only because Racket's `provide` form doesn't provide a
convenient analog at the moment, but it could be added._

## Example

Here's a junky example meant to demonstrate many `import` and `export`
forms.

```
// a.rhm
#lang rhombus

export:
  dont_look $
  rename: bleating ~to greeting

def dont_look:
  "oops"

def bleating:
  "Hello from A"

operator (a $ b):
  a+b
```

```
// b.rhm
#lang rhombus

import:
  "a.rhm":
    except: dont_look
    rename: $ ~to $$
  lib("racket/base.rkt"):
    no_prefix
    only: modulo remainder
  racket/base:
    prefix base
    rename: car ~to kar
            / ~to div
    expose: gcd

export:
  a base.cdr
  rename: base.cdr ~to kdr
  all_from(racket/base):
    except: remainder
  all_in(base):
    except: base.cdr

"Hello from B"
a.greeting
base.kar

1 a.($$) 2

// doesn't affect export form:
def all_from: "ok"

gcd
```

```
// c.rhm
#lang rhombus

import:
  "b.rhm"

b.a.greeting
b.cdr
b.modulo
```
