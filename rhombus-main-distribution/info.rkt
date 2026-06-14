#lang info

(define collection 'multi)

(define deps
  '("rhombus"

    "rhombus-ffi"
    "rhombus-draw"
    "rhombus-gui"
    "rhombus-pict"
    "rhombus-scribble"

    "rhombus-ssl"
    "rhombus-http"
    "rhombus-url"
    "rhombus-html"
    "rhombus-xml"
    "rhombus-json"
    "rhombus-net-cookie"

    "rhombus-icons"
    "rhombus-logo"

    "drracket-core"
    "xrepl"
    "expeditor"

    ;; ensure full-package dependency for anythign that
    ;; might have only a "-lib" dependency
    "gui-easy"
    "box-extra"
    "actor"
    "http-easy"
    "resource-pool"
    "rackcheck"
    "pretty-expressive"
    "version-case"))

(define pkg-desc "A package that combines all of the packages in the main Rhombus distribution")

(define license '(Apache-2.0 OR MIT))

(define language-families '("Rhombus"))
