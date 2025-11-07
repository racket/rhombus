#lang racket/base

;; This is the only module outside the "amalgam" directory that is
;; allowed to refer to modules in that directory. All other references
;; to that code need to go through the "amalgam.rkt" module and its
;; submodules, which (roughly) has a copy all of the "amalgam" modules
;; in one big module with big submodules.

;; Some modules here, such as `core` versus `core-macro`, represent
;; different sets of modules that might be loaded and instantiated
;; without other modules. The flattened form "amalgam.rkt" keeps those
;; entry points as separate submodules, so the separateness of loading
;; and instantiating the dependent modules is preserved. Those
;; different sets may overlap, and the demodularizer figures out how
;; to organize the dependent modules into submodules so that modules
;; are compiled together as much as possible, but no module is
;; duplicated when it is reachable from different entry points and
;; potentially at different phases.

(require (only-in "amalgam/core.rkt")
         "bounce.rkt")

(module+ core
  (require (rename-in "amalgam/core.rkt"
                      [#%module-begin module-begin]))
  (provide (except-out (all-from-out "amalgam/core.rkt")
                       module-begin)
           (rename-out [module-begin #%module-begin])))

(module+ core-macro
  (bounce "amalgam/core-macro.rkt"))

(module+ core-derived
  (bounce "amalgam/core-derived.rkt"))

(module+ core-meta
  (bounce "amalgam/core-meta.rkt")

  (module+ sequence_meta
    (bounce "amalgam/sequence_meta.rhm"))

  (module+ class-meta
    (bounce-meta "amalgam/class-meta.rkt"))

  (module+ interface-meta
    (bounce-meta "amalgam/interface-meta.rkt"))

  (module+ veneer-meta
    (bounce-meta "amalgam/veneer-meta.rkt")))

(module+ runtime-config
  (bounce "amalgam/runtime-config.rkt"))

(module+ expand-config
  (bounce "amalgam/expand-config.rkt"))

(module+ parse
  (bounce "amalgam/parse.rkt"))

(module+ dynamic-static
  (bounce "amalgam/dynamic-static.rkt"))

(module+ name-root
  (bounce "amalgam/name-root.rkt"
          "amalgam/name-root-ref.rkt"
          "amalgam/name-root-space.rkt"
          "amalgam/all-spaces-out.rkt"))

(module+ enforest
  (bounce enforest/transformer
          enforest/property
          enforest/proc-name
          enforest/syntax-local
          enforest/hier-name-parse))

(module+ more-parse
  (bounce "amalgam/parse.rkt"
          "amalgam/forwarding-sequence.rkt"
          (submod "amalgam/core.rkt" module-begin)))

(module+ module-block
  (bounce (submod "amalgam/core.rkt" module-block)))

(module+ srcloc
  (bounce "amalgam/srcloc.rkt"
          "amalgam/parsed-relocate.rkt"))

(module+ unsafe
  (bounce "amalgam/unsafe.rkt"))

(module+ modpath-meta
  (bounce (submod "amalgam/module-path.rkt" for-meta)))

(module+ modpath
  (require (only-in (submod "amalgam/module-path.rkt" for-import-export)
                    :module-path
                    current-module-path-context)
           (for-syntax
            (only-in "amalgam/module-path-parse.rkt" module-path-convert-parsed)))
  (provide (for-syntax :module-path
                       module-path-convert-parsed
                       current-module-path-context)))

(module+ pack
  (bounce "amalgam/pack.rkt"))

(module+ doc_spec
  (bounce "amalgam/doc_spec.rhm"))

(module+ error_adjust
  (bounce "amalgam/error-adjust.rkt"))

(module+ rx
  (bounce "amalgam/rx.rhm"))

(module+ deserializer
  (bounce (submod "amalgam/serializable.rkt" deserializer)))

(module+ deserialize-set
  (bounce (submod "amalgam/set.rkt" deserialize)))

(module+ evt
  (bounce "amalgam/evt.rkt")
  (bounce "amalgam/evt.rhm"))

(module+ dot
  (bounce (submod "amalgam/dot.rkt" for-external)))

(module+ syntax-map
  (bounce "amalgam/syntax-map-macro.rkt"))

(module+ unwrap
  (bounce (submod "amalgam/syntax-object.rkt" for-unwrap)))
