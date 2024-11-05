#lang racket/base
(require (for-syntax racket/base)
         racket/path
         "name-root.rkt"
         "define-arity.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         (submod "path-object.rkt" for-static-info))

(provide (for-space rhombus/namespace
                    filesystem))

(define-name-root filesystem
  #:fields
  ([simplify_path filesystem.simplify_path]
   [normalize_path filesystem.normalize_path]
   [resolve_path filesystem.resolve_path]
   [expand_user_path filesystem.expand_user_path]
   [file_exists filesystem.file_exists]
   [directory_exists filesystem.directory_exists]
   [link_exists filesystem.link_exists]))

(define/arity (filesystem.simplify_path p)
  #:local-primitive (simplify-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (simplify-path p #t))

(define/arity (filesystem.normalize_path p)
  #:local-primitive (simplify-path) ; potential error from `simplify-path` as called by `simple-form-path`
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (simple-form-path p))

(define/arity (filesystem.resolve_path p)
  #:local-primitive (resolve-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (resolve-path p))

(define/arity (filesystem.expand_user_path p)
  #:primitive (expand-user-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (expand-user-path p))

(define/arity (filesystem.file_exists p)
  #:primitive (file-exists?)
  (file-exists? p))

(define/arity (filesystem.directory_exists p)
  #:primitive (directory-exists?)
  (directory-exists? p))

(define/arity (filesystem.link_exists p)
  #:primitive (link-exists?)
  (link-exists? p))
