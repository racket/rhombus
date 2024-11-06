#lang racket/base
(require (for-syntax racket/base)
         racket/path
         "name-root.rkt"
         "define-arity.rkt"
         "annotation-failure.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "static-info.rkt"
         "treelist.rkt"
         (submod "list.rkt" for-compound-repetition)
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
   [link_exists filesystem.link_exists]
   [list_directory filesystem.list_directory]))

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

(define-static-info-getter get-treelist-of-paths
  (#%index-result #,(get-path-static-infos))
  . #,(get-treelist-static-infos))

(define/arity (filesystem.list_directory [p (current-directory)]
                                         #:extend_path [extend? #f])
  #:primitive (directory-list)
  #:static-infos ((#%call-result #,(get-treelist-of-paths)))
  (list->treelist (directory-list p #:build? extend?)))
