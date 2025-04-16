#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         racket/path
         "provide.rkt"
         "class-primitive.rkt"
         "call-result-key.rkt"
         "define-arity.rkt"
         "compare-key.rkt"
         "index-result-key.rkt"
         "maybe-key.rkt"
         "static-info.rkt"
         "enum.rkt"
         "name-root.rkt"
         "rhombus-primitive.rkt"
         "parens.rkt"
         "parse.rkt"
         "rename-parameter.rkt"
         (submod "annotation.rkt" for-class)
         (submod "bytes.rkt" static-infos)
         (submod "parameter.rkt" for-info)
         (submod "list.rkt" for-listable)
         (submod "string.rkt" static-infos)
         (submod "symbol.rkt" for-static-info))

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot)
                     Path
                     CrossPath)
         (for-space rhombus/annot
                    PathString))

(module+ for-builtin
  (provide path-method-table))

(module+ for-static-info
  (provide (for-syntax get-path-static-infos)))

(define (path-order who p q)
  (cond
    [(path? p)
     (cond
       [(path? q) (cond
                    [(path<? p q) -1]
                    [(path<? q p) 1]
                    [else 0])]
       [(path-for-some-system? q) (if (eq? (path-convention-type p) 'unix)
                                      1
                                      -1)]
       [else (raise-annotation-failure who q "CrossPath")])]
    [(path-for-some-system? p)
     (cond
       [(path? q) 1]
       [(path-for-some-system? q)
        (cond
          [(eq? (path-convention-type p)
                (path-convention-type q))
           (cond
             [(bytes<? (path->bytes p) (path->bytes q)) -1]
             [(bytes>? (path->bytes p) (path->bytes q)) 1]
             [else 0])]
          [(eq? (path-convention-type p) 'unix) -1]
          [else 1])]
       [else (raise-annotation-failure who q "CrossPath")])]
    [else (raise-annotation-failure who p "CrossPath")]))

(define (cross-path<? p q) ((path-order '|Path.(<)| p q) . < . 0))
(define (path<=? p q) ((path-order '|Path.(<=)| p q) . <= . 0))
(define (path=? p q) ((path-order '|Path.(=)| p q) . = . 0))
(define (path>=? p q) ((path-order '|Path.(>=)| p q) . >= . 0))
(define (path>? p q) ((path-order '|Path.(>)| p q) . > . 0))
(define (path!=? p q) (not ((path-order '|Path.(=)| p q) . = . 0)))

(define-static-info-getter get-any-path-static-infos
  (#%compare ((< cross-path<?)
              (<= path<=?)
              (> path>?)
              (>= path>=?)
              (= path=?)
              (!= path!=?))))

(define-primitive-class Path path
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-path-static-infos)
  #:existing
  #:translucent
  #:fields
  ([bytes Path.bytes #,(get-bytes-static-infos)])
  #:namespace-fields
  ([Absolute Path.Absolute]
   [Relative Path.Relative]
   [DriveRelative Path.DriveRelative]
   [Element Path.Element]
   [Directory Path.Directory]
   [Dot Path.Dot]
   [like Path.like]
   [current_directory Path.current_directory]
   [current_directory_for_user Path.current_directory_for_user])
  #:properties
  ()
  #:methods
  (name
   parent
   bytes
   string
   convention
   add
   split
   to_absolute_path
   to_directory_path
   directory_only
   suffix
   add_suffix
   replace_suffix
   cleanse
   normal_case
   simplify
   as_relative_to))

(define/arity #:name Path (path c)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (cond
    [(path? c) c]
    [(bytes? c) (bytes->path c)]
    [(string? c) (string->path c)]
    [(or (eq? c 'up) (eq? c 'same)) (build-path c)]
    [else (raise-annotation-failure who c "ReadableString || Bytes || Path || Path.Dot")]))

(define-primitive-class CrossPath path-for-some-system
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info #,(get-any-path-static-infos)
  #:existing
  #:translucent
  #:fields
  ([bytes Path.bytes #,(get-bytes-static-infos)]
   [convention Path.convention #,(get-symbol-static-infos)])
  #:namespace-fields
  ([Absolute CrossPath.Absolute]
   [Relative CrossPath.Relative]
   [DriveRelative CrossPath.DriveRelative]
   [Element CrossPath.Element]
   [Directory CrossPath.Directory]
   [Unix CrossPath.Unix]
   [Windows CrossPath.Windows]
   [Convention CrossPath.Convention])
  #:properties
  ()
  #:methods
  ()
  #:dot-methods
  ([name Path.name]
   [parent Path.parent]
   [bytes Path.bytes]
   [string Path.string]
   [convention Path.convention]
   [add Path.add]
   [split Path.split]
   [to_absolute_path Path.to_absolute_path]
   [to_directory_path Path.to_directory_path]
   [directory_only Path.directory_only]
   [suffix Path.suffix]
   [add_suffix Path.add_suffix]
   [replace_suffix Path.replace_suffix]
   [cleanse Path.cleanse]
   [normal_case Path.normal_case]
   [simplify Path.simplify]
   [as_relative_to Path.as_relative_to]))

(define/arity #:name CrossPath (path-for-some-system bstr [c (system-path-convention-type)])
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (or (bytes? bstr) (eq? bstr 'up) (eq? bstr 'same))
    (raise-annotation-failure who bstr "Bytes || Path.Dot"))
  (unless (or (eq? c 'unix) (eq? c 'windows)) (raise-annotation-failure who c "CrossPath.Convention"))
  (if (bytes? bstr)
      (bytes->path bstr c)
      (build-path/convention-type c bstr)))

(define (path-is-absolute? v)
  (and (path? v)
       (complete-path? v)))

(define (path-is-relative? v)
  (and (path? v)
       (relative-path? v)))

(define (path-is-drive-relative? v)
  (and (path? v)
       (absolute-path? v)
       (not (complete-path? v))))

(define (path-directory? s)
  (and (path? s)
       (let-values ([(base name dir?) (split-path s)])
         dir?)))

(define (this-system-path-element? s)
  (and (path? s) (path-element? s)))

(define-annotation-syntax Path.Absolute
  (identifier-annotation path-is-absolute? #,(get-path-static-infos)))

(define-annotation-syntax Path.Relative
  (identifier-annotation path-is-relative? #,(get-path-static-infos)))

(define-annotation-syntax Path.DriveRelative
  (identifier-annotation path-is-drive-relative? #,(get-path-static-infos)))

(define-name-root Path.Element
  #:fields
  ([maybe Path.Element.maybe]
   [string Path.Element.string]
   [bytes Path.Element.bytes]))

(define-annotation-syntax Path.Element
  (identifier-annotation this-system-path-element? #,(get-path-static-infos)))

(define-annotation-syntax Path.Directory
  (identifier-annotation path-directory? #,(get-path-static-infos)))

(define-simple-symbol-enum Path.Dot
  up
  same)

(define-annotation-syntax Path.like
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (syntax-parse stx
       [(form-id (~and args (_::parens g)) . tail)
        (values (annotation-predicate-form
                 (relocate+reraw
                  (datum->syntax #f (list #'form-id #'args))
                  #`(let ([c (path-like-convention 'form-id (rhombus-expression g))])
                      (lambda (v)
                        (and (path-for-some-system? v)
                             (eq? (path-convention-type v) c)))))
                 (get-path-for-some-system-static-infos))
                #'tail)]))))

(define (path-like-convention who v)
  (cond
    [(path-for-some-system? v)
     (path-convention-type v)]
    [(or (path-string? v)
         (eq? v 'up)
         (eq? v 'same))
     (system-path-convention-type)]
    [else (raise-annotation-failure who v "PathString || CrossPath || Path.Dot")]))

(define/arity (Path.Element bstr)
  #:primitive (bytes->path-element)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path-element bstr))

(define/arity (Path.Element.maybe bstr)
  #:static-infos ((#%call-result ((#%maybe #,(get-path-static-infos)))))
  (unless (bytes? bstr) (raise-annotation-failure who bstr "Bytes"))
  (bytes->path-element bstr (system-path-convention-type) #t))

(define/arity (Path.Element.string p)
  (unless (this-system-path-element? p) (raise-annotation-failure who p "Path.Element"))
  (string->immutable-string (path-element->string p)))

(define/arity (Path.Element.bytes p)
  (unless (this-system-path-element? p) (raise-annotation-failure who p "Path.Element"))
  (bytes->immutable-bytes (path-element->bytes p)))

(define Path.current_directory (rename-parameter current-directory 'Path.current_directory))
(define Path.current_directory_for_user (rename-parameter current-directory-for-user 'Path.current_directory_for_user))
(define-static-info-syntaxes (Path.current_directory Path.current_directory_for_user)
  (#%call-result (#:at_arities
                  ((1 #,(get-path-static-infos))
                   (2 ()))))
  . #,(get-parameter-static-infos))

(define/method (Path.name p)
  #:local-primitive (split-path)
  (define-values (parent name dir?) (split-path p))
  name)

(define/method (Path.parent p)
  #:local-primitive (split-path)
  (define-values (parent name dir?) (split-path p))
  parent)

(define/method (Path.bytes p)
  #:primitive (path->bytes)
  #:static-infos ((#%call-result #,(get-bytes-static-infos)))
  (bytes->immutable-bytes (path->bytes p)))

(define/method (Path.string s)
  #:primitive (path->string)
  #:static-infos ((#%call-result #,(get-string-static-infos)))
  (if (path-for-some-system? s)
      (string->immutable-string (some-system-path->string s))
      (string->immutable-string (path->string s))))

(define/method (Path.convention p)
  #:primitive (path-convention-type)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (path-convention-type p))

(define/method (Path.add p . ss)
  #:primitive (build-path)
  #:static-infos ((#%call-result #,(get-path-static-infos)))
  (apply build-path p ss))

(define/method (Path.split p)
  #:primitive (explode-path)
  #:static-infos ((#%call-result ((#%index-result #,(get-path-static-infos))
                                  #,@(get-treelist-static-infos))))
  (to-treelist #f (explode-path p)))

(define/method (Path.directory_only p)
  #:primitive (path-only)
  (path-only p))

(define/method (Path.suffix p)
  #:primitive (path-get-extension)
  #:static-infos ((#%call-result ((#%maybe #,(get-path-static-infos)))))
  (define maybe-bstr (path-get-extension p))
  (and maybe-bstr (bytes->immutable-bytes maybe-bstr)))

(define/method (Path.add_suffix p sfx #:sep [sep "_"])
  #:primitive (path-add-extension)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (path-add-extension p sfx sep))

(define/method (Path.replace_suffix p sfx)
  #:primitive (path-replace-extension)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (path-replace-extension p sfx))

(define/method (Path.to_absolute_path p #:relative_to [base-path (current-directory)])
  #:primitive (path->complete-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (path->complete-path p base-path))

(define/method (Path.to_directory_path p)
  #:primitive (path->directory-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (path->directory-path p))

(define/method (Path.cleanse p)
  #:primitive (cleanse-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (cleanse-path p))

(define/method (Path.normal_case p)
  #:primitive (normal-case-path)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (normal-case-path p))

(define/method (Path.simplify p)
  #:primitive (simplify)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (path-string? p) (raise-annotation-failure who p "PathString"))
  (simplify-path p #f))

(define/method (Path.as_relative_to p base-p
                                    #:more_than_root [more-than-root? #f]
                                    #:more_than_same [more-than-same? #t]
                                    #:normal_case [normalize-case? #t])
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (unless (or (path-string? p) (path-for-some-system? p)) (raise-annotation-failure who p "PathString || CrossPath"))
  (unless (or (path-string? base-p) (path-for-some-system? p)) (raise-annotation-failure who base-p "PathString || CrossPath"))
  (find-relative-path base-p
                      (if (string? p) (string->path p) p)
                      #:more-than-root? more-than-root?
                      #:more-than-same? more-than-same?
                      #:normalize-case? normalize-case?))

(define-annotation-syntax PathString (identifier-annotation path-string? ()))

(define (path-for-some-system-is-absolute? v)
  (and (path-for-some-system? v)
       (complete-path? v)))

(define (path-for-some-system-is-relative? v)
  (and (path-for-some-system? v)
       (relative-path? v)))

(define (path-for-some-system-is-drive-relative? v)
  (and (path-for-some-system? v)
       (absolute-path? v)
       (not (complete-path? v))))

(define (some-system-path-element? s)
  (path-element? s))

(define (some-system-path-directory? s)
  (and (path-element? s)
       (let-values ([(base name dir?) (split-path s)])
         dir?)))

(define-annotation-syntax CrossPath.Absolute
  (identifier-annotation path-for-some-system-is-absolute? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Relative
  (identifier-annotation path-for-some-system-is-relative? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.DriveRelative
  (identifier-annotation path-for-some-system-is-drive-relative? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Element
  (identifier-annotation some-system-path-element? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Directory
  (identifier-annotation some-system-path-directory? #,(get-path-for-some-system-static-infos)))

(define-simple-symbol-enum CrossPath.Convention
  #:extra
  ([current CrossPath.Convention.current])
  unix
  windows)

(define/arity (CrossPath.Convention.current)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (system-path-convention-type))

(define (path-is-unix? v)
  (and (path-for-some-system? v)
       (eq? 'unix (path-convention-type v))))

(define (path-is-windows? v)
  (and (path-for-some-system? v)
       (eq? 'windows (path-convention-type v))))

(define-annotation-syntax CrossPath.Unix
  (identifier-annotation path-is-unix? #,(get-path-for-some-system-static-infos)))

(define-annotation-syntax CrossPath.Windows
  (identifier-annotation path-is-windows? #,(get-path-for-some-system-static-infos)))

(define (CrossPath/convention who bstr convention)
  (unless (or (bytes? bstr) (eq? bstr 'up) (eq? bstr 'same))
    (raise-annotation-failure who bstr "Bytes || Path.Dot"))
  (if (bytes? bstr)
      (bytes->path bstr convention)
      (build-path/convention-type convention bstr)))

(define/arity (CrossPath.Unix bstr)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (CrossPath/convention who bstr 'unix))

(define/arity (CrossPath.Windows bstr)
  #:static-infos ((#%call-result #,(get-path-for-some-system-static-infos)))
  (CrossPath/convention who bstr 'windows))

(void (set-primitive-contract! 'path-string? "PathString"))
(void (set-primitive-contract! '(or/c path-string? path-for-some-system? 'up 'same) "PathString || CrossPath || Path.Dot"))
