#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "srcloc.rkt"
                     "make-get-veneer-like-static-infos.rkt"
                     "extract-elem-from-literal.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "static-info.rkt"
         "define-arity.rkt"
         "number.rkt"
         "call-result-key.rkt"
         "compare-key.rkt"
         (submod "annotation.rkt" for-class)
         (submod "literal.rkt" for-info)
         (submod "symbol.rkt" for-static-info)
         "class-primitive.rkt"
         "literal.rkt")

(provide (for-spaces (#f
                      rhombus/repet
                      rhombus/bind
                      rhombus/annot
                      rhombus/namespace)
                     Char)
         (for-space rhombus/annot
                    CharCI))

(module+ for-builtin
  (provide char-method-table))

(module+ for-static-info
  (provide (for-syntax get-char-static-infos)))

(define-primitive-class Char char
  #:lift-declaration
  #:no-constructor-static-info
  #:instance-static-info ((#%compare ((compare_to char-compare-to)
                                      (< char<?)
                                      (<= char<=?)
                                      (= char=?)
                                      (!= char!=?)
                                      (>= char>=?)
                                      (> char>?))))
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  ([from_int Char.from_int])
  #:properties
  ()
  #:methods
  (to_int
   utf8_length
   is_alphabetic
   is_lowercase
   is_uppercase
   is_titlecase
   is_numeric
   is_symbolic
   is_punctuation
   is_graphic
   is_whitespace
   is_blank
   is_iso_control
   is_extended_pictographic
   general_category
   grapheme_break_property
   upcase
   downcase
   foldcase
   titlecase
   grapheme_step))

(define-for-syntax (convert-char-ci-compare-static-info static-info)
  (syntax-parse static-info
    #:datum-literals (#%compare)
    [(#%compare . _) #'(#%compare ((compare_to char-ci-compare-to)
                                   (< char-ci<?)
                                   (<= char-ci<=?)
                                   (= char-ci=?)
                                   (!= char-ci!=?)
                                   (>= char-ci>=?)
                                   (> char-ci>?)))]
    [_ static-info]))

(define-for-syntax (get-char-ci-static-infos)
  (make-get-veneer-like-static-infos get-char-static-infos
                                     convert-char-ci-compare-static-info))

(define-annotation-syntax CharCI
  (identifier-annotation char? #,(get-char-ci-static-infos) #:static-only))

(define-syntax Char
  (expression-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id str . new-tail)
        (define char (extract-char-from-string #'form-id #'str))
        (values (wrap-static-info*
                 (relocate+reraw
                  (respan (datum->syntax #f (list #'form-id #'str)))
                  #`(quote #,char))
                 (get-char-static-infos))
                #'new-tail)]))))

(define-repetition-syntax Char
  (repetition-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id str . new-tail)
        (define char (extract-char-from-string #'form-id #'str))
        (values (make-repetition-info (list #'form-id #'str)
                                      '()
                                      #`(quote #,char)
                                      (get-char-static-infos)
                                      0)
                #'new-tail)]))))

(define-binding-syntax Char
  (binding-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id str . new-tail)
        (define char (extract-char-from-string #'form-id #'str))
        (values (binding-form #'literal-infoer
                              #`([#,char #,(string-append "Char" (shrubbery-syntax->string #'str))]))
                #'new-tail)]))))

(define-for-syntax (extract-char-from-string form-id str-stx)
  (extract-elem-from-literal form-id str-stx
                             string? string-length string-ref
                             "character" "string"))

(define/method (Char.to_int c)
  #:primitive (char->integer)
  #:static-infos ((#%call-result #,(get-int-static-infos)))
  (char->integer c))

(define/arity (Char.from_int i)
  #:primitive (integer->char)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (integer->char i))

(define/method (Char.utf8_length c)
  #:primitive (char-utf-8-length)
  (char-utf-8-length c))

(define/method (Char.is_alphabetic c)
  #:primitive (char-alphabetic?)
  (char-alphabetic? c))

(define/method (Char.is_lowercase c)
  #:primitive (char-lower-case?)
  (char-lower-case? c))

(define/method (Char.is_uppercase c)
  #:primitive (char-upper-case?)
  (char-upper-case? c))

(define/method (Char.is_titlecase c)
  #:primitive (char-title-case?)
  (char-title-case? c))

(define/method (Char.is_numeric c)
  #:primitive (char-numberic?)
  (char-numeric? c))

(define/method (Char.is_symbolic c)
  #:primitive (char-symbolic?)
  (char-symbolic? c))

(define/method (Char.is_punctuation c)
  #:primitive (char-punctuation?)
  (char-punctuation? c))

(define/method (Char.is_graphic c)
  #:primitive (char-graphics?)
  (char-graphic? c))

(define/method (Char.is_whitespace c)
  #:primitive (char-whitespace?)
  (char-whitespace? c))

(define/method (Char.is_blank c)
  #:primitive (char-blank?)
  (char-blank? c))

(define/method (Char.is_iso_control c)
  #:primitive (char-blank?)
  (char-blank? c))

(define/method (Char.is_extended_pictographic c)
  #:primitive (char-extended-pictographic?)
  (char-extended-pictographic? c))

(define/method (Char.general_category c)
  #:primitive (char-general-category)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (char-general-category c))

(define/method (Char.grapheme_break_property c)
  #:primitive (char-grapheme-break-property)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (char-grapheme-break-property c))

(define/method (Char.upcase c)
  #:primitive (char-upcase)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (char-upcase c))

(define/method (Char.downcase c)
  #:primitive (char-downcase)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (char-downcase c))

(define/method (Char.foldcase c)
  #:primitive (char-foldcase)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (char-foldcase c))

(define/method (Char.titlecase c)
  #:primitive (char-titlecase)
  #:static-infos ((#%call-result #,(get-char-static-infos)))
  (char-titlecase c))

(define/method (Char.grapheme_step c state)
  #:primitive (char-grapheme-step)
  (char-grapheme-step c state))

(define (raise-char-comp-failure who a b)
  (raise-annotation-failure who (if (char? a) b a) "Char"))

(define (char!=? a b)
  (if (and (char? a) (char? b))
      (not (char=? a b))
      (raise-char-comp-failure '!= a b)))

(define (char-compare-to a b)
  (unless (and (char? a) (char? b))
    (raise-char-comp-failure 'compare_to a b))
  (cond
    [(char=? a b) 0]
    [(a . char<? . b) -1]
    [else 1]))

(define (char-ci!=? a b)
  (if (and (char? a) (char? b))
      (not (char-ci=? a b))
      (raise-char-comp-failure '!= a b)))

(define (char-ci-compare-to a b)
  (unless (and (char? a) (char? b))
    (raise-char-comp-failure 'compare_to a b))
  (cond
    [(char-ci=? a b) 0]
    [(a . char-ci<? . b) -1]
    [else 1]))

(begin-for-syntax
  (install-get-literal-static-infos! 'char get-char-static-infos))
