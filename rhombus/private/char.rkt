#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         "number.rkt"
         "call-result-key.rkt"
         "compare-key.rkt"
         (submod "annotation.rkt" for-class)
         (submod "literal.rkt" for-info)
         (submod "symbol.rkt" for-static-info)
         "realm.rkt")

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Char)
         (for-space rhombus/annot
                    CharCI))

(module+ for-static-info
  (provide (for-syntax char-static-infos)))

(define-for-syntax char-static-infos
  #'((#%compare ((< char<?)
                 (<= char<=?)
                 (= char=?)
                 (!= char!=?)
                 (>= char>=?)
                 (> char>?)))))

(define-annotation-syntax Char
  (identifier-annotation #'char? char-static-infos))
(define-annotation-syntax CharCI
  (identifier-annotation #'char? #'((#%compare ((< char-ci<?)
                                                (<= char-ci<=?)
                                                (= char-ci=?)
                                                (!= char-ci!=?)
                                                (>= char-ci>=?)
                                                (> char-ci>?))))))

(define-name-root Char
  #:fields
  (to_int
   from_int
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
   is_extended_pictographic
   general_category
   grapheme_break_property
   upcase
   downcase
   foldcase
   titlecase
   grapheme_step))

(define/arity (to_int c)
  #:static-infos ((#%call-result #,int-static-infos))
  (char->integer c))

(define/arity (from_int i)
  #:static-infos ((#%call-result #,char-static-infos))
  (integer->char i))

(define/arity (utf8_length c)
  (char-utf-8-length c))

(define/arity (is_alphabetic c)
  (char-alphabetic? c))

(define/arity (is_lowercase c)
  (char-lower-case? c))

(define/arity (is_uppercase c)
  (char-upper-case? c))

(define/arity (is_titlecase c)
  (char-title-case? c))

(define/arity (is_numeric c)
  (char-numeric? c))

(define/arity (is_symbolic c)
  (char-symbolic? c))

(define/arity (is_punctuation c)
  (char-punctuation? c))

(define/arity (is_graphic c)
  (char-graphic? c))

(define/arity (is_whitespace c)
  (char-whitespace? c))

(define/arity (is_blank c)
  (char-blank? c))

(define/arity (is_extended_pictographic c)
  (char-extended-pictographic? c))

(define/arity (general_category c)
  #:static-infos ((#%call-result #,symbol-static-infos))
  (char-general-category c))

(define/arity (grapheme_break_property c)
  #:static-infos ((#%call-result #,symbol-static-infos))
  (char-grapheme-break-property c))

(define/arity (upcase c)
  #:static-infos ((#%call-result #,char-static-infos))
  (char-upcase c))

(define/arity (downcase c)
  #:static-infos ((#%call-result #,char-static-infos))
  (char-downcase c))

(define/arity (foldcase c)
  #:static-infos ((#%call-result #,char-static-infos))
  (char-foldcase c))

(define/arity (titlecase c)
  #:static-infos ((#%call-result #,char-static-infos))
  (char-titlecase c))

(define/arity (grapheme_step c state)
  (char-grapheme-step c state))

(define (char!=? a b)
  (if (and (char? a) (char? b))
      (not (char=? a b))
      (raise-argument-error* '!= rhombus-realm "Char" (if (char? a) b a))))

(define (char-ci!=? a b)
  (if (and (char? a) (char? b))
      (not (char-ci=? a b))
      (raise-argument-error* '!= rhombus-realm "Char" (if (char? a) b a))))

(begin-for-syntax
  (install-literal-static-infos! 'char char-static-infos))
