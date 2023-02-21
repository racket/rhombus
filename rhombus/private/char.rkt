#lang racket/base
(require (for-syntax racket/base)
         "provide.rkt"
         "name-root.rkt"
         "define-arity.rkt"
         (submod "annotation.rkt" for-class))

(provide (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Char))

(define-annotation-syntax Char (identifier-annotation #'char? #'()))

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
  (char->integer c))

(define/arity (from_int i)
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
  (char-general-category c))

(define/arity (grapheme_break_property c)
  (char-grapheme-break-property c))

(define/arity (upcase c)
  (char-upcase c))

(define/arity (downcase c)
  (char-downcase c))

(define/arity (foldcase c)
  (char-foldcase c))

(define/arity (titlecase c)
  (char-titlecase c))

(define/arity (grapheme_step c state)
  (char-grapheme-step c state))
