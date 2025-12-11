#lang racket/base
(require rhombus/private/version-case)

(meta-if-version-at-least
 "9.0.0.10" ; proxy for "scribble-lib" version 1.58
 (require scribble/core
          scribble/base)   
 (begin
   (struct cell-padding-property (left top right bottom))
   (define (tabular cells
                    #:style table_style
                    #:pad pad
                    #:sep sep
                    #:column-properties column-properties
                    #:row-properties row-properties
                    #:cell-properties cell-properties
                    #:sep-properties sep-properties)
     (local-require scribble/base)
     (tabular cells
              #:style table_style
              #:sep sep
              #:column-properties column-properties
              #:row-properties row-properties
              #:cell-properties cell-properties
              #:sep-properties sep-properties))))
 
(provide cell-padding-property?
         cell-padding-property
         tabular)


