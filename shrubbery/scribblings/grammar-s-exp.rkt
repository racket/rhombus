#lang at-exp racket/base
(require scribble/bnf
         scribble/manual)

(provide shrubbery_s_expression_grammar)

(define shrubbery_s_expression_grammar
  (BNF (list @nonterm{parsed}
             @racket[(top @#,nonterm{group} ...)])
       (list @nonterm{group}
             @racket[(group @#,nonterm{term} ...)])
       (list @nonterm{term}
             @nonterm{atom}
             @racket[(op @#,nonterm{symbol})]
             @racket[(parens @#,nonterm{group} ...)]
             @racket[(brackets @#,nonterm{group} ...)]
             @racket[(braces @#,nonterm{group} ...)]
             @nonterm{block}
             @racket[(alts @#,nonterm{block} ...)])
       (list @nonterm{block}
             @racket[(block @#,nonterm{group} ...)])))
             

             
             
