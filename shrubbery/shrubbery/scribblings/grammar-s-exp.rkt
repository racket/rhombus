#lang at-exp racket/base
(require scribble/bnf
         scribble/manual)

(provide shrubbery_s_expression_grammar)

(define shrubbery_s_expression_grammar
  (BNF (list @nonterm{document}
             @racket[(top @#,nonterm{group} ...)])
       (list @nonterm{group}
             @racket[(group @#,nonterm{item} ... @#,nonterm{item})]
             @racket[(group @#,nonterm{item} ... @#,nonterm{block})]
             @racket[(group @#,nonterm{item} ... @#,nonterm{alts})]
             @racket[(group @#,nonterm{item} ... @#,nonterm{block} @#,nonterm{alts})])
       (list @nonterm{item}
             @nonterm{atom}
             @racket[(op @#,nonterm{symbol})]
             @racket[(parens @#,nonterm{group} ...)]
             @racket[(brackets @#,nonterm{group} ...)]
             @racket[(braces @#,nonterm{group} ...)]
             @racket[(quotes @#,nonterm{group} ...)])
       (list @nonterm{block}
             @racket[(block @#,nonterm{group} ...)])
       (list @nonterm{alts}
             @racket[(alts @#,nonterm{block} ...)])))
             

             
             
