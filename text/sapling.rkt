#lang racket/base
(require racket/match
         racket/pretty
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base))

#|

Sapling Notation

Goals:

 - Line-sensitive, but not indentation-snsitive. Code can be indented
   to a proper form based only on lexemes (i.e., without binding
   information).

 - Line-based grouping is a kind of lower bound on parser grouping.
   The same rules that specify indentation also imply groups that
   affect parsing (in the sense of macro expansion). Unlike
   S-expression-inspired conventions, however, the line-based grouping
   rules do not specify all the grouping that is implied by parsing.

   For example, a form with multiple subexpressions does not require
   that each subexpression is on its own line or otherwise explicitly
   grouped; grouping-by-binding applies within a line-based group. To
   ensure that line-based grouping and indentation is not misleading,
   however, candidates for binding-based grouping are restricted to be
   nested within line-based groups.

   This approach means that line breaks are meaningful, and a properly
   indented program reliably tells you something about its syntactic
   structure. At the same time, it doesn't force the programmer to pay
   the tax of making all grouping explicit at the lexeme level when a
   parser (possibly binding-sensitive) would do beter.

 - The syntax is intended look lightweight, but it doesn't avoid
   parentheses at all costs. Furthermore, it allows parentheses to be
   wrapped around any expression, definition, etc. --- unlike some
   indentation-senstive syntaxes, where parentheses tend to turn off
   indentation mode.

Preliminaries:

 - Identifiers are Java-style with alphanumerics and underscores.

 - Operators are sequences of symbolic characters in the sense of
   `char-symbolic?`, but with "\", "#", ",", and ";" removed from the
   set of characters allowed in operators. Also, "//" starts a line
   comment, "/*" starts a block comment, and "*/" ends a block
   comment, so those are not operators. Some operators, including `|`,
   are treated specially as described below, but the special treatment
   for indentation and grouping doesn't preclude their use like other
   operators.

 - Numbers are written in some reasonable way distinct from
   identifiers. Literals are similarly distinct; we use "#true" and
   "#false" for booleans, for example. Keywords are distinct from
   identifier; we use identifiers that end in "#". Strings are written
   in the obvious way with straight doublequotes.

 - No spaces are needed between operators and non-operators, so `1+2`
   and `1 + 2` mean the same thing.

Special syntactic tokens:

 <arrow> = `=>`

 <colon> = `:`

 <opener> = `(` | `[` | `{` | ....
   (All Unicode openers?
    A `{` is soline in the sense that it just creates a group.)

 <closer> = `)` | `]` | `}` | ....
   (All Unicode closers?)

 <bar> = `|`

 <backslash> = `\`

 <comma> = `,`

 <semicolon> = `;`
   (Silent, in the sense that these won't appear in
    the result of `read` as procssed by macros)

 <comment> = `//` up to a newline
           | `/*` up to `*/`
   (Equivalent to whitespace)

Grouping overview:

 * Pairs of <opener> and <closer> contain a sequence of groups. No
   group within the <opener>-<closer> pair extends outisde the pair.

 * A blank line terminates all currently enclosing groups up to the
   enclosing <opener>-<closer> pair. A <comma> or <semicolon> has the
   same effect.

 * A <bar> ends all nested groups up to the previous <bar>, if any,
   within an enclosing <opener>-<closer> pair. It then continues any
   current group that remains open, but starts a nested subgroup. So,
   pairs of <bar> bracket groups in a similar way to
   <opener>-<closer>, but a blank line, <comma>, or <semicolon> is
   stronger.

 * A line-starting <colon> continues the current group from the
   previous line (and does not start a subgroup).

 * A line-ending <arrow> continues the current group into the new
   line, and a line-starting <arrow> continues the current group from
   the previous line. Whether at the start, end, or middle of a line,
   an <arrow> starts a nested sequence of subgroups for the parts
   after the <arrow>.

 * Nested subgroups are ended by a <closer>, blank line, <comma>,
   <semicolon>, or <bar> --- possibly ending multiple groups at once.

 * Unless continued with a <colon>, <arrow>, or <bar>, each line
   starts a new group.

Grouing and Indentation Detail:

A container (such as the input stream) has a sequence of groups. Every
container has a standard indentation, and the immediate groups of the
container start with the same standard indentation as the enclosing
container.

An <opener> starts a nested container. The standard indentation for
groups in the container matches the position after the opener ---
except in the case that <opener> is a line-ending `{`, in which case
the standard indentation for groups in the container is one level
larger than the enclosing group's standard indentation.

  RATIONALE: If you take a sequence of groups in standard indentation
  and drop it inside an empty <closer> and <opener> pair, it reindents
  to have the same shape as before, but shifted. It's shift to line up
  after the opener or one level nested within a line-ending `{`.

A blank line, <comma>, or <semicolon> ends all open groups and
subgroups in the current container. A container-ending <closer> also
ends all open groups and subgroups in the container.

  RATIONALE: A blank line is a natural group ender, but soemtimes you
  want to write things in a more dense layout, and then a semicolon
  effectively substitutes for a blank line. Making a comma behave the
  same way works nicely with froms like function-call aguments (where
  the parentheses for a function call create a conference of groups).

A line-starting <bar> continues the group of the previous line and is
indented specially: either lined up under the first <bar> that appears
in the group (if there was one) or at or at the group's indentation
(if the <bar> is both line-starting and the first in the group). In
general, the first <bar> in a group determines the group's
<bar>-indentation.

  RATIONALE: Aligning <bar>s looks nice and provides a good way to
  group-by-separator within a form, as in

    match x
    | 1 => "one"
    | 2 => "two"
    | _ => "many"

  or
  
    match x | 1 => "one"
            | 2 => "two"
            | _ => "many"

A line-starting <bar> also creates a nested sequence of subgroups
whose default indentation is one level after the enclosing group's
<bar>-indentation. Each of the new subgroups however, starts in a
mode where <bar> ends the subgroup and continues the enclosing group.

A line-starting <colon> continues the current group and neither starts
a nested subgrop or changes the current group's standard indentation.

  RATIONALE: Forms that have multiple parts will have to choose
  between a way to separate the parts: whitespace, <bar>, <colon>.
  (Using <arrow> only works well for separating a final part.) Just
  use <whitespace> for a small, fixed number of parts, like the
  identifier or function-call form after `define`. Use <bar> for
  clauses that are alternatives on an equal footing. Use <colon> for
  everything else. Since <colon> doesn't start a subgroup, an
  <opener>-<closer> pair will have to be used. For example, `:` is a
  good choice to separate parts in `for` before the body:

    define show_all(l) =>
      for (x => in_list(l))
      => println(x)
         newline()

    define show_combos(l, l2) =>
      for (x => in_list(l))
      :   (x2 => in_list(l2))
      => printf("<~a, ~a>\n", x, x2)

    define list_of_combos(l, l2) =>
      for list
      :   (x => in_list(l))
      :   (x2 => in_list(l2))
      => (x, x2)

An <arrow> continues the current group (even if it is a line-starter)
and starts a nested sequence of subgroups. For a line-ending <arrow>,
each subgroup has a standard indentation that is one step larger than
the current group's indentation. For a line-middle or line-starting
<arrow>, the standard indetation of the subgroups matches the right
edge of the arrow. Within a nested sequence created by an <arrow>, a
<bar> continues to match up with the same group as before the <arrow>.

 RATIONALE: Starting nested groups avoids some parentheses. In
 particular, having a <bar>, blank line, <comma>, <colon>, or
 <closer> end multiple nested groups at once can avoid a pile up of
 closing grouping forms.

 An <arrow> work best as the tail of a form or <bar>-initiated clause
 that start an implicit "begin" or definition context. A programmer
 may pefer to put the arrow at the end, start, or middle of the line,
 and we want all of those choices to work well and mean the same
 thing:

    define approx(x) =
      match x
      | something(v) =>
          printf("got it\n")
          v
      | nothing => 0

    define approx(x) =
      match x
      | something(v)
        => printf("got it\n")
           v
      | nothing => 0

    define approx(x) =
      match x
      | something(v) => printf("got it\n")
                        v
      | nothing => 0

As a special case, when an <arrow> is followed by a line-ending "{",
then the standard indentation of the groups within "{" and "}" is the
indentation that would have been used for the <arrow> subgroups if the
<arrow> had been line-ending.

A line-ending <backslash> continues the current group on the following
line, adding one level of indentation to the group's indentation if no
line-ending <backslash> previously appeared in the group.

  RATIONALE: Sometimes a group doesn't fit on a line. Indenting only
  the first <backslash> continuation avoids rightard drift.

  Line continuation can be particularly important for large arithmetic
  expressions. For example

      my_very_long_identifer_name
      + my_other_long_identifer_name

  is no good with the rules here, so a backslash is needed to
  continue:

      my_very_long_identifer_name \
        + my_other_long_identifer_name

  A C programmer may be tempted to write
  
      (my_very_long_identifer_name
       + my_other_long_identifer_name)

  and the language's parentheses-as-expression handling in this case
  would be responsible to compain that there are two expressions
  within the parentheses.

|#

;; There's lot of room for improvement in this implementation!

(define INDENT 2)

(define-tokens non-terminals (identifier
                              keyword
                              number
                              literal
                              comment
                              whitespace

                              operator
                              arrow-operator
                              colon-operator
                              bar-operator
                              bs-operator

                              opener closer
                              comma-operator
                              semicolon-operator
                              EOF))

(define stx-for-original-property (read-syntax #f (open-input-string "original")))

(define-syntax (token stx)
  (syntax-case stx ()
    [(_ name val)
     (identifier? (syntax name))
     (let ([name (syntax name)])
       (with-syntax ([token-name (datum->syntax
                                  name
                                  (string->symbol
                                   (format "token-~a" (syntax-e name))))]
                     [source-name (datum->syntax name 'source-name)]
                     [start-pos (datum->syntax name 'start-pos)]
                     [end-pos (datum->syntax name 'end-pos)])
         (syntax 
          (token-name 
           (datum->syntax #f val
                                 (list
                                  source-name
                                  (position-line start-pos)
                                  (position-col start-pos)
                                  (position-offset start-pos)
                                  (- (position-offset end-pos)
                                     (position-offset start-pos)))
                                 stx-for-original-property)))))]))
(define-syntax (ttoken stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? (syntax name))
     (syntax (token name 'name))]))

(define-lex-abbrev symbolic
  ;; Note: no "\" or "#"
  (:or "=" "!" "@" "$" "%" "^" "&" "*" "-" "+" "<" ">" "." "?" "/" "~" "'" "`"))

(define (lex source-name)
  (lexer
   [(eof) (ttoken EOF)]
   ["=>"
    (token arrow-operator (string->symbol lexeme))]
   [":"
    (token colon-operator (string->symbol lexeme))]
   [#\| (token bar-operator '\|)]
   [#\\ (token bs-operator '|\|)]
   [(:- (:+ symbolic) #\| "=>" #\: "//" "/*" "*/")
    (token operator (string->symbol lexeme))]
   [(:: #\"
        (:* (:~ #\"))
        #\")
    (token literal lexeme)]
   [(:: "#" (:or "true" "false"))
    (token literal (if (equal? lexeme "#true")
                       #t
                       #f))]
   [(:: (:or alphabetic #\_)
        (:* (:or alphabetic
                 numeric
                 #\_)))
    (token identifier (string->symbol lexeme))]
   [(:: (:* (:or alphabetic
                 numeric
                 #\_))
        #\#)
    (token keyword (string->symbol lexeme))]
   [(:: (char-range #\0 #\9)
        (:* (char-range #\0 #\9))
        (:? (:: #\.
                (:* (char-range #\0 #\9)))))
    (token number (string->number lexeme))]
   [(:or #\( #\[ #\{) (token opener lexeme)]
   [(:or #\) #\] #\}) (token closer lexeme)]
   [(:or #\,) (token comma-operator (string->symbol lexeme))]
   [(:or #\;) (token semicolon-operator (string->symbol lexeme))]
   [(:: #\/ #\/ (:* (:~ #\newline))) (token comment lexeme)]
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (token comment lexeme)]
   [(:+ whitespace) (token whitespace lexeme)]))

(define (p in)
  (port-count-lines! in)
  (let ([lex (lex (object-name in))])
    (let loop ()
      (define v (lex in))
      (if (eq? 'EOF (token-name v))
          null
          (cons v (loop))))))

(define (starts-function-call? l)
  (and (pair? l)
       (pair? (cdr l))
       (eq? 'opener (token-name (cadr l)))
       (equal? "(" (syntax-e (token-value (cadr l))))))

(struct bar-state (indents group-indent group sub-group))

(define (bar->indent bar v default)
  (hash-ref (bar-state-indents bar)
            (syntax->datum (token-value v))
            default))

(define (bar-add-indent ht v c)
  (hash-set (or ht #hash()) (syntax->datum (token-value v)) c))

(struct container (tag
                   [rev-content #:mutable]))

(define (make-container #:tag [tag '#%grp] #:in [in #f])
  (define c (container tag null))
  (when in
    (container-add! in c))
  c)

(define (container-add! c v)
  (set-container-rev-content! c (cons v (container-rev-content c))))

(define (container-unpack c)
  (datum->syntax
   #f
   (cons (container-tag c)
         (container-unpack-content c))))
  
(define (container-unpack-content c)
  (for/list ([i (in-list (reverse (container-rev-content c)))])
    (cond
      [(container? i)
       (container-unpack i)]
      [else i])))

(define (render* l
                 #:reindent? [reindent? #t]
                 #:respace? [respace? #f])
  (define (render* l
                   #:line line
                   #:container container
                   #:parent-group [parent-group container]
                   #:group in-group
                   #:next-group [next-group #f]
                   #:start-line [in-start-line line]
                   #:space-before? [space-before? #f]
                   #:container-indent [container-indent 0]
                   #:group-indent [group-indent 0]
                   #:pending-indent [pending-indent 0]
                   #:bar [in-bar #f]
                   #:operator-continues? [operator-continues? #f]
                   #:continued? [continued? #f]
                   #:pre-arrow-indent [pre-arrow-indent #f]
                   #:stack [stack null])
    (cond
      [(null? l)
       (void)]
      [else
       (define v (car l))
       (define this-line (syntax-line (token-value v)))
       (define new-line? (> this-line line))
       (define new-group? (> this-line (+ line 1)))
       (define start-line (if new-group?
                              this-line
                              in-start-line))
       (define bar (if new-group?
                       #f
                       in-bar))
       (define indent (if new-group?
                          container-indent
                          group-indent))
       (define (get-next-group #:group [group in-group]
                               #:new-line? [new-line? new-line?])
         (if (or new-group?
                 (and new-line?
                      (not (= this-line start-line)))
                 (not group))
             (or next-group
                 (make-container #:in (if new-group?
                                          container
                                          parent-group)))
             group))
       (define (get-current-group)
         (or in-group
             (make-container #:in parent-group)))
       (define (out-column)
         (define-values (l c p) (port-next-location (current-output-port)))
         c)
       (define (closer? l)
         (cond
           [(null? l) #f]
           [else (case (token-name (car l))
                   [(whitespace comment) (closer? (cdr l))]
                   [(CLOSE) #t]
                   [else #f])]))
       (define (next-line l)
         (cond
           [(null? l) this-line]
           [else (case (token-name (car l))
                   [(whitespace comment) (next-line (cdr l))]
                   [else (syntax-line (token-value (car l)))])]))
       (define (next-line? l)
         (not (= (next-line l) this-line)))
       (define (next #:container [container container]
                     #:parent-group [parent-group (if new-group?
                                                      container
                                                      parent-group)]
                     #:group group
                     #:next-group [next-group #f]
                     #:line [line this-line]
                     #:start-line [start-line start-line]
                     #:space-before? [space-before? #t]
                     #:container-indent [container-indent container-indent]
                     #:group-indent [group-indent (if new-group?
                                                      container-indent
                                                      group-indent)]
                     #:pending-indent [pending-indent 0]
                     #:bar bar
                     #:operator-continues? [operator-continues? operator-continues?]
                     #:continued? [continued? continued?]
                     #:pre-arrow-indent [pre-arrow-indent #f]
                     #:stack [stack stack])
         (render* (cdr l)
                  #:container container
                  #:parent-group parent-group
                  #:group group
                  #:next-group next-group
                  #:line line
                  #:start-line start-line
                  #:space-before? space-before?
                  #:container-indent container-indent
                  #:group-indent group-indent
                  #:pending-indent pending-indent
                  #:bar bar
                  #:operator-continues? operator-continues?
                  #:continued? continued?
                  #:pre-arrow-indent pre-arrow-indent
                  #:stack stack))
       (define (push #:group group
                     #:group-indent [group-indent group-indent])
         (vector container-indent
                 group-indent
                 bar
                 operator-continues?
                 continued?
                 container
                 parent-group
                 group
                 stack))
       (define (next/pop closer-str)
         (cond
           [(null? stack)
            (write-string closer-str)
            (next #:space-before? #f
                  #:group in-group
                  #:bar bar)]
           [else
            (match stack
              [(vector container-indent
                       group-indent
                       bar
                       operator-continues?
                       continued?
                       container
                       parent-group
                       group
                       stack)
               (when new-line?
                 (emit-indent #:indent group-indent))
               (write-string closer-str)
               (next #:space-before? (not (closer? (cdr l)))
                     #:container-indent container-indent
                     #:group-indent group-indent
                     #:bar bar
                     #:operator-continues? operator-continues?
                     #:continued? continued?
                     #:container container
                     #:parent-group parent-group
                     #:group group
                     #:stack stack)])]))
       (define (emit-indent #:indent [indent indent])
         (unless (or (not reindent?)
                     (zero? indent))
           (write-string (make-string indent #\space))))
       (define (emit-space)
         (emit-indent #:indent pending-indent)
         (cond
           [(= line this-line)
            (when (and space-before?
                       respace?)
              (write-string " "))]
           [else
            (emit-indent)]))
       (define (ender-block #:new-block? new-block? not-ender-k)
         (emit-space)
         (write-string (symbol->string (syntax-e (token-value v))))
         (define to-line (next-line (cdr l)))
         (define group (get-current-group))
         (container-add! group (token-value v))
         (cond
           [(and (to-line . > . this-line)
                 (not new-group?))
            (define next-group (if new-block?
                                   (make-container #:in group)
                                   group))
            (define next-indent (if new-block?
                                    (+ INDENT indent)
                                    indent))
            (next #:group #f
                  #:parent-group next-group
                  #:space-before? #f
                  #:start-line to-line
                  #:group-indent next-indent
                  #:continued? #f
                  #:bar bar)]
           [else
            (not-ender-k group)]))
       (define (atom value
                     #:new-line? [new-line? new-line?])
         (emit-space)
         (cond
           [(symbol? value)
            (write-string (symbol->string value))]
           [(string? value)
            (write-string value)]
           [(eq? value #t)
            (write-string "#true")]
           [(eq? value #f)
            (write-string "#false")]
           [else
            (write value)])
         (define call? (starts-function-call? l))
         (define next-group (get-next-group #:new-line? new-line?))
         (container-add! next-group (token-value v))
         (next #:space-before? (not call?)
               #:group next-group
               #:bar (and (not new-group?) bar)))
       (case (token-name v)
         [(identifier number keyword literal)
          (atom (syntax-e (token-value v)))]
         [(operator)
          (atom (syntax-e (token-value v))
                #:new-line? (and new-line?
                                 (not operator-continues?)))]
         [(opener)
          (emit-space)
          (define str (syntax-e (token-value v)))
          (write-string str)
          (define sub-indent (cond
                               [(and (equal? str "{")
                                     (next-line? (cdr l)))
                                (if pre-arrow-indent
                                    (+ pre-arrow-indent INDENT)
                                    (+ group-indent INDENT))]
                               [else
                                (out-column)]))
          (define next-group (get-next-group))
          (define tag
            (case str
              [("(") '#%paren]
              [("[") '#%bracket]
              [("{") '#%grp]
              [else '#%open]))
          (define next-container (make-container #:tag tag #:in next-group))
          (next #:space-before? #f
                #:stack (push #:group next-group
                              #:group-indent (or (and (equal? str "{")
                                                      (next-line? (cdr l))
                                                      pre-arrow-indent)
                                                 indent))
                #:container next-container
                #:parent-group next-container
                #:group #f
                #:bar #f
                #:container-indent sub-indent
                #:group-indent sub-indent
                #:operator-continues? (not (equal? str "{"))
                #:continued? #f)]
         [(closer)
          (next/pop (syntax-e (token-value v)))]
         [(bar-operator)
          (cond
            [(and new-line?
                  bar)
             (emit-indent #:indent (bar->indent bar v (bar-state-group-indent bar)))]
            [else (emit-space)])
          (define c (out-column))
          (write-string (symbol->string (syntax-e (token-value v))))
          (define group (if bar
                            (bar-state-group bar)
                            (get-current-group)))
          (container-add! group (token-value v))
          (define sub-group (make-container #:in group))
          (next #:bar (if bar
                          (struct-copy bar-state bar
                                       [sub-group sub-group]
                                       [indents (bar-add-indent
                                                 (bar-state-indents bar)
                                                 v
                                                 (bar->indent bar v c))])
                          (bar-state (bar-add-indent #f v c) group-indent group sub-group))
                #:group sub-group
                #:parent-group group
                #:group-indent (if bar
                                   (+ (bar->indent bar v c) INDENT)
                                   (+ c INDENT))
                #:operator-continues? #f
                #:continued? #f)]
         [(bs-operator)
          (emit-space)
          (write-string (symbol->string (syntax-e (token-value v))))
          (define to-line (next-line (cdr l)))
          (cond
            [(to-line . > . this-line)
             (next #:line to-line
                   #:space-before? #f
                   #:continued? #t
                   #:group in-group
                   #:next-group in-group
                   #:parent-group parent-group
                   #:pending-indent (+ group-indent INDENT)
                   #:bar bar)]
            [else
             (define group (get-current-group))
             (container-add! group (token-value v))
             (next #:group group
                   #:bar bar)])]
         [(colon-operator)
          (ender-block
           #:new-block? #f
           ;; If not ender:
           (lambda (group)
             (next #:group group
                   #:bar bar)))]
         [(arrow-operator)
          (ender-block
           #:new-block? #t
           ;; If not ender:
           (lambda (group)
             (define c (out-column))
             (define sub-group (make-container #:in group))
             (next #:group #f
                   #:parent-group sub-group
                   #:group-indent (+ c 1)
                   #:operator-continues? #f
                   #:continued? #f
                   #:bar bar
                   #:pre-arrow-indent indent)))]
         [(comma-operator semicolon-operator)
          (write-string (symbol->string (syntax-e (token-value v))))
          (define ender? (next-line? (cdr l)))
          (when (eq? 'comma-operator (token-name v))
            (container-add! container (token-value v)))
          (next #:group #f
                #:parent-group container
                #:group-indent (if ender?
                                   container-indent
                                   group-indent)
                #:continued? #f
                #:bar #f)]
         [(comment)
          (emit-space)
          (write-string (syntax-e (token-value (car l))))
          (next #:line (if new-line? (add1 line) line)
                #:start-line (if new-line? (add1 start-line) start-line)
                #:space-before? #f
                #:group in-group
                #:next-group next-group
                #:parent-group parent-group
                #:bar bar
                #:pre-arrow-indent pre-arrow-indent)]
         [(whitespace)
          (define str (syntax-e (token-value v)))
          (define newlines (regexp-replace* #rx"[^\n]" str ""))
          (cond
            [(string=? "" newlines)
             (when (if (= line this-line)
                       (not respace?)
                       (not reindent?))
               (write-string str))]
            [else
             (write-string newlines)
             (unless reindent?
               (write-string (car (regexp-match #rx"[^\n]*$" str))))])
          (next #:line line
                #:space-before? space-before?
                #:group in-group
                #:next-group next-group
                #:parent-group parent-group
                #:pending-indent pending-indent
                #:bar bar
                #:pre-arrow-indent pre-arrow-indent)]
         [else (error "unknown:" v)])]))
  (if (null? l)
      null
      (let ([c (make-container #:tag '#%all)])
        (render* l
                 #:line (sub1 (syntax-line (token-value (car l))))
                 #:container c
                 #:group #f)
        (container-unpack c))))

(define (render in
                #:reindent? [reindent? #t]
                #:respace? [respace? #f]
                #:grouping? [grouping? #f])
  (define l (p in))
  (unless (null? l)
    (define o (open-output-string))
    (port-count-lines! o)
    (define g
      (parameterize ([current-output-port o])
        (render* l
                 #:reindent? reindent?
                 #:respace? respace?)))
    (when (or reindent? respace?)
      (write-string (get-output-string o))
      (when reindent?
        (newline)))
    (when grouping?
      (pretty-write (syntax->datum g)))))

(module+ main
  (require racket/cmdline)

  (define reindent? 'default)
  (define respace? #f)
  (define grouping? #f)

  (define (do-render in)
    (render in
            #:reindent? (if (eq? reindent? 'default)
                            (not grouping?)
                            reindent?)
            #:respace? respace?
            #:grouping? grouping?))

  (command-line
   #:once-each
   [("--reindent") "Show re-indentation"
                   (set! reindent? #t)]
   [("--respace") "Normalize non-leading space, too"
                  (set! respace? #t)]
   [("--group") "Show grouping as S-expression"
                (set! grouping? #t)]
   #:args
   file
   (if (null? file)
       (do-render (current-input-port))
       (for-each (lambda (file)
                   (call-with-input-file*
                    file
                    do-render))
                 file))))
