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
   comment and "/*" starts a block comment, so those are not
   operators. Some operators, including `|`, are treated specially as
   described below, but the special treatmen for indentation and
   grouping doesn't preclude their use like other operators.

 - Numbers are written in some reasonable way distinct from
   identifiers. Literals are similarly distinct; we use "#true" and
   "#false" for booleans, for example. Keywords are distinct from
   identifier; we use identifiers that end in "#". Strings are written
   in the obvious way with straight doublequotes.

 - No spaces are needed between operators and non-operators, so `1+2`
   and `1 + 2` mean the same thing.

 - Function calls, recognized having no white space between an
   expression and open parenthesis, are distinct from other forms at
   the reader level. Checking for the proper use of commas for
   arguments, however is up to the consumer of a grouped stream.

Special syntactic tokens:

 <arrow> = `=>` | `==>` | `=->` ....
   (Any operator that starts "=" and ends ">".)

 <equal> = `=` | `:` | `?=` | `?:` ...
   (Operators that end in "=" or ":".)

 <opener> = `(` | `[` | `{` | ....
   (All Unicode openers?)

 <closer> = `)` | `]` | `}` | ....
   (All Unicode closers?)

 <conj> = `|` | `&`

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

 * A <conj> ends all nested groups up to the previous <conj>, if any,
   within an enclosing <opener>-<closer> pair. It then continues any
   current group that remains open. So, pairs of <conj> bracket groups
   in a similar way to <opener>-<closer>, but a blank line, <comma>,
   or <semicolon> is stronger.

 * A line-ending <equal> or line-ending/middle/starting <arrow>
   continues the current group into the new line, but starts a nested
   sequence of subgroups for the parts after the <equal> or <arrow>.
   Nested subgruped are ended by a <closer>, blank line, <comma>,
   <semicolon>, or <conj> --- possibly ending multiple groups at once.

 * Unless continued with a <equal>, <arrow>, or <conj>, each line
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

A line-starting <conj> continues the group of the previous line and is
indented specially: either lined up under the first same-shaped <conj>
that appears in the group (if there was one) or at or at the group's
indentation (if the <conj> is both line-starting and the first
same-shaped <conj> in the group). In general, the first <conj> in a
group determines the group's <conj>-indentation for that <conj> shape.

 RATIONALE: Aligning <conj>s looks nice and provides a good way to
 group-by-separator within a form, as in

    match x
    | 1 => "one"
    | 2 => "two"
    | _ => "many"

  or
  
    match x | 1 => "one"
            | 2 => "two"
            | _ => "many"

  Having two <conj> shapes is useful for multi-part forms like `let`,
  which has a sequence of bindings followed by a sequence of
  definitions and expressions.

    let | x = 8
        | y = 9
    & show(x + y)
      x-y

A line-starting <conj> also creates a nested sequence of subgroups
whose default indentation is one level after the enclosing group's
<conj>-indentation. Each of the new subgroups however, starts in a
mode where <conj> ends the subgroup and continues the enclosing group.

A line-ending <equal> or line-ending <arrow> operator starts a nested
sequence of subgroups, where each subgroup has a standard indentation
that is one step larger than the current group's indentation. A
line-starting or line-middle <arrow> similarly extends the current
group while starrting a nested sequence of subgroups, where the
subgroup indentation is one space after the <arrow> end. Within a
nested sequence created by an <equal> or <arrow>, a <conj> continues
to match up with the same group as before the <equal> or <arrow>.

 RATIONALE: Starting nested groups avoids some parentheses. In
 particular, having a <conj>, blank line, <comma>, <colon>, or
 <closer> end multiple nested groups at once can avoid a pile up of
 closing grouping forms.

 We treat <equal> and <arrow> differently to offer two grouping
 mechanisms --- one that always starts a new subgrup (the <arrow>s)
 and one that only starts a subgroup if it ends a line (the <equal>s)
 --- because the each option works better than the other in certain
 cases. For example, <equal> works best for definitions, so that
 short definitions can be written without line breaks:

     define pi = 3.14
     define tau = 2 * pi

 In contrast, <arrow> works better for cases that start an implicit
 "begin" or definition context, where may look best o have the arrow
 at the end, start, or middle of the line, and it's better if those
 don't mean different things:

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
                              equal-operator
                              conj-operator
                              bs-operator

                              open close
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
  (:or "=" "!" "@" "$" "%" "^" "&" "*" "-" "+" "<" ">" "." "?" "/" "~" "'" "`"))

(define (lex source-name)
  (lexer
   [(eof) (ttoken EOF)]
   [(:: "="
        (:* symbolic)
        ">")
    (token arrow-operator (string->symbol lexeme))]
   [(:: (:* (:- symbolic (:or "<" ">")))
        (:or ":" "="))
    (token equal-operator (string->symbol lexeme))]
   [(:- (:+ symbolic) #\| #\&) (token operator (string->symbol lexeme))]
   [#\| (token conj-operator '\|)]
   [#\& (token conj-operator '\&)]
   [#\\ (token bs-operator '|\|)]
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
   [(:or #\( #\[ #\{) (token open lexeme)]
   [(:or #\) #\] #\}) (token close lexeme)]
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
       (eq? 'open (token-name (cadr l)))
       (equal? "(" (syntax-e (token-value (cadr l))))))

(struct conj-state (indents group-indent group sub-group))

(define (conj->indent conj v default)
  (hash-ref (conj-state-indents conj)
            (syntax->datum (token-value v))
            default))

(define (conj-add-indent ht v c)
  (hash-set (or ht #hash()) (syntax->datum (token-value v)) c))

(struct container (tag
                   [rev-content #:mutable]
                   [pending-call? #:mutable]))

(define (make-container #:tag [tag '#%grp] #:in [in #f])
  (define c (container tag null #f))
  (when in
    (container-add! in c))
  c)

(struct call (rator rands))

(define (container-add! c in-v #:call? [call? #f])
  (define v (cond
              [(container-pending-call? c)
               (set-container-pending-call?! c #f)
               (define a (car (container-rev-content c)))
               (set-container-rev-content! c (cdr (container-rev-content c)))
               (call a in-v)]
              [else in-v]))
  (set-container-rev-content! c (cons v (container-rev-content c)))
  (when call?
    (set-container-pending-call?! c #t)))

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
      [(call? i)
       (datum->syntax #f `(#%call ,(call-rator i) . ,(container-unpack-content (call-rands i))))]
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
                   #:conj [in-conj #f]
                   #:continued? [continued? #f]
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
       (define conj (if new-group?
                       #f
                       in-conj))
       (define indent (if new-group?
                          container-indent
                          group-indent))
       (define (get-next-group #:group [group in-group])
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
                     #:conj conj
                     #:continued? [continued? continued?]
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
                  #:conj conj
                  #:continued? continued?
                  #:stack stack))
       (define (push #:group group)
         (vector container-indent
                 group-indent
                 conj
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
                  #:conj conj)]
           [else
            (match stack
              [(vector container-indent
                       group-indent
                       conj
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
                     #:conj conj
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
       (define (ender-block not-ender-k)
         (emit-space)
         (write-string (symbol->string (syntax-e (token-value v))))
         (define to-line (next-line (cdr l)))
         (define group (get-current-group))
         (container-add! group (token-value v))
         (cond
           [(and (to-line . > . this-line)
                 (not new-group?))
            (define next-group (make-container #:in group))
            (define next-indent (+ INDENT indent))
            (next #:group #f
                  #:parent-group next-group
                  #:space-before? #f
                  #:start-line to-line
                  #:group-indent next-indent
                  #:continued? #f
                  #:conj conj)]
           [else
            (not-ender-k group)]))
       (define (atom value)
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
         (define next-group (get-next-group))
         (container-add! next-group (token-value v) #:call? call?)
         (next #:space-before? (not call?)
               #:group next-group
               #:conj (and (not new-group?) conj)))
       (case (token-name v)
         [(identifier number keyword literal operator)
          (atom (syntax-e (token-value v)))]
         [(open)
          (emit-space)
          (define str (syntax-e (token-value v)))
          (write-string str)
          (define sub-indent (if (and (equal? str "{")
                                      (next-line? (cdr l)))
                                 (+ group-indent INDENT)
                                 (out-column)))
          (define group (get-current-group))
          (define next-group (get-next-group #:group group))
          (define tag
            (case str
              [("(") '#%paren]
              [("[") '#%bracket]
              [("{") '#%brace]
              [else '#%open]))
          (define next-container (make-container #:tag tag #:in next-group))
          (next #:space-before? #f
                #:stack (push #:group group)
                #:container next-container
                #:parent-group next-container
                #:group #f
                #:conj #f
                #:container-indent sub-indent
                #:group-indent sub-indent
                #:continued? #f)]
         [(close)
          (next/pop (syntax-e (token-value v)))]
         [(conj-operator)
          (cond
            [(and new-line?
                  conj)
             (emit-indent #:indent (conj->indent conj v (conj-state-group-indent conj)))]
            [else (emit-space)])
          (define c (out-column))
          (write-string (symbol->string (syntax-e (token-value v))))
          (define group (if conj
                            (conj-state-group conj)
                            (get-current-group)))
          (container-add! group (token-value v))
          (define sub-group (make-container #:in group))
          (next #:conj (if conj
                          (struct-copy conj-state conj
                                       [sub-group sub-group]
                                       [indents (conj-add-indent
                                                 (conj-state-indents conj)
                                                 v
                                                 (conj->indent conj v c))])
                          (conj-state (conj-add-indent #f v c) group-indent group sub-group))
                #:group sub-group
                #:parent-group group
                #:group-indent (if conj
                                   (+ (conj->indent conj v c) INDENT)
                                   (+ c INDENT))
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
                   #:conj conj)]
            [else
             (define group (get-current-group))
             (container-add! group (token-value v))
             (next #:group group
                   #:conj conj)])]
         [(equal-operator)
          (ender-block
           ;; If not ender:
           (lambda (group)
             (next #:group group
                   #:conj conj)))]
         [(arrow-operator)
          (ender-block
           ;; If not ender:
           (lambda (group)
             (define c (out-column))
             (define sub-group (make-container #:in group))
             (next #:group #f
                   #:parent-group sub-group
                   #:group-indent (+ c 1)
                   #:continued? #f
                   #:conj conj)))]
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
                #:conj #f)]
         [(comment)
          (emit-space)
          (write-string (syntax-e (token-value (car l))))
          (next #:line (if new-line? (add1 line) line)
                #:start-line (if new-line? (add1 start-line) start-line)
                #:space-before? #f
                #:group in-group
                #:next-group next-group
                #:parent-group parent-group
                #:conj conj)]
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
                #:conj conj)]
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

  (define reindent? #f)
  (define respace? #f)
  (define grouping? #f)

  (define (do-render in)
    (render in
            #:reindent? reindent?
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
