#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    scribble/rx open    
    meta_label:
      rhombus/rx open)

@(expr.macro
  | 'rhombusp(($g), $arg, ...)':
      'rhombus($g, $arg, ...)'
  | 'rhombusp($g, $arg, ...)':
      'rhombus($g, $arg, ...)')

@(expr.macro 'quickref($space, $op $desc, ...)':
    'tabular(
       [[@hspace(1), @rhombusp($op, ~at $space), @hspace(1), elem($desc)],
        ...]
     )')

@title(~tag: "rx-quickref"){Regexp Quick Reference}

Regexp operators:

@quickref(
  rhombus/rx
  (#,(@rhombus(string, ~var))) @{Match literal string; see @rhombus(#%literal, ~at rhombus/rx)}
  (#,(@rhombus(bytes, ~var))) @{Match literal byte string; see @rhombus(#%literal, ~at rhombus/rx)}
  (#,(@rhombus(pat, ~var)) #,(@rhombus(pat, ~var))) @{Match concatenation of matches; see @rhombus(#%juxtapose, ~at rhombus/rx)}
  (#,(@rhombus(pat, ~var)) ++ #,(@rhombus(pat, ~var))) @{Match concatenation of matches}
  ((#,(@rhombus(pat, ~var)))) @{Same as @rhombus(pat, ~var)}
  ([#,(@rhombus(charset, ~var))])  @{Match any in @rhombus(charset, ~var); see @rhombus(#%brackets, ~at rhombus/rx)}
  (#,(@rhombus(pat, ~var)) || #,(@rhombus(pat, ~var)))  @{Match either @rhombus(pat, ~var)}
  (#,(@rhombus(pat, ~var)) *)   @{Zero or more repetitions of @rhombus(pat, ~var)}
  (#,(@rhombus(pat, ~var)) +)   @{One or more repetitions of @rhombus(pat, ~var)}
  (#,(@rhombus(pat, ~var)) ?)   @{Zero or one matches of @rhombus(pat, ~var)}
  (#,(@rhombus(pat, ~var)) {#,(@rhombus(min, ~var)) .. #,(@rhombus(max, ~var))}) \
  @{@rhombus(min, ~var) to @rhombus(max-1, ~var) repetitions of @rhombus(pat, ~var); see @rhombus(#%comp, ~at rhombus/rx)}
  (#,(@rhombus(pat, ~var)) {#,(@rhombus(min, ~var)) ..= #,(@rhombus(max, ~var))}) \
  @{@rhombus(min, ~var) to @rhombus(max, ~var) repetitions of @rhombus(pat, ~var); see @rhombus(#%comp, ~at rhombus/rx)}
  .   @{Any non-newline character}
  any @{Any character}
  char @{Like @rhombus(any, ~at rhombus/rx), but implies string mode}
  byte @{Like @rhombus(any, ~at rhombus/rx), but implies byte-string mode}
  bof  @{Beginning of input (i.e., ``file'')}
  eof  @{End of input (i.e., ``file'')}
  bol  @{Beginning of a line}
  eol  @{End of a line}
  ($ #,(@rhombus(id, ~var)): #,(@rhombus(pat, ~var)))  @{Capture group, set @rhombus(id, ~var) to match for @rhombus(pat, ~var)}
  ($ #,(@rhombus(id, ~var)))  @{Backreference, match same as @rhombus(id, ~var)}
  ($ #,(@rhombus(int, ~var)))  @{Backreference, match same as @rhombus(int, ~var)}
  ($ #,(@rhombus(expr, ~var)))  @{Splice: match pattern produced by @rhombus(expr, ~var)}
  (~~ #,(@rhombus(pat, ~var)))  @{Anonymous capture group match @rhombus(pat, ~var)}
  (lookahead(#,(@rhombus(pat, ~var)))) @{Match empty if @rhombus(pat, ~var) matches after}
  (lookbehind(#,(@rhombus(pat, ~var)))) @{Match empty if @rhombus(pat, ~var) matches before}
  (! lookahead(#,(@rhombus(pat, ~var)))) @{Match empty if @rhombus(pat, ~var) does not match after}
  (! lookbehind(#,(@rhombus(pat, ~var)))) @{Match empty if @rhombus(pat, ~var) does match before}
  word_boundary @{Match empty outside of word}
  word_continue @{Match empty within word}
  (if #,(@rhombus(tst, ~var)) | #,(@rhombus(pat, ~var)) | #,(@rhombus(pat, ~var))) @{Condition on lookahead, lookbehind, or backreference}
  cut @{Match empty, but limit backtracking}
  (string: #,(@rhombus(pat, ~var))) @{Specify string mode, match @rhombus(pat, ~var)}
  (bytes: #,(@rhombus(pat, ~var))) @{Specify byte-string mode, match @rhombus(pat, ~var)}
  (case_sensitive: #,(@rhombus(pat, ~var))) @{Match @rhombus(pat, ~var) case-sensitively}
  (case_insensitive: #,(@rhombus(pat, ~var))) @{Match @rhombus(pat, ~var) case-insensitively} 
)

Character sets that can be used directly as regexp operators:

@quickref(
  rhombus/rx_charset
  alpha @{ASCII letters @litchar{a}-@litchar{z} and @litchar{A}-@litchar{Z}}
  upper @{ASCII uppercase letters @litchar{A}-@litchar{Z}}
  lower @{ASCII lowercase letters @litchar{a}-@litchar{z}}
  digit @{ASCII digits @litchar{0}-@litchar{9}}
  xdigit @{Hexadecimal digits @litchar{0}-@litchar{9}, @litchar{a}-@litchar{f}, and @litchar{A}-@litchar{F}}
  alnum @{@rhombus(alpha, ~at rhombus/rx_charset) plus @rhombus(digit, ~at rhombus/rx_charset)}
  word @{@rhombus(alnum, ~at rhombus/rx_charset) plus @litchar{_}}
  newline @{Newline (ASCI 10)}
  blank @{Space (ASCI 32) and tab (ASCI 7)}
  space @{Newline (10), return (13), space (32), tab (7), and form feed (12)}
  graph @{ASCII characters that print with ink}
  print @{@rhombus(graph, ~at rhombus/rx_charset) plus @rhombus(space, ~at rhombus/rx_charset)}
  cntrl @{ASCII control character(ASCII 0 through 31)}
  ascii @{ASCII characters (ASCII 0 through 127)}
  latin1 @{Latin-1 characters (Unicode 0 through 255)}
  (unicode.Ll) @{Character in a Unicode general category Ll}
  (unicode.#,(@rhombus(cat, ~var))) @{Character in other Unicode general category...}
)

Character sets that can be used directly as regexp operators:

@quickref(
  rhombus/rx_charset
  (#,(@rhombus(string, ~var))) @{All characters in @rhombus(string, ~var)}
  (#,(@rhombus(bytes, ~var))) @{All bytes in @rhombus(bytes, ~var)}
  ((#,(@rhombus(charset, ~var)))) @{Same as @rhombus(charset, ~var)}
  (#,(@rhombus(charset, ~var)) #,(@rhombus(charset, ~var))) @{Union of @rhombus(charset, ~var)s; see @rhombus(#%juxtapose, ~at rhombus/rx_charset)}
  (#,(@rhombus(charset, ~var)) - #,(@rhombus(charset, ~var))) @{Inclusive range between single-element @rhombus(charset, ~var)s}
  (#,(@rhombus(charset, ~var)) || #,(@rhombus(charset, ~var))) @{Union of @rhombus(charset, ~var)s}
  (#,(@rhombus(charset, ~var)) && #,(@rhombus(charset, ~var))) @{Intersection of @rhombus(charset, ~var)s}
  (#,(@rhombus(charset, ~var)) -- #,(@rhombus(charset, ~var))) @{Difference of @rhombus(charset, ~var)s}
  (! #,(@rhombus(charset, ~var))) @{Inverse of @rhombus(charset, ~var)}
  any @{All characters}
)