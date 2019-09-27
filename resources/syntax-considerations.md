# Motivation
[motivation]: #motivation

This document is meant to facilitate the discussion around syntactic principles by providing a record of considerations that have been brought up.  The intent is to be usable both as a checklist to help in discussing specific proposals and also as a starting point for developing and refining principles in the future.


# Considerations List
[considerations-list]: #considerations-list

* familiar expectations
  * US high school algebra
    * infix operators
    * algebra-style function notation
    * parens can be used for grouping
* flexible
* clean / not "noisy"
* dependent on advanced tooling/editor support
* easy to rearrange chunks of code
* distinctive
* tree resemblance
* uniformity vs texture
* macros / language extensions feel natural next to existing forms
* syntax leads to "syntax patterns" and "syntax templates" that are easy to use for macro writers
* should be easily readable and editable outside of "your favorite editor"
  - users (beginners in particular) should not need to learn keybindings or advanced editor features to effectively edit code. Editing code is not easy if it requires you to use `paredit`, for instance.
  - users (beginners in particular) should be able to read code in various environments, like GitHub and codeblock embeded in Markdown. Matching parens is not easy if it requires you to use rainbow parens, for instance.

∑∑