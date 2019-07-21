This repository, inspired by the Rust project's RFC process, is
intended to serve as a point of discussion for the design of Racket 2.

Please participate by commenting on pull requests and issues. Please
post RFCs by forking and copying `0000-template.md`, which is based on
the corresponding file in the Rust project.

***
Posts

* [Racket2 possibilities](https://groups.google.com/d/msg/racket-users/HiC7z3A5O-k/XPR2wbSJCQAJ) Matthew Flatt 20-July-2019
* [Racket2 and syntax](https://groups.google.com/d/msg/racket-users/3aIPOGbGgmc/A4HHSbdxAwAJ) Matthew Flatt 15-July-2019


***

> Subject: [Racket2 and syntax](https://groups.google.com/d/msg/racket-users/3aIPOGbGgmc/A4HHSbdxAwAJ)
>
> Date: Jul 15, 2019
>
> From: Matthew Flatt
>
> tl;dr DON'T PANIC 
>
> At RacketCon today, after summarizing the state of work on Racket CS, I 
> recommended that we next explore the possibly of changing to an 
> infix-oriented syntax in "Racket2". 
>
> You can find the recording here: 
>
>  <https://www.youtube.com/watch?v=dnz6y5U0tFs> 
>
> Start at 32:27 for the part about what Racket2 could be. 
>
> I'll produce a text version of the rationale soon. For now, I'd like to 
> offer a few clarifications: 
>
>  * There is no specific proposal for a new syntax, yet. Our next step 
>    will be creating a process to explore a possible new syntax. 
>
>  * The talk does include some initial constraints that might guide the 
>    choice of a syntax. Even that meta level (i.e., the set of 
>    constraints) remains subject to a community process. 
>
>  * `#lang racket` is not going away and will always have its current 
>    parenthesis-oriented syntax. In the same way that Racket still 
>    supports `#lang scheme` and `#lang mzscheme` and even `(module 
>    <name> mzscheme ....)` and even top-level programs, the Racket 
>    compiler and runtime system will always support `#lang racket` 
>    programs. We believe that Racket's `#lang`-based ecosystem makes it 
>    uniquely positioned for trying new language variants while 
>    preserving and building on our past investments. 
>
>  * Any new syntax must specifically preserve Racket-style 
>    language-oriented programming, which means everything from defining 
>    simple pattern-based macros to building whole new `#lang`s with a 
>    smooth path in between. Again, our current macro technology must be 
>    an enabler for a new surface syntax, not a casualty. 
>
> As I hope comes across in the talk, I like the current Racket syntax 
> --- and how could I not, after 24 years of helping to define it? --- 
> and I am aware of many potential technical and social pitfalls that 
> this kind of shift could create. Still, in addition to keeping the core 
> Racket implementation running, I feel obliged to recommend changes that 
> I think would be good for the Racket language and community. We've been 
> lining up some technical solutions for a while. I don't know where the 
> community discussion will lead, but I'm pretty sure it's time to start 
> the conversation. 
>
> Whether or not we eventually decide on a different syntax, the design 
> of Racket2 will require community input and participation. If you want 
> to know more about how we're thinking about that process, see the 
> keynote by Aaron Turon: 
>
>       <https://www.youtube.com/watch?v=xSjk2PdQm5k> 
>
> (We'll have professionally edited videos of all talks available soon.) 
>
> Thanks, 
> Matthew 

