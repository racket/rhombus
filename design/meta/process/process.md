Rhombus Design Recording Guidelines
===================================

Innovation in the Rhombus project occurs through communication in many different channels -- realtime as well as asychronous, persistent as well as ephemeral. Ideally, discussion is posted at [https://github.com/racket/rhombus-prototype](https://github.com/racket/rhombus-prototype) (as described in the [README](https://github.com/racket/rhombus-prototype/blob/master/README.md)), but requiring all discussion to be there is not realistic goal. Even if all discussion were centralized, making potential contributors hunt through channels and reconstruct discussion threads to understand a design creates a substantial obstacle to contribution. To help everyone keep track of a design, including people who may be new to an ongoing discussion, Rhombus will employ the practice of creating and maintaining *edited* discussion summaries for each aspect of the language, along with minimal process to aid openness and efficiency.

The Process
-----------

For a nontrivial addition to the language, work likely starts out as an informal discussion or a contributor's worked-out idea. Ideally, an initial design should be implemented in a prototype as soon as possible, because having an implementation aids communication and discussion. Some ideas may be worked out enough to merit a design document in advance of the implementation. Either way, a design document and any associated implementation can be posted to [https://racket/rhombus-prototype](https://racket/rhombus-prototype) as a pull request. (Note that more speculative ideas or options can be suitable for posting as an [issue](https://github.com/racket/rhombus-prototype/issues).)

Every design has an editor. Often, the de facto editor is the person that has been leading discussions or championing a particular proposal. In cases where it is less obvious who the editor should be, someone may volunteer, or the group may nominate someone. The only outcome from this stage of the process is *agreement* on who the editor is.

The editor forks the `rhombus-prototype` repo and creates a new document at the path `design/topic/filename.ext` -- using Markdown format or Scribble (rendered to HTML or Markdown). Initially, the design document is maintained *on the editor's fork* until it is ready to merge. That is, any edits to the design document must be made either by the editor or as pull requests against the editor's fork.

Once a design document and its prototype have evolved to the point that they seem likely components of the Rhombus design, the pull request can be merged into the *Rhombus repo's main branch*. Discussion proceeds in this second phase as in the first. The switch from being a pull request (with recommended changes as part of the request) to a merged change (with recommended changes perhaps as new pull requests) simply reflects that different tools seem better suited to the earlier phase versus a later phase.

An editor may at any time abdicate their responsibility to another project member, which can be decided by discussion in any convenient, even private, channel. But any change of editor *must be announced to the group* in a canonical channel.

The Design Document
-------------------

The purpose of the document is to propose a particular way of doing things. In its discussion summary, a design document should showcase different ways of accomplishing the design's aims as have been discussed, and the summary should relate them to the consensus choices that are reflected in the design document. An editor is expected to both participate in dicussion and also synthesize and organize the record of discussion. Some alternative designs may be worth mentioning as part of a design's description, but as much as possible the disucssion summary as an appendix should be a complete and accurate reflection of the discussion.

At the point where a design becomes wholly integrated into the Rhombus documentation, the original design document may become redundant except for the discussion summary. A redundant design description can be removed (replaced with a reference to documentation sections), but the discusssion summary should be preserved and maintained in perpetuity.

Supporting Materials
--------------------

The pull request for an implementation may include changes to the Rhombus implementation and documentation. For additional code and material, the design may have a corresponding subfolder in the `design` folder in the Rhombus repo. For example, supporting material in the subfolder may include data gathered in the service of claims made in the design document, such as surveys, studies, or analyses.

Attribution
-----------

The design document should include a contributors section naming anyone who made any kind of contribution to the design. Prior to the PR being merged into the main repo, the contributions to the proposal could be enumerated by component, with the contributors to each of those components listed. This stage should ideally be done in dialogue with other contributors to ensure fair recognition.

Appendix: Communication Channels
--------------------------------

* GitHub Issues / Discussions / Pull Requests
* Discourse
* Discord
* Email
