Rhombus Design Process
======================

.. sectnum::

.. contents:: :depth: 1

Summary
-------

Innovation in the Rhombus project occurs through communication in many different channels -- realtime as well as asychronous, persistent as well as ephemeral. In order to keep track of the working design of any particular component, and options and improvements being considered, along with tradeoffs that have become apparent, and any evaluations or assessment of these options, Rhombus will employ the practice of creating and maintaining *edited* design documents for each aspect of the language, along with minimal process to aid openness and efficiency.

Why?
----

* Trust - an open and simple process with clear criteria fosters trust and encourages contributions.
* Flexibility - documents reflecting the latest and greatest on each topic allow anyone to jump in and get up to speed and contribute at any stage of the process.
* Predictability - minimal process scaffolding ensures that the status of individual components as well as the project as a whole are reasonably clear to everyone.
* Prioritization - predictability expands visibility to longer timelines with more clarity, supporting informed decisionmaking.
* Focus - with the project reified as status documents at varying and visible stages of completion, project members can direct their focus to the most important things at each stage.
* Confidence - with clarity comes confidence, and this eliminates stress on project members that comes with uncertainty and vague demands on their time.
* Quality - Documents revealing variations on each proposal with all the tradeoffs considered and substantiated allow the best combination of options to be selected with confidence, producing better results.
* Efficiency - With focus and prioritization comes efficiency, making the best use of everyone's time, allowing contributors to become involved only when they are truly needed, and in way that can be accommodating of their schedules due to the tractability of the whole process.

The Process
-----------

For a nontrivial addition to the language, work on it begins as an RFC-style document called the *design document*. Discussions on the topic from all channels are integrated into the design document so that it is always current.

Selecting an Editor
~~~~~~~~~~~~~~~~~~~

The design document has an editor.

The first stage of the process is for project members to agree on an editor. Often, there would be a de facto editor who is simply the person that has been leading discussions or championing a particular proposal, but in cases where it is less obvious who the editor should be, someone may simply volunteer, or the group may nominate someone and that person should accept. The only outcome from this stage of the process is *agreement* on who the editor is.

The editor may at any stage of the process abdicate their responsibility to another project member, which can be decided by discussion in any convenient, even private, channel. But any change of editor *must be announced to the group* in a canonical channel.

Maintaining the Document
~~~~~~~~~~~~~~~~~~~~~~~~

The editor then forks the ``rhombus-prototype`` repo and creates a new document at the path ``design/topic/filename.ext``.

Once an editor has been assigned, the design document is maintained *on the editor's fork* until it is ready to merge. That is, any edits to the design document must be made either by the editor or as pull requests against the editor's fork.

Document Format
~~~~~~~~~~~~~~~

The design document should use an open and simple format. Options here include:

* Markdown
* Scribble
* reStructuredText
* Plain text
* Emacs Org

In the interests of minimizing the number of markup formats that contributors need be familiar with, this proposal recommends requiring either Markdown or Scribble.

Review
~~~~~~

As soon as an initial draft is ready -- as early in the process as possible -- the editor starts a draft pull request against the main Rhombus repo.

At this stage, other contributors may review the document (i.e. an HTML rendered version of it) and post review comments on the pull request, similar to code review. There is no need to "approve" or "reject" the pull request, since any decisions on the proposal will be made as a group during Rhombus meetings, rather than via this process of review.

Decisions
~~~~~~~~~

When the document is provisionally complete with respect to the outlined scope, the editor marks the PR as ready to merge. Once this PR is merged, the design document becomes a record of design decisions, and documentation of Rhombus's design, and *the role of the editor ends*. Anyone, including the original editor, may assume the role of editor if the proposal is revisited in the future, and the process at that stage would be the same as it is for fresh proposals. At that stage, the new editor may choose to edit the existing document or start a fresh one, at their discretion and in consultation with team members.

The Design Document
-------------------

The design document follows narrative structure and isn't merely a collection of opinions and discussions but a synthesis of them.

The purpose of the document isn't to propose a particular way of doing things but to showcase different ways of accomplishing the aims of various components of the design and *make recommendations from amongst them*.

Towards this end, the design document focuses on developing each alternative to the best version of itself and does not engage in evaluation of options at too early a stage. Once these alternatives are developed and the full scope of their tradeoffs are better understood, they may even be combined to yield new options with different tradeoffs. Where possible, the document should make recommendations for and against these various alternatives.

Supporting Materials
--------------------

Each component of the design may have a corresponding subfolder in the ``design`` folder in the Rhombus repo. This subfolder includes the design document, and may also include any other supporting materials that inform the recommendations in the design document. These supporting materials may include but are not limited to:

* Code in the form of a proof-of-concept (POC), or several, demonstrating or invalidating aspects of the proposed design
* Any data gathered in the service of claims made in the design document, including surveys, studies, or analyses

Attribution
-----------

The design document should include a contributors section naming anyone who made any kind of contribution to the design. Prior to the PR being merged into the main repo, the contributions to the proposal could be enumerated by component, with the contributors to each of those components listed. This stage should ideally be done in dialogue with other contributors to ensure fair recognition.

Implementing Proposals
----------------------

Once a design is accepted, the role of the editor of the design document ends. The implementation of the design is a distinct phase that is, for the moment, outside the scope of the present document.

Appendix: Communication Channels
--------------------------------

* Discourse
* Discord
* Email
* GitHub Issues / Discussions / Pull Requests
