Rhombus Design Process Guidelines
=================================

This document describes the design process used to streamline innovation in the Rhombus project. The process is oriented around design documents and accompanying prototypes.

Motivation
----------

Innovation in the Rhombus project occurs through communication in many different channels -- realtime as well as asychronous, persistent as well as ephemeral. Ideally, discussion is posted at [the Rhombus repository](https://github.com/racket/rhombus-prototype) (as described in the [README](https://github.com/racket/rhombus-prototype/blob/master/README.md)), but requiring all discussion to be there is not a realistic goal. Even if all discussion were centralized, making potential contributors hunt through channels and reconstruct discussion threads to understand a design creates a substantial obstacle to contribution.

Summary
-------

To help everyone keep track of a design, including people who may be new to an ongoing discussion, Rhombus employs the practice of creating and maintaining *edited design documents* for each aspect of the language. These documents reflect and inform *accompanying prototypes*. Rhombus also institutes minimal process to aid openness and efficiency.

The Process
-----------

### Overview

For a nontrivial addition to the language, work likely starts out as an informal discussion or a contributor's worked-out idea. Ideally, an initial design should be implemented in a prototype as soon as possible, because having an implementation aids communication and discussion. Some ideas may be worked out enough to merit a design document in advance of the implementation. Either way, a design document and any associated implementation is posted to the Rhombus repo as a pull request. Work on the design proceeds in connection with this pull request, guided by the initial contributor or another designated editor, until it is eventually merged.

For small additions to Rhombus, following the design process is not required, in favor of simply creating a pull request with the necessary changes to the Rhombus implementation and documentation.

Note also that more speculative ideas or options may be suitable for posting as [issues](https://github.com/racket/rhombus-prototype/issues) instead of, or as a precursor to, following the design process documented here.

### The Editor

Every design has an editor. Often, the de facto editor is the person that has been leading discussions or championing a particular proposal. In cases where it is less obvious who the editor should be, someone may volunteer, or the group may nominate someone. The only outcome from this stage of the process is *agreement* on who the editor is.

The editor is typically interested and knowledgeable about the topic, and their role is to shepherd the proposal through to its conclusion in collaboration with other project members. Contributors often take on the role of editor when they have a vision for that particular component of the language, and are interested in delivering on that vision with the help of other project members. Being an editor is a creative and a guiding role, but it equally entails the responsibility to synthesize ongoing discussions into the design document and the prototype as needed. This ensures that the document is informed by multiple perspectives and that the available alternatives are effectively developed through collaboration.

If at any stage of the process the editor is unable to fulfill their role, they may choose to abdicate their responsibility to another project member, which can be decided by discussion in any convenient, even private, channel. But any change of editor *must be announced to the group* in a canonical channel.

### Working on the Design

The editor first forks the [`rhombus-prototype` repository](https://github.com/racket/rhombus-prototype), starts a branch to track the new design, creates a new document at the path `design/topic/filename.ext` on this branch, and then starts a draft pull request to add it back to the main repository. This pull request should be started as early as possible, even though the document may only be an early draft or placeholder, as it lays the foundation for open collaboration on the design.

Initially, the design document and any prototypes are maintained *on the editor's fork* until they are ready to be merged. That is, any modifications to the design must be made either by the editor or as pull requests against the editor's fork. Contributors may also perform reviews of the working design in the form of comments on the upstream pull request.

Once a design document and its prototype have evolved to the point that they seem likely components of the Rhombus design, the upstream pull request can be merged into the *Rhombus repo's main branch*. Discussion proceeds in this second phase as in the first. The switch from being a pull request (with recommended changes as part of the request) to a merged change (with recommended changes perhaps as new pull requests) simply reflects that different tools seem better suited to the earlier phase versus a later phase.

The Design Document
-------------------

### Purpose

The purpose of the document in the early stages is to propose and develop alternatives accomplishing the aims of the design, while making recommendations from these alternatives. In later stages, as these alternatives become more well-specified and the recommendations are accepted, the document naturally serves as a record of the design process and as rationale for the accepted design. The editor should ensure that the finished product reflects its role for posterity -- that is, if necessary, the verbiage should be modified from "Option B is recommended due to X and Y" to "Option B was accepted due to X and Y."

### Content

At the outset, the document should summarize the proposed (or accepted) design. The rest of the document should showcase different ways of accomplishing the design's aims as have been discussed, and relate them to the consensus choices that are reflected in the accepted or proposed design, motivating why those choices are warranted or were made. The document must synthesize and organize the record of discussion in these terms.

The style of the document is left to the discretion of the editor as long as it meets the goals outlined here. For convenience, two sample templates are included with the present document to give an idea of what the document should accomplish. The first of these presents the design in terms of its various components, and the options within those components are discussed at length, with recommendations made from amongst those options. The second presents a series of alternative, complete, designs, with a recommendation made from these alternatives. Either style may be more appropriate depending on the type of design being discussed (for instance, the former may be more appropriate when the design ostensibly has well-defined components that would be common to all alternatives, while the latter may be more appropriate when the alternative designs have distinct components), and once again, this is left to the discretion of the editor.

At the point where a design becomes wholly integrated into the Rhombus documentation, the original design document need no longer be maintained, but it should be preserved in perpetuity as a record of the design process and the decisions that were made.

### Format

The design document should use Markdown or Scribble format. In either case, a link to a rendered HTML version or instructions to render it to HTML locally could optionally be included in the PR description, for the reader's convenience.

The Prototype
-------------

In the early stages of the process, the design may use a `poc` subfolder in the `design/<topic>/` folder in the Rhombus repo to implement standalone proofs-of-concept, including full prototypes that implement the design. In later stages of the process when there is broad agreement on the general direction of the proposal, the prototype could take the form of changes to the Rhombus implementation and documentation to incorporate the design into the language.

Supporting Materials
--------------------

The `design/<topic>` folder may also include additional supporting material such as data gathered in the service of claims made in the design document, surveys, studies, or analyses of any kind.

Attribution
-----------

The design document should include a "Contributors" section naming anyone who made any kind of contribution to the design. Prior to the PR being merged into the main repo, the contributions to the proposal should be enumerated by component, with the contributors to each of those components listed. This stage should be done in dialogue with other contributors to ensure fair recognition.

Appendix: Group Communication Channels
--------------------------------------

* [The Rhombus Repository](https://github.com/racket/rhombus-prototype) (e.g. discussions, issues, and pull requests)
* [The Racket Discourse](https://racket.discourse.group/)
* [Racket Discord](https://discord.gg/6Zq8sH5)
