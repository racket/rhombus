#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "panels", ~style: #'toc){Panels}

@local_table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "hpanel", ~style: [#'hidden]){@rhombus(HPanel)}

@centered{@gallery("hpanel")}

@doc(
  ~page
  class gui.HPanel():
    implements WindowChildView
    constructor (
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'center],
      ~styles: styles :: ObsOrValue.of(List.of(HPanel.Style)) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView),
      ...
    )
){

 Creates a panel that arranges @rhombus(child) views horizontally.

}

@doc(
  enum gui.HPanel.Style
  | border
  | vscroll
  | hscroll
  | auto_vscroll
  | auto_hscroll
  | hide_vscroll
  | hide_hscroll
){

 A panel style option (the same for horizontal and vertical panels):

@itemlist(

  @item{@rhombus(#'border): Draw a thin border around the panel (in
  which case the client size of the panel may be less than its total
  size).}

  @item{@rhombus(#'hscroll): The panel includes a horizontal scrollbar,
  and the panel’s own width is not constrained by the widths of its
  children.}

  @item{@rhombus(#'vscroll): The panel includes a vertical scrollbar,
  and the panel’s own height is not constrained by the heights of its
  children.}

  @item{@rhombus(#'auto_hscroll): Like @rhombus(#'hscroll), but the
  horizontal scrollbar disappears when no horizontal scrolling is needed.}

  @item{@rhombus(#'auto_vscroll): Like @rhombus(#'vscroll), but the
  horizontal scrollbar disappears when no horizontal scrolling is needed.}

  @item{@rhombus(#'hide_hscroll): Like @rhombus(#'auto_hscroll), but the
  horizontal scrollbar is not made visible, even when horizontal scrolling
  is possible.}

  @item{@rhombus(#'hide_vscroll): Like @rhombus(#'auto_vscroll), but the
  vertical scrollbar is not made visible, even when vertical scrolling is
  possible.}

)

}

@// ------------------------------------------------------------
@section(~tag: "vpanel", ~style: [#'hidden]){@rhombus(VPanel)}

@centered{@gallery("vpanel")}

@doc(
  ~page
  class gui.VPanel():
    implements WindowChildView
    constructor (
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'center],
      ~styles: styles :: ObsOrValue.of(List.of(VPanel.Style)) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView),
      ...
    )
){

 Creates a panel that arranges @rhombus(child) views vertically.

}

@doc(
  enum gui.VPanel.Style
  | border
  | vscroll
  | hscroll
  | auto_vscroll
  | auto_hscroll
  | hide_vscroll
  | hide_hscroll
){

 A panel style option; see @rhombus(gui.HPanel.Style, ~annot).

}

@// ------------------------------------------------------------
@section(~tag: "group-panel", ~style: [#'hidden]){@rhombus(GroupPanel)}

@centered{@gallery("group-panel")}

@doc(
  ~page
  class gui.GroupPanel():
    implements WindowChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'center],
      ~styles: styles :: ObsOrValue.of(List.of(GroupPanel.Style)) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView),
      ...
    )
){

 Creates a vertical panel that shows grouping under @rhombus(label).

}

@doc(
  enum gui.GroupPanel.Style
){

 A group panel style option. No options are supported, currently.

}

@// ------------------------------------------------------------
@section(~tag: "tabs-panel", ~style: [#'hidden]){@rhombus(TabsPanel)}

@centered{@gallery("tabs-panel") @hspace(1) @(gallery("tabs-panel-no-border").pad(~bottom: 4).drop_baseline(4))}

@doc(
  ~page
  class gui.TabsPanel():
    implements WindowChildView
    constructor (
      choices :: ObsOrValue.of(List),
      ~selection: selection :: ObsOrValue.of(Any),
      ~action: action :: (TabsPanel.Action, List, maybe(Any)) -> ~any = values,
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~align: align :: ObsOrValue.of(View.Alignment) = [#'center, #'center],
      ~styles: styles :: ObsOrValue.of(List.of(TabsPanel.Style)) = [],
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      ~window_callbacks: window_callbacks :: maybe(WindowCallbacks) = #false,
      child :: ObsOrValue.of(WindowChildView && !WindowView),
      ...
    )

  property (tabs :: gui.TabsPanel).at_selection :: Obs
){

 Creates a tab panel where @rhombus(choices) provides the number and
 identity of choices, and @rhombus(selection) determines which of the
 tabs is selected.

 The @rhombus(TabsPanel.at_selection) property returns an observable
 that is updated whenever the panel changes through an action (as also
 reported via @rhombus(action)) or via @rhombus(selection) as an
 observable.

 The @rhombus(choice_to_label) function converts an item in
 @rhombus(choices) to a label to be shown for the tab, and
 @rhombus(choice_equal) defines equality for choice identities. By
 default, @rhombus(choices) is expected to be a list of
 @rhombus(View.LabelString, ~annot), since @rhombus(choice_to_label) is the identity
 function.

 When the tab selection changes, @rhombus(action) is called as

@rhombusblock(
  action(#,(@rhombus(what, ~var)), choices, #,(@rhombus(selected, ~var)))
)

 where @rhombus(what, ~var) describes the action, @rhombus(choices) is
 the list of choices at the time of the action, and
 @rhombus(selected, ~var) is the tab (if any) selected after the action.

 To change the content of a @rhombus(TabsPanel, ~class) based on its
 selection, supply a @rhombus(child) that is an observable derived from
 one supplied as @rhombus(selection).

}

@doc(
  enum gui.TabsPanel.Style
  | no_border
  | can_reorder
  | can_close
  | new_button
  | flat_portable

){

 A tab panel style option.

@itemlist(

  @item{@rhombus(#'no_border): No border is drawn around the panel
  content. A borderless tab panel typically has a different look for its
  tabs, too.}

  @item{@rhombus(#'can_reorder): Allows the user to reorder tabs by
  dragging them.}

  @item{@rhombus(#'can_close): Allows the user to close tabs by clicking
  a close icon on a tab.}

  @item{@rhombus(#'new_button): Include a button to create a new tab, if
  supported. A tab-creation button is always available with
  @rhombus(#'flat_portable).}

  @item{@rhombus(#'flat_portable): Use a platform-independent
  implementation of the tab control, which provides more consistent
  functionality and style across platforms.}

)

}

@doc(
  enum gui.TabsPanel.Action
  | select
  | new
  | close
  | reorder
){

 A tab panel action provided to the @rhombus(~action) callback function
 of a @rhombus(Panel, ~class).

}
