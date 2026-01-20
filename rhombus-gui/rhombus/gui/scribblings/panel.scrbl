#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title(~tag: "panels"){Panels and Tabs}

@doc(
  class gui.HPanel():
    satisfies WindowChildView
    constructor (
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~styles: styles :: ObsOrValue.of(List.of(HPanel.Style)) = [],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      child :: ObsOrValue.of(View), ...
    )
  class gui.VPanel():
    implements WindowChildView
    constructor (
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~styles: styles :: ObsOrValue.of(List.of(VPanel.Style)) = [],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      child :: ObsOrValue.of(View), ...
    )
){

 Creates a panel that arranges the @rhombus(child) views horizontally or
 vertically, respectively.

}

@doc(
  class gui.GroupPanel():
    satisfies WindowChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~styles: styles :: ObsOrValue.of(List.of(GroupPanel.Style)) = [],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      child :: ObsOrValue.of(View), ...
    )
){

 Creates a vertical panel that shows grouping under @rhombus(label).

}

@doc(
  class gui.TabsPanel():
    satisfies WindowChildView
    constructor (
      choices :: ObsOrValue.of(List),
      ~selection: selection :: ObsOrValue.of(Any),
      ~action: action :: maybe((TabsPanel.Action, List, maybe(Any)) -> Any)
                 = #,(@rhombus(set_selection, ~var)),
      ~choice_to_label: choice_to_label :: Any -> Any = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~alignment: alignment :: ObsOrValue.of(View.Alignment) = [#'center, #'top],
      ~styles: styles :: ObsOrValue.of(List.of(TabsPanel.Style)) = [],
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~spacing: spacing :: ObsOrValue.of(View.SpacingInt) = 0,
      ~margin: margin :: ObsOrValue.of(View.Margin) = [0, 0],
      ~min_size: min_size :: ObsOrValue.of(View.Size) = [#false, #false],
      ~stretch: stretch :: ObsOrValue.of(View.Stretch) = [#true, #true],
      child :: ObsOrValue.of(View), ...
    )

  property (tabs :: gui.TabsPanel).at_selection :: Obs
){

 Creates a tab panel where @rhombus(choices) provides the number and
 identity of choices, and @rhombus(selection) determines which of the
 tabs is selected.

 If @rhombus(selection) is not an observable, then an observable
 @rhombus(at_selection, ~var) is created with initial value
 @rhombus(selection). Otherwise, @rhombus(at_selection, ~var) is
 @rhombus(selection). A observable derived from
 @rhombus(at_selection, ~var) can be obtained from the
 @rhombus(TabsPanel.at_selection) property.

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
 The default @rhombus(set_selection, ~var) function corresponds to

@rhombusblock(
  fun (_, _, selected):
    #,(@rhombus(at_selection, ~var)).value := selected
)

 To change the content of a @rhombus(TabsPanel, ~class) based on its
 selection, supply a @rhombus(child) that is an observable derived from
 one supplied as @rhombus(selection).

}


@doc(
  enum gui.HPanel.Style
  | deleted
  | border
  | vscroll
  | hscroll
  | auto_vscroll
  | auto_hscroll
  | hide_vscroll
  | hide_hscroll

  enum gui.VPanel.Style
  | deleted
  | border
  | vscroll
  | hscroll
  | auto_vscroll
  | auto_hscroll
  | hide_vscroll
  | hide_hscroll
){

 A panel style option (the same for horizontal and vertical panels).

}

@doc(
  enum gui.GroupPanel.Style
  | deleted
){

 A group panel style option.

}


@doc(
  enum gui.TabsPanel.Style
  | deleted
  | no_border
  | can_reorder
  | can_close
  | new_button
  | flat_portable

){

 A tab panel style option.

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
