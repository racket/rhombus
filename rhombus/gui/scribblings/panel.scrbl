#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title(~tag: "panels"){Panels and Tabs}

@doc(
  class HPanel():
    implements View
    constructor (
      ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
      ~styles: styles :: MaybeObs.of(List.of(HPanel.Style)) = [],
      ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
      ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
      ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      child :: MaybeObs.of(View), ...
    )
  class VPanel():
    implements View
    constructor (
      ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
      ~styles: styles :: MaybeObs.of(List.of(VPanel.Style)) = [],
      ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
      ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
      ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      child :: MaybeObs.of(View), ...
    )
){

 Creates a panel that arranges the @rhombus(child) views horizontally or
 vertically, respectively.

}

@doc(
  class GroupPanel():
    implements View
    constructor (
      label :: MaybeObs.of(LabelString),
      ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
      ~styles: styles :: MaybeObs.of(List.of(GroupPanel.Style)) = [],
      ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
      ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
      ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      child :: MaybeObs.of(View), ...
    )
){

 Creates a vertical panel that shows grouping under @rhombus(label).

}

@doc(
  class TabsPanel():
    implements View
    constructor (
      choices :: MaybeObs.of(List),
      ~selection: selection :: MaybeObs.of(Any),
      ~action: action :: Function.of_arity(3) = #,(@rhombus(set_selection, ~var)),
      ~choice_to_label: choice_to_label :: Function.of_arity(1) = values,
      ~choice_equal: choice_equal :: Function.of_arity(2) = (_ == _),
      ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
      ~styles: styles :: MaybeObs.of(List.of(TabsPanel.Style)) = [],
      ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
      ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
      ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
      ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
      ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
      child :: MaybeObs.of(View), ...
    )

  property (tabs :: TabsPanel).at_selection :: Obs
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
 @rhombus(LabelString), since @rhombus(choice_to_label) is the identity
 function.

 When the tab selection changes, @rhombus(action) is called as

@rhombusblock(
  action(#,(@rhombus(what, ~var)) :: Any.of(#'select, #'new, #'close, #'reorder),
         choices :: List,
         #,(@rhombus(selected, ~var)) :: maybe(Any))
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
  enum HPanel.Style:
    deleted
    border
    vscroll
    hscroll
    auto_vscroll
    auto_hscroll
    hide_vscroll
    hide_hscroll

  enum VPanel.Style:
    deleted
    border
    vscroll
    hscroll
    auto_vscroll
    auto_hscroll
    hide_vscroll
    hide_hscroll
){

 A panel style option (the same for horizontal and vertical panels).

}

@doc(
  enum GroupPanel.Style:
    deleted
){

 A group panel style option.

}


@doc(
  enum TabsPanel.Style:
    deleted
    no_border
    can_reorder
    can_close
    new_button
    flat_portable

){

 A tab panel style option.

}
