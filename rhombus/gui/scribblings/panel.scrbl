#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title(~tag: "panels"){Panels and Tabs}

@doc(
  class HPanel():
    implements View
    constructor (~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~style: style :: MaybeObs.of(List.of(HPanel.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 child :: View,
                 ...)
  class VPanel():
    implements View
    constructor (~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~style: style :: MaybeObs.of(List.of(VPanel.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 child :: View,
                 ...)
){

 Creates a panel that arranges the @rhombus(child) views horizontally or
 vertically, respectively.

}

@doc(
  class GroupPanel():
    implements View
    constructor (label :: MaybeObs.of(LabelString),
                 ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~style: style :: MaybeObs.of(List.of(Group.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 child :: View,
                 ...)
){

 Creates a vertical panel that shows grouping under @rhombus(label).

}

@doc(
  class TabsPanel():
    implements View
    constructor (choices :: MaybeObs.of(List),
                 action :: Function,
                 ~choice_to_label: choice_to_label :: Function = values,
                 ~choice_equal: choice_equal :: Function = (fun (a, b): a == b),
                 ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~style: style :: MaybeObs.of(List.of(TabsPanel.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true],
                 child :: View,
                 ...)
){

 Creates a tab vertical panel with @rhombus(choices) providing the
 number and identity of choices. The @rhombus(choice_to_label) function
 converts an item in @rhombus(choices) to a label to be shown for the
 tab, and @rhombus(choice_equal) defines equality for choice identities.
 By default, @rhombus(choices) is expected to be a list of
 @rhombus(LabelString), since @rhombus(choice_to_label) is the identity
 function.

 When the tab selection changes, @rhombus(action) is called as

@rhombusblock(
  action(#,(@rhombus(what, ~var)) :: Any.of(#'select, #'new, #'close, #'reorder),
         choices :: List,
         #,(@rhombus(selected, ~var)) :: Maybe(Any))
)

 where @rhombus(what, ~var) describes the action, @rhombus(choices) is
 the list of choices at the time of the action, and
 @rhombus(selected, ~var) is the tab (if any) selected after the action.

 The @rhombus(View.if) form, @rhombus(View.cond) form, or
 @rhombus(ListPanel) can be useful for building the content of a tab
 based on an observable whose value is updated by @rhombus(action).

}

@doc(
  class ListPanel():
    implements View
    constructor (children :: MaybeObs.of(List),
                 make :: Function,
                 ~key: key :: Function = values,
                 ~alignment: alignment :: MaybeObs.of(Alignment) = [#'center, #'top],
                 ~style: style :: MaybeObs.of(List.of(Group.StyleSymbol)) = [],
                 ~is_enabled: is_enabled :: MaybeObs.of(Boolean) = #true,
                 ~spacing: spacing :: MaybeObs.of(SpacingInteger) = 0,
                 ~margin: margin :: MaybeObs.of(Margin) = [0, 0],
                 ~min_size: min_size :: MaybeObs.of(Size) = [#false, #false],
                 ~stretch: stretch :: MaybeObs.of(Stretch) = [#true, #true])
){

 Creates a vertical panel whose content is supplied as a list
 @rhombus(children), instead of individual views, and where
 @rhombus(children) can be an @tech{observable}.

 A value in the @rhombus(children) list is mapped to a
 @rhombus(View, ~class) in two steps: a key is obtained for each list
 element via @rhombus(key), and then both @rhombus(key) and an observable
 for element in @rhombus(children) are passed to @rhombus(make).

}



@doc(
  annot.macro 'HPanel.StyleSymbol'
  annot.macro 'VPanel.StyleSymbol'
){

 Satisfied by the following symbols (the same for horizontal and vertical panels):

@itemlist(

 @item{@rhombus(#'deleted)}
 @item{@rhombus(#'border)} 
 @item{@rhombus(#'vscroll)}
 @item{@rhombus(#'hscroll)}
 @item{@rhombus(#'auto_vscroll)}
 @item{@rhombus(#'auto_hscroll)}
 @item{@rhombus(#'hide_vscroll)}
 @item{@rhombus(#'hide_hscroll)}

)

}

@doc(
  annot.macro 'GroupPanel.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'deleted)}

)

}


@doc(
  annot.macro 'TabsPanel.StyleSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'deleted)}
 @item{@rhombus(#'no_border)} 
 @item{@rhombus(#'can_reorder)}
 @item{@rhombus(#'can_close)}
 @item{@rhombus(#'new_button)}
 @item{@rhombus(#'flat_portable)}

)

}

