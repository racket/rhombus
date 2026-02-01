#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@(def unexported: @rhombus(hidden, ~var))

@title(~tag: "all-menu", ~style: #'toc){Menu Views}

@local_table_of_contents()

@// ------------------------------------------------------------
@section(~tag: "menu-view"){Menu View Interface}

@doc(
  interface gui.MenuChildView:
    extends View
    implementable #,(@unexported)
){

 A @deftech{menu-child view} represents a view that can be added to a
 menu.

 Create a @rhombus(MenuChildView, ~class) using @rhombus(Menu, ~class),
 @rhombus(MenuItem, ~class), @rhombus(CheckableMenuItem, ~class), or
 @rhombus(MenuItemSeparator, ~class).

}

@// ------------------------------------------------------------
@section(~tag: "menu-bar", ~style: [#'hidden]){@rhombus(MenuBar)}

@centered(@(gallery("menu-bar")))

@doc(
  ~page
  class gui.MenuBar():
    implements View
    constructor (
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      item :: ObsOrValue.of(Menu),
      ...
    )
){

 Creates a menu bar for association with a @rhombus(Window, ~class)
 through its @rhombus(~menu_bar) constructor argument.

}

@// ------------------------------------------------------------
@section(~tag: "menu", ~style: [#'hidden]){@rhombus(Menu)}

@centered(@(gallery("menu")))

@doc(
  ~page
  class gui.Menu():
    implements MenuChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~enable: enable :: ObsOrValue.of(Boolean) = #true,
      ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      item :: ObsOrValue.of(MenuChildView),
      ...
    )
){

 Creates a menu containing menu items and submenus for inclusion in a
 @rhombus(MenuBar, ~class), @rhombus(Menu, ~class), or
 @rhombus(PopupMenu, ~class).

}

@// ------------------------------------------------------------
@section(~tag: "popup-menu", ~style: [#'hidden]){@rhombus(PopupMenu)}

@centered(@(gallery("popup-menu")))

@doc(
  ~page
  class gui.PopupMenu():
    constructor (
      item :: ObsOrValue.of(MenuChildView),
      ...
    )

  method (menu :: gui.PopupMenu).popup(
    view :: WindowChildView,
    x :: View.PositionInt,
    y :: View.PositionInt
  ) :: Void
){

 Creates a popup menu containing menu items and submenus. Render a popup
 menu using @rhombus(PopupMenu.popup) or @rhombus(WindowChildView.popup).

 A method call @rhombus(menu.popup(view, x, y)) is equivalent to
 @rhombus(view.popup(menu, x, y)).

}

@// ------------------------------------------------------------
@section(~tag: "menu-item", ~style: [#'hidden]){@rhombus(MenuItem)}

@centered(@(gallery("menu-item")))

@doc(
  ~page
  class gui.MenuItem():
    implements MenuChildView
    constructor (
     label :: ObsOrValue.of(View.LabelString),
     ~action: action :: Boolean -> ~any = values,
     ~enable: enable :: ObsOrValue.of(Boolean) = #true,
     ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
     ~shortcut: shortcut :: ObsOrValue.of(maybe(MenuItem.Shortcut))
                  = #false
    )
){

 Creates a menu item for including in a menu.

}

@doc(
  annot.macro 'gui.MenuItem.Shortcut'
  fun gui.MenuItem.default_shortcut_prefix() :: List.of(Symbol)
){

 The @rhombus(MenuItem.Shortcut, ~annot) annotation recognizes a
 menu-item shortcut key combination in one of the following forms:

@itemlist(

 @item{a @rhombus(Char, ~annot);}

 @item{a @rhombus(KeyEvent.Key, ~annot); or}

 @item{a list of modifier symbols ending with a @rhombus(Char, ~annot)
  or @rhombus(KeyEvent.Key, ~annot). The following modifier symbols are
  allowed:

 @itemlist(

 @item{@rhombus(#'alt) (Windows and Unix only)}

 @item{@rhombus(#'cmd) (Mac OS only)}

 @item{@rhombus(#'meta) (Unix only, and not combined with @rhombus(#'alt))}

 @item{@rhombus(#'ctl)}

 @item{@rhombus(#'shift)}

 @item{@rhombus(#'option) (Mac OS only)}
 )}

)

 The @rhombus(MenuItem.default_shortcut_prefix) function returns the
 list of modifier symbols that is added for the current platform when
 just a @rhombus(Char, ~annot) or @rhombus(KeyEvent.Key, ~annot) is
 provided for a shortcut.

}

@// ------------------------------------------------------------
@section(~tag: "checkable-menu-item", ~style: [#'hidden]){@rhombus(CheckableMenuItem)}

@centered(@(gallery("checkable-menu-item")))

@doc(
  ~page
  class gui.CheckableMenuItem():
    implements MenuChildView
    constructor (
     label :: ObsOrValue.of(View.LabelString),
     ~is_checked: is_checked :: ObsOrValue.of(Boolean) = #false,
     ~action: action :: () -> ~any = values,
     ~enable: enable :: ObsOrValue.of(Boolean) = #true,
     ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
     ~shortcut: shortcut :: ObsOrValue.of(maybe(MenuItem.Shortcut))
                  = #false
    )
){

 Like @rhombus(MenuItem), but for a menu item that can have a checkmark.

}

@// ------------------------------------------------------------
@section(~tag: "menu-item-sep", ~style: [#'hidden]){@rhombus(MenuItemseparator)}

@centered(@(gallery("menu-item-separator")))

@doc(
  ~page
  class gui.MenuItemSeparator():
    implements MenuChildView
    constructor ()
){

 Creates a separator to be used between items in a menu.

}
