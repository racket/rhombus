#lang rhombus/scribble/manual
@(import:
    "common.rhm" open)

@title{Menus}

@doc(
  class gui.MenuBar():
    implements View
    constructor (
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      item :: ObsOrValue.of(Menu),
      ...
    )
){

 Creates a menu bar for association with a @rhombus(Window, ~class)
 through its @rhombus(~menu_bar) constructor argument.

}

@doc(
  class gui.Menu():
    implements MenuChildView
    constructor (
      label :: ObsOrValue.of(View.LabelString),
      ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
      ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
      item :: ObsOrValue.of(MenuChildView),
      ...
    )
){

 Creates a menu containing menu items and submenus.

}

@doc(
  class gui.MenuItem():
    implements MenuChildView
    constructor (
     label :: ObsOrValue.of(View.LabelString),
     ~action: action :: () -> ~any = values,
     ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
     ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
     ~shortcut: shortcut :: ObsOrValue.of(maybe(MenuItem.Shortcut))
                  = #false
    )
){

 Creates a menu item for including in a menu.

}


@doc(
  class gui.CheckableMenuItem():
    implements MenuChildView
    constructor (
     label :: ObsOrValue.of(View.LabelString),
     ~is_checked: is_checked :: ObsOrValue.of(Boolean) = #false,
     ~action: action :: () -> ~any = values,
     ~is_enabled: is_enabled :: ObsOrValue.of(Boolean) = #true,
     ~help: help_text :: ObsOrValue.of(maybe(View.LabelString)) = #false,
     ~shortcut: shortcut :: ObsOrValue.of(maybe(MenuItem.Shortcut))
                  = #false
    )
){

 Like @rhombus(MenuItem), but for a menu item that can have a checkmark.

}

@doc(
  class gui.MenuItemSeparator():
    implements MenuChildView
    constructor ()
){

 Creates a separator to be used between items in a menu.

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
