#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "screenshot.rhm" open
    pict)

@(macro 'entry_wrap($str, ..., $id, $wrap)':
    'tabular(~column_properties: [#'center],
             [[rhombus($id)],
              [@rhombuslink($id){@($wrap(gallery($str)))}],
              ...])')

@(macro 'entry($str, ..., $id)': 'entry_wrap($str, ..., $id, values)')

@(fun refade_trim(p :~ pict.Pict): refade(window_trim(p.scale(2))).scale(0.5))

@title(~tag: "gallery"){View Gallery}

@tabular(
  [[hspace(3),
    entry_wrap("window", gui.Window, refade_trim),
    entry_wrap("dialog", gui.Dialog, refade_trim),
    hspace(3)]]
)

@tabular(
  ~column_properties: [[#'center, #'top]],
  ~pad: 1,
  [[entry("button", gui.Button),
    entry("checkbox", gui.Checkbox),
    entry("choice", gui.Choice)],
   [entry("radio-choice", gui.RadioChoice),
    entry("list-choice", gui.ListChoice),
    entry("table", gui.Table)],
   [entry("input", "choice-input", gui.Input),
    entry("slider", gui.Slider),
    entry("progress", gui.Progress)],
   [entry("label", gui.Label),
    entry("image", gui.Image),
    entry("canvas", gui.Canvas)],
   [entry("group-panel", gui.GroupPanel),
    entry("tabs-panel", gui.TabsPanel),
    entry("editor-canvas", gui.EditorCanvas)],
   [entry("vpanel", gui.VPanel),
    entry("hpanel", gui.HPanel),
    rhombus(gui.Spacer)],
   [entry("popup-menu", gui.PopupMenu),
    entry("menu-bar", gui.MenuBar),
    entry("menu", gui.Menu)],
   [entry("menu-item", gui.MenuItem),
    entry("checkable-menu-item", gui.CheckableMenuItem),
    entry("menu-item-separator", gui.MenuItemSeparator)]]
)
