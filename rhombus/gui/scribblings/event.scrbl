#lang scribble/rhombus/manual
@(import: "common.rhm" open)

@title{Events}

@doc(
  class MouseEvent():
    constructor (~kind: kind :: MouseEvent.Kind = #'motion,
                 ~down: down :: Set.of(MouseEvent.DownSymbol) = Set{},
                 ~x: x :: Int = 0,
                 ~y: y :: Int = 0,
                 ~timestamp: timestamp :: Int = 0)

  property (ev :: MouseEvent).kind :: MouseEvent.Kind
  property (ev :: MouseEvent).down :: Set.of(MouseEvent.DownSymbol)
  property (ev :: MouseEvent).x :: Int
  property (ev :: MouseEvent).y :: Int
  property (ev :: MouseEvent).timestamp :: Int
){

 Represents a mouse event.

}

@doc(
  method (ev :: MouseEvent).is_down(sym :: MouseEvent.DownSymbol)
    :: Boolean
){

  Checks whether @rhombus(MouseEvent.down(ev)) would include @rhombus(sym),
  potentially avoiding the construction of a set internally.

}

@doc(
  method (ev :: MouseEvent).is_button_changed(
    which :: MouseEvent.Button = #'any
  ) :: Boolean
  method (ev :: MouseEvent).is_button_press(
    which :: MouseEvent.Button = #'any
  ) :: Boolean
  method (ev :: MouseEvent).is_button_release(
    which :: MouseEvent.Button = #'any
  ) :: Boolean
  method (ev :: MouseEvent).is_dragging() :: Boolean
){

 Reports a derived property of the mouse event.

}


@doc(
  class KeyEvent():
    constructor (code :: (Char || KeyEvent.Key),
                 ~release_code: r_code :: (Char || KeyEvent.Key) = #'press,
                 ~other_caps_code: oc_code :: Maybe(Char || KeyEvent.Key) = #false,
                 ~other_shift_code: os_code :: Maybe(Char || KeyEvent.Key) = #false,
                 ~other_altgr_code: oa_code :: Maybe(Char || KeyEvent.Key) = #false,
                 ~other_shift_altgr_code: osa_code :: Maybe(Char || KeyEvent.Key) = #false,
                 ~down: down :: Set.of(KeyEvent.DownSymbol) = Set{},
                 ~x: x :: Int = 0,
                 ~y: y :: Int = 0,
                 ~timestamp: timestamp :: Int = 0,
                 ~use_altgr: use_altgr = #true)

  property (ev :: KeyEvent).code :: Char || KeyEvent.Key
  property (ev :: KeyEvent).release_code :: Char || KeyEvent.Key
  property (ev :: KeyEvent).other_caps_code :: Maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_shift_code :: Maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_altgr_code :: Maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_shift_altgr_code :: Maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).down :: Set.of(KeyEvent.DownSymbol)
  property (ev :: KeyEvent).x :: Int
  property (ev :: KeyEvent).y :: Int
  property (ev :: KeyEvent).timestamp :: Int
){

 Represents a keyboard event.

}

@doc(
  method (ev :: KeyEvent).is_down(sym :: KeyEvent.DownSymbol)
    :: Boolean
){

 Checks whether @rhombus(KeyEvent.down(ev)) would include @rhombus(sym),
 potentially avoiding the construction of a set internally.

}


@doc(
  annot.macro 'MouseEvent.Kind'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'enter)}
 @item{@rhombus(#'leave)}
 @item{@rhombus(#'left_down)}
 @item{@rhombus(#'left_up)}
 @item{@rhombus(#'middle_down)}
 @item{@rhombus(#'middle_up)}
 @item{@rhombus(#'right_down)}
 @item{@rhombus(#'right_up)}
 @item{@rhombus(#'motion)}

)

}


@doc(
  annot.macro 'MouseEvent.DownSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'left)}
 @item{@rhombus(#'middle)}
 @item{@rhombus(#'right)}
 @item{@rhombus(#'shift)}
 @item{@rhombus(#'control)}
 @item{@rhombus(#'meta)}
 @item{@rhombus(#'alt)}
 @item{@rhombus(#'caps)}
 @item{@rhombus(#'mod3)}
 @item{@rhombus(#'mod4)}
 @item{@rhombus(#'mod5)}

)

}


@doc(
  annot.macro 'MouseEvent.Button'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'left)}
 @item{@rhombus(#'middle)}
 @item{@rhombus(#'right)}
 @item{@rhombus(#'any)}

)

}

@doc(
  annot.macro 'KeyEvent.DownSymbol'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'shift)}
 @item{@rhombus(#'control)}
 @item{@rhombus(#'meta)}
 @item{@rhombus(#'alt)}
 @item{@rhombus(#'caps)}
 @item{@rhombus(#'mod3)}
 @item{@rhombus(#'mod4)}
 @item{@rhombus(#'mod5)}

)

}


@doc(
  annot.macro 'KeyEvent.Key'
){

 Satisfied by the following symbols:

@itemlist(

 @item{@rhombus(#'start)}
 @item{@rhombus(#'cancel)}
 @item{@rhombus(#'clear)}
 @item{@rhombus(#'shift)}
 @item{@rhombus(#'rshift)}
 @item{@rhombus(#'control)}
 @item{@rhombus(#'rcontrol)}
 @item{@rhombus(#'menu)}
 @item{@rhombus(#'pause)}
 @item{@rhombus(#'capital)}
 @item{@rhombus(#'prior)}
 @item{@rhombus(#'next)}
 @item{@rhombus(#'end)}
 @item{@rhombus(#'home)}
 @item{@rhombus(#'left)}
 @item{@rhombus(#'up)}
 @item{@rhombus(#'right)}
 @item{@rhombus(#'down)}
 @item{@rhombus(#'escape)}
 @item{@rhombus(#'select)}
 @item{@rhombus(#'print)}
 @item{@rhombus(#'execute)}
 @item{@rhombus(#'snapshot)}
 @item{@rhombus(#'insert)}
 @item{@rhombus(#'help)}
 @item{@rhombus(#'numpad0)}
 @item{@rhombus(#'numpad1)}
 @item{@rhombus(#'numpad2)}
 @item{@rhombus(#'numpad3)}
 @item{@rhombus(#'numpad4)}
 @item{@rhombus(#'numpad5)}
 @item{@rhombus(#'numpad6)}
 @item{@rhombus(#'numpad7)}
 @item{@rhombus(#'numpad8)}
 @item{@rhombus(#'numpad9)}
 @item{@rhombus(#'numpad_enter)}
 @item{@rhombus(#'multiply)}
 @item{@rhombus(#'add)}
 @item{@rhombus(#'separator)}
 @item{@rhombus(#'subtract)}
 @item{@rhombus(#'decimal)}
 @item{@rhombus(#'divide)}
 @item{@rhombus(#'f1)}
 @item{@rhombus(#'f2)}
 @item{@rhombus(#'f3)}
 @item{@rhombus(#'f4)}
 @item{@rhombus(#'f5)}
 @item{@rhombus(#'f6)}
 @item{@rhombus(#'f7)}
 @item{@rhombus(#'f8)}
 @item{@rhombus(#'f9)}
 @item{@rhombus(#'f10)}
 @item{@rhombus(#'f11)}
 @item{@rhombus(#'f12)}
 @item{@rhombus(#'f13)}
 @item{@rhombus(#'f14)}
 @item{@rhombus(#'f15)}
 @item{@rhombus(#'f16)}
 @item{@rhombus(#'f17)}
 @item{@rhombus(#'f18)}
 @item{@rhombus(#'f19)}
 @item{@rhombus(#'f20)}
 @item{@rhombus(#'f21)}
 @item{@rhombus(#'f22)}
 @item{@rhombus(#'f23)}
 @item{@rhombus(#'f24)}
 @item{@rhombus(#'numlock)}
 @item{@rhombus(#'scroll)}
 @item{@rhombus(#'wheel_up)}
 @item{@rhombus(#'wheel_down)}
 @item{@rhombus(#'wheel_left)}
 @item{@rhombus(#'wheel_right)}
 @item{@rhombus(#'release)}
 @item{@rhombus(#'press)}

)

}
