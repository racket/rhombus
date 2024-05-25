#lang scribble/rhombus/manual
@(import:
    "common.rhm" open)

@title{Events}

@doc(
  class MouseEvent():
    constructor (
      ~kind: kind :: MouseEvent.Kind = #'motion,
      ~downs: downs :: Set.of(MouseEvent.Down) = Set{},
      ~x: x :: Int = 0,
      ~y: y :: Int = 0,
      ~timestamp: timestamp :: Int = 0,
    )

  property (ev :: MouseEvent).kind
    :: MouseEvent.Kind
  property (ev :: MouseEvent).downs
    :: Set.of(MouseEvent.Down)
  property (ev :: MouseEvent).x :: Int
  property (ev :: MouseEvent).y :: Int
  property (ev :: MouseEvent).timestamp :: Int
){

 Represents a mouse event.

}

@doc(
  method (ev :: MouseEvent).is_down(
    sym :: MouseEvent.Down
  ) :: Boolean
){

  Checks whether @rhombus(MouseEvent.downs(ev)) would include @rhombus(sym),
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
  method (ev :: MouseEvent).is_dragging()
    :: Boolean
){

 Reports a derived property of the mouse event.

}


@doc(
  class KeyEvent():
    constructor (
      code :: (Char || KeyEvent.Key),
      ~release_code: r_code :: (Char || KeyEvent.Key) = #'press,
      ~other_caps_code: oc_code :: maybe(Char || KeyEvent.Key) = #false,
      ~other_shift_code: os_code :: maybe(Char || KeyEvent.Key) = #false,
      ~other_altgr_code: oa_code :: maybe(Char || KeyEvent.Key) = #false,
      ~other_shift_altgr_code: osa_code :: maybe(Char || KeyEvent.Key) = #false,
      ~downs: downs :: Set.of(KeyEvent.Down) = Set{},
      ~x: x :: Int = 0,
      ~y: y :: Int = 0,
      ~timestamp: timestamp :: Int = 0,
      ~use_altgr: use_altgr :: Any = #true,
    )

  property (ev :: KeyEvent).code
    :: (Char || KeyEvent.Key)
  property (ev :: KeyEvent).release_code
    :: (Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_caps_code
    :: maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_shift_code
    :: maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_altgr_code
    :: maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).other_shift_altgr_code
    :: maybe(Char || KeyEvent.Key)
  property (ev :: KeyEvent).downs
    :: Set.of(KeyEvent.Down)
  property (ev :: KeyEvent).x :: Int
  property (ev :: KeyEvent).y :: Int
  property (ev :: KeyEvent).timestamp :: Int
){

 Represents a keyboard event.

}

@doc(
  method (ev :: KeyEvent).is_down(
    sym :: KeyEvent.Down
  ) :: Boolean
){

 Checks whether @rhombus(KeyEvent.downs(ev)) would include @rhombus(sym),
 potentially avoiding the construction of a set internally.

}


@doc(
  enum MouseEvent.Kind:
    enter
    leave
    left_down
    left_up
    middle_down
    middle_up
    right_down
    right_up
    motion    
){

 General category for a mouse event.

}


@doc(
  enum MouseEvent.Down:
    shift
    control
    meta
    alt
    caps
    mod3
    mod4
    mod5
){

 Modifier keys that can be down during a mouse event.

}


@doc(
  enum MouseEvent.Button:
    left
    middle
    right
    any
){

 Mouse buttons that can be down during a mouse event.

}

@doc(
  enum KeyEvent.Down:
    shift
    control
    meta
    alt
    caps
    mod3
    mod4
    mod5
){

 Modifier keys that can be down during a key event.

}


@doc(
  enum KeyEvent.Key:
    start
    cancel
    clear
    shift
    rshift
    control
    rcontrol
    menu
    pause
    capital
    prior
    next
    end
    home
    left
    up
    right
    down
    escape
    select
    print
    execute
    snapshot
    insert
    help
    numpad0
    numpad1
    numpad2
    numpad3
    numpad4
    numpad5
    numpad6
    numpad7
    numpad8
    numpad9
    numpad_enter
    multiply
    add
    separator
    subtract
    decimal
    divide
    f1
    f2
    f3
    f4
    f5
    f6
    f7
    f8
    f9
    f10
    f11
    f12
    f13
    f14
    f15
    f16
    f17
    f18
    f19
    f20
    f21
    f22
    f23
    f24
    numlock
    scroll
    wheel_up
    wheel_down
    wheel_left
    wheel_right
    release
    press
){

 Keys that can be pressed or released for a key event.

}
