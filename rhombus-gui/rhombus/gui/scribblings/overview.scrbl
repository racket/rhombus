#lang rhombus/scribble/manual
@(import:
    "common.rhm" open
    "screenshot.rhm" open
    rhombus/meta open
    rhombus/runtime_path
    pict
    pict/radial
    rhombus/draw)

@(runtime_path.def example1_png: "example1.png")
@(runtime_path.def example2_png: "example2.png")
@(runtime_path.def example3_pre_png: "example3_pre.png")
@(runtime_path.def example3_post_png: "example3_post.png")
@(runtime_path.def example4_pre_png: "example4_pre.png")
@(runtime_path.def example4_post_png: "example4_post.png")
@(runtime_path.def example5_png: "example5.png")
@(runtime_path.def example6_png: "example6.png")
@(runtime_path.def example7_png: "example7.png")

@title(~tag: "overview"){Overview}

Here's ``Hello, World!'' in @rhombusmodname(gui) form:

@rhombusblock(
  #,(@hash_lang()) #,(@rhombuslangname(rhombus))
  import:
    #,(@rhombusmodname(gui))

  gui.Window(gui.Label("Hello, World!")).run()
)

@screenshot(example1_png)

The @rhombus(gui.Window(gui.Label("Hello, World!"))) part constructs a
@tech{view} that describes a window containing a ``Hello, World!''
label. Calling the @rhombus(WindowView.run) method of the view
@tech{renders} it as a live GUI and waits until the user closes the
rendered window.


@section(~tag: "overview-callback"){Controls and Callbacks}

Here's a window with more controls:

@rhombusblock(
  gui.Window(
    ~title: "Recipe",
    gui.VPanel(
      ~margin: [10, 10],
      ~spacing: 10,
      gui.HPanel(
        gui.Checkbox("Lime"),
        gui.Checkbox("Coconut")
      ),
      gui.Choice(~label: "Finish",
                 ["Mix",
                  "Drink It All Up"]),
      gui.Button("Order Drink")
    )
  ).run()
)

@screenshot(example2_png)

Although the window has many controls, nothing happens when a user
clicks the checkbox, popup choice, or button. Let's give the controls
names, so we can query their values after the window is closed, and let's
make the @onscreen{Order Drink} button close the window by giving the
button an @rhombus(~action) callback.

@rhombusblock(
  def lime = gui.Checkbox("Lime")
  def coconut = gui.Checkbox("Coconut")
  def finish = gui.Choice(~label: "Finish",
                          ["Mix",
                           "Drink It All Up"])

  def win:
    gui.Window(
      ~title: "Recipe",
      gui.VPanel(
        ~margin: [10, 10],
        ~spacing: 10,
        gui.HPanel(
          lime,
          coconut,
        ),
        finish,
        gui.Button("Order Drink",
                   ~action: fun (): win.close())
      )
    )

  win.run()

  [lime.at_is_checked.value,
   coconut.at_is_checked.value,
   finish.at_selection.value]
)

The rendered GUI does not look any different in this case, but the
user's selected order is reported after the window is closed.

@section(~tag: "overview-obs"){Connecting Controls via Observables}

The previous example is still fairly simple, because the controls in the
window are all independent. The @emph{reactive} nature of
@rhombusmodname(gui) helps when changing a control should affect another
part of the window immediately.

There are two parts to creating a connection between GUI elements:

@itemlist(

 @item{When a user manipulates a control, it triggers a callback that is
 provided as an @rhombus(~action) argument to the control's constructor.
 The arguments to the callback depend on the kind of control. The
 @rhombus(~action) callback for a @rhombus(gui.Button) takes no
 arguments, but the @rhombus(~action) callback for a
 @rhombus(gui.Checkbox) or @rhombus(gui.Choice) receives the new control
 state or selection.}

 @item{To make some part of a view variable, supply an @tech{observable}
 to the view constructor, instead of a plain value. Create an observable
 with @rhombus(gui.Obs). Internally, a GUI control will read the
 @rhombus(gui.Obs.value) property of an observable and update the GUI
 when it changes. A callback can update an @rhombus(gui.Obs.value)
 property using @rhombus(:=).}

)

Here's a first try at using a @rhombus(message) observable for the label
of a button. The button's label changes whenever the text in a
@rhombus(gui.Input) control is modified by the user.

@rhombusblock(
  fun hello_to(name): "Hello, " ++ name ++ "!"

  def init_name = "Harry"
  def at_message = gui.Obs(hello_to(init_name))

  gui.Window(
    gui.Input(init_name,
              ~action: fun (_, str):
                         at_message.value := hello_to(str)),
    gui.Button(at_message)
  ).run()
)

@screenshot(example3_pre_png, example3_post_png)

Using an observable may seem like the long way around, as opposed to
just setting the button label imperatively in a callback. Part of the
awkwardness here is that the way a button label is derived from a
name really belongs to the button, not to the text-input widget. We can
improve the above implementation by using the @rhombus(~>) operator,
which creates a @tech{derived observable} whose value changes whenever
the original observable's value changes. The left-hand side of
@rhombus(~>) is the original observable, and the right-hand side is a
function to convert the original observable's value to the new
observable's value.

@rhombusblock(
  import:
    gui:
      expose: ~>

  def at_name = gui.Obs("Harry")

  gui.Window(
    gui.Input(at_name,
              ~action: fun (_, str): at_name.value := str),
    gui.Button(at_name ~> (fun (str): "Hello, " ++ str ++ "!"))
  ).run()
)

Since it's so common for a control's action to update an observable,
most control views provide an observable automatically. In fact, we used
those observables at the end of @secref("overview-callback") to get
final control states via @rhombus(lime.at_is_checked.value),
@rhombus(coconut.at_is_checked.value), and
@rhombus(finish.at_selection.value). By convention, an automatic
observable is a property whose name starts with @litchar{at_}. In the
case of a @rhombus(gui.Input) view, the property is
@rhombus(gui.Input.at_content).

@rhombusblock(
  def name_input = gui.Input("Harry")

  gui.Window(
    name_input,
    gui.Button(name_input.at_content ~> (fun (str): "Hello, " ++ str ++ "!"))
  ).run()
)

Observables can determine other properties of a control besides its
label, including whether the control is enabled or the list of available
choices in a @rhombus(gui.Choice). Even the children of a window or
panel can be observables. For example, if we want to let a
user add multiple greeters, we can make the child of a
@rhombus(gui.Window) be an observable whose value is a
@rhombus(gui.VPanel).

@rhombusblock(
  fun make_greeter():
    let name_input = gui.Input("Harry")
    gui.VPanel(
      name_input,
      gui.Button(name_input.at_content ~> (fun (str): "Hello, " ++ str ++ "!"))
    )

  def at_greeters :: gui.Obs.of(List) = gui.Obs([make_greeter()])

  gui.Window(
    at_greeters ~> (fun ([greeter, ...]): gui.VPanel(greeter, ...)),
    gui.HPanel(
      gui.Button("+", ~action:
                        fun ():
                          at_greeters.value
                            := at_greeters.value ++ [make_greeter()]),
      ~alignment: [#'right, #'center],
      ~stretch: [#true, #false]
    )
  ).run()
)

@screenshot(example4_pre_png, example4_post_png)

@section(~tag: "overview-combine-obs"){Combining Observables}

Often, a GUI includes elements whose presentation depends on multiple
other controls. The @rhombus(gui.Obs.combine) function is handy when
multiple observables need to be merged into a single observable. It can
convert a map with observable values into an observable of a map. For
example, the values of two sliders can be combined to configure the
drawing in a canvas.

@rhombusblock(
  import:
    #,(@rhombusmodname(draw))

  def lime_color = draw.Color("limegreen")
  def coconut_color = draw.Color("tan")

  fun plot(dc :: draw.DC, config :: Map):
    let limes = config[#'limes]
    let coconuts = config[#'coconuts]

    let mix_color:
      coconut_color.blend(lime_color with (alpha = limes/(limes+coconuts)))

    dc.pen := draw.Pen.none
    dc.brush := draw.Brush(~color: mix_color with (alpha = 1))
    dc.rectangle([[0, 0], dc.size])

  fun ingredient(label, init) :: gui.Slider:
    gui.Slider(~value: init,
               ~label: label,
               ~min_value: 1,
               ~max_value: 10,
               ~stretch: [#true, #false],
               ~margin: [5, 5])

  def limes = ingredient("Limes", 1)
  def coconuts = ingredient("Coconuts", 3)

  gui.Window(
    ~title: "Mixed Drink",
    ~size: [300, 200],
    limes,
    coconuts,
    gui.Canvas(gui.Obs.combine({ #'limes: limes.at_value,
                                 #'coconuts: coconuts.at_value }),
               plot)
  ).run()
)

@screenshot(example5_png)

@section(~tag: "overview-state"){Views, State, and Rendered Controls}

Going back to the multiple-greeter example, what would happen if we put
the result of a single call to @rhombus(make_greeter) multiple times in
the @rhombus(at_greeters) list? The window will start with two greeters,
because each use of the view is rendered to a fresh panel, input field,
and button. The two different renderings are not independent, however.
Editing either input field adjusts both buttons, as well as the other
input field.

@rhombusblock(
  def g1 = make_greeter()
  def at_greeters :: gui.Obs.of(List) = gui.Obs([g1, g1])
)

@screenshot(example6_png)

The two greeter renderings are connected because a single
@rhombus(gui.Input.at_content) observable is created when
@rhombus(make_greeter) calls @rhombus(gui.Input). Editing either
rendered input field updates that same observable, so both rendered
buttons see the change and adapt their labels.

Meanwhile, editing one input field adjusts the other input field because
a @rhombus(gui.Input) view's observable is reflected back to the input
field's content. The intent of that reflection is to preserve the view's
content if the view's rendering is removed and recreated---which is what
happens when a new greeter is added and the panel of greeters changes.
When an observable is provided to a constructor like @rhombus(gui.Input)
for the view's value, it provides the initial value for the view's
state, and any change to the provided observable's value is treated like
input from a user to update the state.

In short, a view can be rendered multiple times, either concurrently or
sequentially and in different windows, but all renderings of the view
share state that is part of the view. The shared state is always limited
to an observable like @rhombus(gui.Input.at_content) to hold a control's
value or selection. Some views, like @rhombus(gui.Button) or
@rhombus(gui.Window) have no such observables and no internal state.
Note that a view can have external state that is part of it's input,
such as an observable for a button's label, but that is not part of the
view's own state.

Each rendering of a view can have state that is specific to the
rendering. For example, each rendering of a @rhombus(gui.Button) will
appear at some location relative to its enclosing window, and it is
independently in the process of being clicked or not. That
rendering-specific state is accessible from a view only for the
@tech{most recent rendering} of the view, and some methods of views use
the most recent rendering. For example, @rhombus(gui.WindowView.close)
closes the most recent rendering of a window, which means that the
following @rhombus(window) view makes sense only for a single rendering
at a time.

@rhombusblock(
  def win:
    gui.Window(
      gui.Button("Order Drink",
                 ~action: fun (): win.close())
    )

  win.run()
  win.run() // ok to run twice, since it's one at a time
)

@screenshot(example7_png)
