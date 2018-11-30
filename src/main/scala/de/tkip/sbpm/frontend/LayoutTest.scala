package de.tkip.sbpm.frontend
import org.scalajs.dom.html
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom, dom.MouseEvent

object LayoutTest {
//
//  def content = SingleSide.Content(source, Main2())
//
//  val source = GhPagesMacros.exampleSource

  def component = ScalaComponent.static("Test example")(
    <.div(
      <.p(
        <.br,
        "test test test"),
      <.br,
     Main()))

  // EXAMPLE:START
  import org.scalajs.dom.ext.KeyCode

  val OuterX    = 1400
  val OuterY    = 560
  val InnerSize =  24
  val MoveDist  =  24
  var offsetx = 0
  var offsety = 0

  case class State(x: Int, y: Int)

  def initState = State((OuterX - InnerSize) / 2, (OuterY - InnerSize) / 2)

  val OuterDiv =
    <.div(
      ^.tabIndex   := 0,
      ^.width      := OuterX.px,
      ^.height     := OuterY.px,
      ^.border     := "solid 1px #333",
      ^.background := "#ddd")

  val InnerDiv =
    <.div(
      ^.position.relative,
      ^.width      := InnerSize.px,
      ^.height     := InnerSize.px,
      ^.background := "#800")

  def moveOneAxis(pos: Int, steps: Int, max: Int): Int =
    (pos + steps * MoveDist) min (max - InnerSize) max 0

  class Backend($: BackendScope[Unit, State]) {
    private val outerRef = Ref[html.Element]

    def init: Callback =
      outerRef.foreach(_.focus())

    def move(dx: Int, dy: Int): Callback =
      $.modState(s => s.copy(
        x = moveOneAxis(s.x, dx, OuterX),
        y = moveOneAxis(s.y, dy, OuterY)))


    def dragStart(x: Int, y: Int)(e: ReactMouseEvent): Callback = {
      offsetx = e.pageX.toInt - x
      offsety = e.pageY.toInt - y
      dom.console.info(s"start: $offsetx, $offsety")
      Callback()
    }

    def dragState(e: ReactMouseEventFromHtml): Callback ={
      e.persist()

      var x = e.pageX.toInt
      var y = e.pageY.toInt
      dom.console.info(x,y)
      if(x == 0 && y == 0)
        return e.preventDefaultCB
      x -= offsetx
      y -= offsety
      e.preventDefaultCB >> $.modState(s => s.copy(x, y))
    }

    def handleKey(e: ReactKeyboardEvent): Callback = {

      def plainKey: CallbackOption[Unit] =             // CallbackOption will stop if a key isn't matched
        CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Up    => move(0, -1)
          case KeyCode.Down  => move(0,  1)
          case KeyCode.Left  => move(-1, 0)
          case KeyCode.Right => move( 1, 0)
        }

      def ctrlKey: CallbackOption[Unit] =              // Like above but if ctrlKey is pressed
        CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
          case KeyCode.Up    => move(0, -OuterY)
          case KeyCode.Down  => move(0,  OuterY)
          case KeyCode.Left  => move(-OuterX, 0)
          case KeyCode.Right => move( OuterX, 0)
        }

      (plainKey orElse ctrlKey) >> e.preventDefaultCB  // This is the interesting part.
      //
      // orElse joins CallbackOptions so if one fails, it tries the other.
      //
      // The >> means "and then run" but only if the left side passes.
      // This means preventDefault only runs if a valid key is pressed.
    }

    def render(s: State) =
      OuterDiv.withRef(outerRef)(
        ^.onKeyDown ==> handleKey,
        ^.onDragStart ==> dragStart(s.x, s.y),
        ^.onDrag ==> dragState,
        InnerDiv(
          ^.draggable := true,
          ^.left := s.x.px,
          ^.top  := s.y.px
        ))
  }

  val Main = ScalaComponent.builder[Unit]("Test Example")
    .initialState(initState)
    .renderBackend[Backend]
    .componentDidMount(_.backend.init)
    .build

  // EXAMPLE:END
}