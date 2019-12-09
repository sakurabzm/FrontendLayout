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

    val OuterX = 1400
    val OuterY = 560
    val InnerSize = 24
    val MoveDist = 24
    var offsetx = 0
    var offsety = 0

    case class State(x: Int, y: Int)

    def initState = State((OuterX - InnerSize) / 2, (OuterY - InnerSize) / 2)

    val OuterDiv =
        <.div(
            ^.tabIndex := 0,
            ^.width := OuterX.px,
            ^.height := OuterY.px,
            ^.border := "solid 1px #333",
            ^.background := "#ddd")

    val InnerDiv =
        <.div(
            ^.position.relative,
            ^.width := 300.px,
            ^.height := 300.px,
            ^.borderRadius := 15.px,
            ^.background := "#800",
            <.div(
                ^.position.absolute,
                ^.margin.auto,
                ^.top := 0.px,
                ^.left := 0.px,
                ^.right := 0.px,
                ^.bottom := 0.px,
                ^.width := 100.px,
                ^.height := 100.px,
                ^.borderRadius := 100.px,
                ^.background := "#200",
            )
        )
    val GenericAction =
        <.div(
            "Action",
            ^.textAlign.center,
            ^.display.`table-cell`,
            ^.verticalAlign.middle,
            ^.position.relative,
            ^.width := 40.px,
            ^.height := 40.px,
            ^.borderRadius := 6.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA"
        )

    val ReceiveAction =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA",

            <.div(
                "R",
                ^.textAlign.center,
                ^.fontWeight.bold,
                ^.position.absolute,
                ^.width := 30.px,
                ^.height := 20.px,
                ^.margin.auto,
                ^.top := 10.px,
                ^.left := 0.px,
                ^.right := 0.px,
                ^.bottom := 0.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 ",
                <.div(
                    ^.position.absolute,
                    "▽",
                    ^.transform := "scale(5.8,2)",
                    ^.top := (-15).px,
                    ^.left := 10.px
                ),
            )
        )

    val SendAction =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA",

            <.div(
                "S",
                ^.textAlign.center,
                ^.fontWeight.bold,
                ^.position.absolute,
                ^.width := 30.px,
                ^.height := 20.px,
                ^.margin.auto,
                ^.top := (0).px,
                ^.left := 0.px,
                ^.right := 0.px,
                ^.bottom := 0.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 ",
                <.div(
                    ^.position.absolute,
                    "▽",
                    ^.transform := "scale(5.3,1.8)",
                    ^.top := 11.px,
                    ^.left := 10.px
                ),
            ))

    val EndAction =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 5px #000",
            ^.borderBottom := "solid 5px #000",
            ^.borderLeft := "solid 5px #000 ",
            ^.borderRight := "solid 5px #000 ",
            ^.background := "#B0C4DE",
        )
    val ModelSplit =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA",
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.top := 11.px,
                ^.left := 12.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 "
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.top := 11.px,
                ^.right := 12.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 "
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.bottom := 11.px,
                ^.right := 12.px,
                ^.borderTop := "dotted 2px #000",
                ^.borderBottom := "dotted 2px #000",
                ^.borderLeft := "dotted 2px #000 ",
                ^.borderRight := "dotted 2px #000 "
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.bottom := 11.px,
                ^.left := 12.px,
                ^.borderTop := "dotted 2px #000",
                ^.borderBottom := "dotted 2px #000",
                ^.borderLeft := "dotted 2px #000 ",
                ^.borderRight := "dotted 2px #000 "
            )
        )

    val Circle =
        <.div(
            ^.position.absolute,
            ^.width := 20.px,
            ^.height := 2.px,
            ^.backgroundColor := "black",

            <.div(
                ^.position.absolute,
                ^.width := 2.px,
                ^.height := 20.px,
                ^.backgroundColor := "black",
                ^.marginTop := (-20).px,
                ^.marginLeft := 18.px,
                <.div(
                    ^.position.absolute,
                    ^.width := 20.px,
                    ^.height := 2.px,
                    ^.backgroundColor := "black",
                    ^.marginTop := (0).px,
                    ^.marginLeft := (-18).px,
                    <.div(
                        ^.position.absolute,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.marginLeft := 0.px,
                        ^.marginTop := 1.px,
                        ^.backgroundColor := "black",
                        ^.transform := "rotate(30deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.marginLeft := (0).px,
                        ^.marginTop := (-1).px,
                        ^.backgroundColor := "black",
                        ^.transform := "rotate(-30deg)"
                    )
                )
            )

        )

    val Cross =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#B0C4DE",
            <.div(
                ^.position.absolute,
                ^.width := 80.px,
                ^.height := 1.px,
                ^.margin.auto,
                ^.bottom := 0.px,
                ^.right := 0.px,
                ^.top := (0).px,
                ^.left := (-10).px,
                ^.background := "#000",
                ^.transform := "rotate(45deg)"
            )
        )

    val RightDown =
        <.div(

        )
    val ModelJoin =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA",
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.top := 11.px,
                ^.left := 12.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 ",
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := 0.px,
                    ^.right := 0.px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(45deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := (0).px,
                    ^.right := (0).px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(135deg)"
                )
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.top := 11.px,
                ^.right := 12.px,
                ^.borderTop := "solid 2px #000",
                ^.borderBottom := "solid 2px #000",
                ^.borderLeft := "solid 2px #000 ",
                ^.borderRight := "solid 2px #000 ",
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := 0.px,
                    ^.right := 0.px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(45deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := (0).px,
                    ^.right := (0).px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(135deg)"
                )
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.bottom := 11.px,
                ^.left := 12.px,
                ^.borderTop := "dotted 2px #000",
                ^.borderBottom := "dotted 2px #000",
                ^.borderLeft := "dotted 2px #000 ",
                ^.borderRight := "dotted 2px #000 ",
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := 0.px,
                    ^.right := 0.px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(45deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 2.px,
                    ^.margin.auto,
                    ^.bottom := (0).px,
                    ^.right := (0).px,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.background := "#000",
                    ^.transform := "rotate(135deg)"
                )
            ),
            <.div(
                ^.position.absolute,
                ^.width := 12.px,
                ^.height := 12.px,
                ^.bottom := 11.px,
                ^.right := 12.px,
                ^.borderTop := "dotted 2px #000",
                ^.borderBottom := "dotted 2px #000",
                ^.borderLeft := "dotted 2px #000 ",
                ^.borderRight := "dotted 2px #000 "
            )
        )
    val Merge =
        <.div(
            ^.position.relative,
            ^.width := 60.px,
            ^.height := 60.px,
            ^.borderRadius := 60.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA"
        )

    val Macro =
        <.div(
            "Macro:",
            ^.fontWeight.bolder,
            ^.textAlign.center,
            ^.display.`table-cell`,
            ^.verticalAlign.middle,
            ^.position.relative,
            ^.width := 90.px,
            ^.height := 60.px,
            ^.borderRadius := 6.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            ^.background := "#58ACFA"

        )

    val NewReceiveS =
        <.div(
            ^.backgroundColor := "gold",
            ^.position.relative,
            ^.width := 40.px,
            ^.height := 40.px,
            ^.borderRadius := 40.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            <.div(
                "R",
                ^.textAlign.center,
                ^.fontWeight.bold,
                ^.position.absolute,
                ^.overflow.auto,
                ^.width := "50%",
                ^.minWidth := "25%",
                ^.minHeight := "18.75%",
                ^.height := "37.5%",
                ^.margin.auto,
                ^.top := 11.px,
                ^.left := 0.px,
                ^.right := 0.px,
                ^.bottom := 0.px,
                ^.borderTop := "solid 1px #000",
                ^.borderBottom := "solid 1px #000",
                ^.borderLeft := "solid 1px #000 ",
                ^.borderRight := "solid 1px #000 "
            ),
            <.div(
                ^.position.absolute,
                ^.marginTop := (11).px,
                ^.marginLeft := 6.px,
                ^.width := (28).px,
                ^.height := 1.px,
                ^.background := "#000",
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 1.px,
                    ^.top := (4).px,
                    ^.left := (-1).px,
                    ^.background := "#000",
                    ^.transform := "rotate(30deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 16.px,
                    ^.height := 1.px,
                    ^.top := (4).px,
                    ^.right := (-1).px,
                    ^.background := "#000",
                    ^.transform := "rotate(-30deg)"
                )
            ),
            <.div(
                ^.position.absolute,
                ^.width := 2.px,
                ^.height := 60.px,
                ^.marginLeft := 19.px,
                ^.marginTop := 40.px,
                ^.backgroundColor := "red",
                <.div(
                    ^.position.absolute,
                    ^.width := 120.px,
                    ^.height := 2.px,
                    ^.marginTop := 60.px,
                    ^.marginLeft := (-60).px,
                    ^.backgroundColor := "red"
                )
            )

        )

    val NewSendS =
        <.div(
            ^.position.relative,
            ^.width := 40.px,
            ^.height := 40.px,
            ^.borderRadius := 40.px,
            ^.borderTop := "solid 1px #000",
            ^.borderBottom := "solid 1px #000",
            ^.borderLeft := "solid 1px #000 ",
            ^.borderRight := "solid 1px #000 ",
            <.div(
                ^.position.absolute,
                ^.width := 20.px,
                ^.height := 15.px,
                ^.top := (11).px,
                ^.left := 9.px,
                ^.borderTop := "solid 1px #000",
                ^.borderBottom := "solid 1px #000",
                ^.borderLeft := "solid 1px #000 ",
                ^.borderRight := "solid 1px #000 ",
                <.p(
                    "S",
                    ^.textAlign.center,
                    ^.fontWeight.bold,
                    ^.marginTop := (-3).px
                )
            ),
            <.div(
                ^.position.absolute,
                ^.marginTop := (24).px,
                ^.marginLeft := 10.px,
                ^.width := (20).px,
                ^.height := 1.px,
                ^.background := "#000",
                <.div(
                    ^.position.absolute,
                    ^.width := 12.px,
                    ^.height := 1.px,
                    ^.top := (4).px,
                    ^.left := (-1).px,
                    ^.background := "#000",
                    ^.transform := "rotate(30deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 12.px,
                    ^.height := 1.px,
                    ^.top := (4).px,
                    ^.right := (-1).px,
                    ^.background := "#000",
                    ^.transform := "rotate(-30deg)"
                )
            )
        )

    val zheRight =
        <.div(
            ^.position.absolute,
            ^.backgroundColor := "black",
            ^.width := 80.px,
            ^.height := 2.px,
            <.div(
                ^.position.absolute,
                ^.width := 2.px,
                ^.height := 240.px,
                ^.backgroundColor := "black",
                ^.marginLeft := 80.px,
                ^.marginTop := (-238).px,
                <.div(
                    ^.position.absolute,
                    ^.backgroundColor := "black",
                    ^.width := 80.px,
                    ^.height := 2.px,
                    ^.marginTop := 0.px,
                    ^.marginLeft := (-80).px,
                    <.div(
                        ^.position.absolute,
                        ^.width := 10.px,
                        ^.height := 2.px,
                        ^.marginLeft := 0.px,
                        ^.marginTop := 3.px,
                        ^.backgroundColor := "red",
                        ^.transform := "rotate(30deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 10.px,
                        ^.height := 2.px,
                        ^.marginLeft := 0.px,
                        ^.marginTop := (-3).px,
                        ^.backgroundColor := "blue",
                        ^.transform := "rotate(-30deg)"
                    )
                )
            )
        )

    val Zhe =
        <.div(
            ^.position.absolute,
            ^.width := 60.px,
            ^.height := 2.px,
            ^.backgroundColor := "black",
            <.div(
                ^.position.absolute,
                ^.width := 2.px,
                ^.height := 240.px,
                ^.marginLeft := 0.px,
                ^.marginTop := 0.px,
                ^.backgroundColor := "black",
                <.div(
                    ^.position.absolute,
                    ^.width := 60.px,
                    ^.height := 2.px,
                    ^.marginTop := 240.px,
                    ^.marginLeft := 0.px,
                    ^.backgroundColor := "black",
                    <.div(
                        ^.position.absolute,
                        ^.width := 2.px,
                        ^.height := 10.px,
                        ^.top := (-2).px,
                        ^.left := 54.px,
                        ^.background := "red",
                        ^.transform := "rotate(60deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 2.px,
                        ^.height := 10.px,
                        ^.top := (-6).px,
                        ^.left := (54).px,
                        ^.background := "blue",
                        ^.transform := "rotate(-60deg)"
                    )
                )
            )
        )
    val ArrowM =
        <.div(
            ^.position.absolute,
            ^.width := 2.px,
            ^.height := 30.px,
            ^.backgroundColor := "black",
            <.div(
                ^.position.absolute,
                ^.width := 120.px,
                ^.height := 2.px,
                ^.marginTop := 30.px,
                ^.marginLeft := (-60).px,
                ^.backgroundColor := "black"
            )
        )
    val Arrow =
        <.div(
            ^.position.relative,
            ^.width := 120.px,
            ^.height := 2.px,
            ^.background := "#000",
            ^.transformOrigin := "top left",
            ^.transform := "rotate(-90deg)",
            <.div(
                ^.position.absolute,
                ^.width := 10.px,
                ^.height := 2.px,
                ^.top := (2).px,
                ^.left := (0).px,
                ^.background := "#000",
                ^.transform := "rotate(20deg)"
            ),
            <.div(
                ^.position.absolute,
                ^.width := 10.px,
                ^.height := 2.px,
                ^.top := (-2).px,
                ^.left := (0).px,
                ^.background := "#000",
                ^.transform := "rotate(-20deg)"
            ),
        )
    val Choice =
        <.div(
            ^.position.relative,
            ^.display.flex,
            ^.width := 80.px,
            ^.height := 80.px,
            // ^.backgroundColor := "red",
            ^.border := "solid 2px black",
            //^.transform := "rotate(45deg)",
            // ^.transformOrigin := "left"
            ^.transform := "scale(0.3)"
            //      <.div(
            //        "te ",
            //        ^.width := 20.px,
            //        ^.height:= 20.px,
            //        ^.backgroundColor := "pink"
            //      ),
            //      <.div(
            //        "te",
            //        ^.marginLeft := 5.px,
            //        ^.width := 20.px,
            //        ^.height:= 20.px,
            //        ^.backgroundColor := "pink"
            //      ),
            //      <.div(
            //        "te",
            //        ^.marginLeft := 5.px,
            //        ^.width := 20.px,
            //        ^.height:= 20.px,
            //        ^.backgroundColor := "pink"
            //      )
        )

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

        def dragState(e: ReactMouseEventFromHtml): Callback = {
            e.persist()

            var x = e.pageX.toInt
            var y = e.pageY.toInt
            dom.console.info(x, y)
            if (x == 0 && y == 0)
                return e.preventDefaultCB
            x -= offsetx
            y -= offsety
            e.preventDefaultCB >> $.modState(s => s.copy(x, y))
        }

        def handleKey(e: ReactKeyboardEvent): Callback = {

            def plainKey: CallbackOption[Unit] = // CallbackOption will stop if a key isn't matched
                CallbackOption.keyCodeSwitch(e) {
                    case KeyCode.Up => move(0, -1)
                    case KeyCode.Down => move(0, 1)
                    case KeyCode.Left => move(-1, 0)
                    case KeyCode.Right => move(1, 0)
                }

            def ctrlKey: CallbackOption[Unit] = // Like above but if ctrlKey is pressed
                CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
                    case KeyCode.Up => move(0, -OuterY)
                    case KeyCode.Down => move(0, OuterY)
                    case KeyCode.Left => move(-OuterX, 0)
                    case KeyCode.Right => move(OuterX, 0)
                }

            (plainKey orElse ctrlKey) >> e.preventDefaultCB // This is the interesting part.
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
                    ^.top := s.y.px
                ),
                GenericAction(
                    ^.left := 100.px,
                    ^.top := 100.px
                ),
                //        ReceiveAction(
                //          ^.left := 150.px,
                //          ^.top  := (-100).px
                //        ),
                //SendAction(
                NewSendS(
                    ^.left := 400.px,
                    ^.top := (-50).px
                ),
                zheRight(
                    ^.left := 400.px,
                    ^.top := 400.px
                ),
                ArrowM(
                    ^.left := 300.px,
                    ^.top := (300).px
                ),
                ModelJoin(
                    ^.left := 300.px,
                    ^.top := (-200).px
                ),
                Circle(
                    ^.left := 500.px,
                    ^.top := (300).px
                ),
                //        Macro(
                //          ^.left := 500.px,
                //          ^.top  := (-500).px
                //        ),
                NewReceiveS(
                    ^.left := 150.px,
                    ^.top := (-500).px
                )

            )
    }

    val Main = ScalaComponent.builder[Unit]("Test Example")
            .initialState(initState)
            .renderBackend[Backend]
            .componentDidMount(_.backend.init)
            .build

    // EXAMPLE:END
}