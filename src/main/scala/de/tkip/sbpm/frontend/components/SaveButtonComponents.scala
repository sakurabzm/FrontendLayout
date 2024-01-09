package de.tkip.sbpm.frontend.components

import de.tkip.sbpm.frontend.Data.{ProcessManager, StateData, Transform}
import de.tkip.sbpm.frontend.pages.InternalBehaviorPage.{State, processID, statesList, subjectID}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.{^, _}
import vdom.html_<^._
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react.{Callback, _}
import org.scalajs.dom

import scala.collection.mutable.ListBuffer
import scala.scalajs.js

object SaveButtonComponents {

    var pid, sid = ""
    var b: BackendScope[Unit, State] = null

    object Style extends StyleSheet.Inline {

        import dsl._


        val buttonStyle = style(
            border :=! "none",
            borderRadius :=! "5px 5px 5px 5px",
            width :=! 60.px,
            height :=! 30.px,
            marginTop :=! 10.px,
            marginLeft :=! 20.px,
            &.hover(backgroundColor :=! "#4682B4")
        )
    }


    import org.scalajs.dom.{WebSocket, MessageEvent, Event, CloseEvent}
    import scala.scalajs.js

    val url = "ws://localhost:8081/storeJson"

//    def addIntro(top: Key => TagMod, f: VdomTag => VdomTag) =
//        ScalaComponent.builder[Unit]("").render(_ =>
//            <.div(
//                f(<.div),
//                top(""))
//        ).build
//
//    lazy val main = addIntro(SaveButton.withKey(_)(), _ (
//        ""))

    //  case class SaveData(json: String, graphml: String)

    case class WebSocketState(ws: Option[WebSocket], logLines: Vector[String], message: String) {

        def allowSend: Boolean =
            ws.exists(_.readyState == WebSocket.OPEN)

        // Create a new state with a line added to the log
        def log(line: String): WebSocketState =
            copy(logLines = logLines :+ line)
    }

    class Backend($: BackendScope[Unit, WebSocketState]) {

        def render(s: WebSocketState) = {

            // Can only send if WebSocket is connected and user has entered text
            val send: Option[Callback] =
                for (ws <- s.ws if s.allowSend)
                    yield sendMessage(ws, s.message)

            <.button(
                ^.border := "none",
                ^.borderRadius := "5px 5px 5px 5px",
                ^.width := 60.px,
                ^.height := 30.px,
                ^.marginTop := 10.px,
                ^.marginLeft := 20.px,
                "Save",
                //          ^.disabled  := send.isEmpty, // Disable button if unable to send
                ^.onClick -->? send // --> suffixed by ? because it's for Option[Callback]
            )
        }

        def sendMessage(ws: WebSocket, msg: String): Callback = {

            //      Filed("D://myfile").pipe(WriteStream("/out"))
            // Send a message to the WebSocket

            def send = Callback(ws.send(Transform.saveProcess(ProcessManager.processMap)))
            send
        }

        def start: Callback = {

            // This will establish the connection and return the WebSocket
            def connect = CallbackTo[WebSocket] {

                // Get direct access so WebSockets API can modify state directly
                // (for access outside of a normal DOM/React callback).
                // This means that calls like .setState will now return Unit instead of Callback.
                val direct = $.withEffectsImpure

                // These are message-receiving events from the WebSocket "thread".

                def onopen(e: Event): Unit = {
                    // Indicate the connection is open
                    direct.modState(_.log("Connected."))
                }

                def onmessage(e: MessageEvent): Unit = {
                    ProcessManager.processMap = Transform.jsonToProcess(e.data.toString)
                    // Echo message received
                    //          direct.modState(_.log(s"Echo: ${e.data.toString}"))
                }

                def onerror(e: Event): Unit = {
                    // Display error message
                    val msg: String =
                        e.asInstanceOf[js.Dynamic]
                                .message.asInstanceOf[js.UndefOr[String]]
                                .fold(s"Error occurred!")("Error occurred: " + _)
                    direct.modState(_.log(msg))
                }

                def onclose(e: CloseEvent): Unit = {
                    // Close the connection
                    direct.modState(_.copy(ws = None).log(s"""Closed. Reason = "${e.reason}""""))
                }

                // Create WebSocket and setup listeners
                val ws = new WebSocket(url)
                ws.onopen = onopen _
                ws.onclose = onclose _
                ws.onmessage = onmessage _
                ws.onerror = onerror _
                ws
            }

            // Here use attempt to catch any exceptions in connect
            connect.attempt.flatMap {
                case Right(ws) => $.modState(_.log(s"Connecting to $url ...").copy(ws = Some(ws)))
                case Left(error) => $.modState(_.log(s"Error connecting: ${error.getMessage}"))
            }
        }

        def end: Callback = {
            def closeWebSocket = $.state.map(_.ws.foreach(_.close())).attempt

            def clearWebSocket = $.modState(_.copy(ws = None))

            closeWebSocket >> clearWebSocket
        }
    }


    val SaveButton = ScalaComponent.builder[Unit]("SaveButton")
            .initialState(WebSocketState(None, Vector.empty, ""))
            .renderBackend[Backend]
            .componentDidMount(_.backend.start)
            .componentWillUnmount(_.backend.end)
            .build
}
