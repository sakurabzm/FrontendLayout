package de.tkip.sbpm.frontend.pages

import de.tkip.sbpm.frontend.Data.{ProcessData, ProcessManager}
import de.tkip.sbpm.frontend.components.{LoadButtonComponents, SaveButtonInProcessPage}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^.{^, _}
import org.scalajs.dom
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import scala.collection.mutable.ListBuffer

object ListProcesses {


    var selectedP: ProcessData = null

    case class State(processes: ListBuffer[ProcessData], isVisible: String)

    object Style extends StyleSheet.Inline {

        import dsl._

        val processStyle = style(
            position.relative,
            display.flex,
            margin.`0`,
            width :=! "100%",
            &.hover(color :=! "Tomato")
        )
        val labelStyle = style(
            border :=! "none",
            borderRadius :=! "5px 5px 5px 5px",
            width :=! 60.px,
            height :=! 30.px,
            marginTop :=! 10.px,
            marginLeft :=! 20.px,
            &.hover(backgroundColor :=! "#4682B4")
        )

        val buttonS = style(
            borderRadius :=! "13em/3em",
            width :=! 60.px,
            height :=! 30.px,
            border :=! "solid 1px transparent",
            fontSize :=! 12.px,
            &.hover(backgroundColor :=! "#4682B4")
        )
    }

    var flag = true
    var genericDate = ""

    class Backend(val $: BackendScope[Unit, State]) {
        processList.clear()
        processList ++= ProcessManager.processMap.valuesIterator.toList

        def render(s: State) = {
            if (flag) {
                <.div(
                    ^.position.absolute,
                    ^.display.flex,
                    ^.margin := 0.px,
//                    ^.minHeight := "100vh",
                    ^.width := "85%",
                    ^.height := "100%",
                    <.div(
                        <.div(
                            ^.position.absolute,
                            ^.marginLeft := 0.px,
                            ^.width := "80%",
                            ^.height := 50.px,
                            ^.backgroundColor := "LightSteelBlue ",
                            <.button(
                                Style.labelStyle,
                                ^.width := "12%",
                                ^.color := "#006400",
                                ^.fontWeight.bold,
                                "New process",
                                ^.cursor.pointer,
                                ^.onClick --> cp()
                            ),
                            <.button(
                                Style.labelStyle,
                                ^.width := "12%",
                                ^.color := "#FF4500",
                                ^.fontWeight.bold,
                                "Delete",
                                ^.onClick --> deleteProcess(s: State)
                            ),
                            <.button(
                                Style.labelStyle,
                                ^.width := "12%",
                                ^.color := "#0000FF",
                                ^.fontWeight.bold,
                                "Management",
                                ^.cursor.pointer,
                                ^.onClick --> onVisibilityChange(s)
                            ),
                            SaveButtonInProcessPage.SaveButton().vdomElement,
                            LoadButtonComponents.getComponent($)
                        ),
                        <.div(
                            ^.position.absolute,
                            ^.marginLeft := 0.px,
                            ^.marginTop := 50.px,
                            ^.width := "80%",
                            ^.height := "83%",
                            ^.overflowX.scroll,
                            ^.overflowY.scroll,
                            <.ul(
                                ^.listStyle := "none",
                                s.processes.toTagMod { item =>
                                    <.li(
                                        Style.processStyle,
                                        <.div(
                                            ^.display.flex,
                                            processCheckbox(item, s),
                                            <.p(
                                                item.nameOfProcess,
                                                ^.alignSelf.center,
                                                ^.fontSize := 16.px,
                                                ^.fontWeight.bold,
                                                ^.fontFamily := "Sans-serif",
                                                ^.paddingLeft := 50.px,
                                                ^.cursor.pointer,
                                                ^.onClick --> selectedProcess(item),
                                                ^.onDoubleClick --> jumpLink(item.id)
                                            )
                                        )
                                    )
                                }
                            )
                        )
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.display.inline,
                        ^.marginLeft := "80%",
                        ^.backgroundColor := "#E6E6E6",
                        ^.width := "20%",
                        ^.height := "90%",
                        <.ul(
                            ^.listStyle := "none",
                            <.label(
                                "Setting Process: ",
                                ^.fontSize := 20.px,
                                ^.textAlign.center
                            ),
                            <.br,
                            <.br,
                            <.br,
                            <.br,
                            <.li(
                                "Process Name: ",
                                <.br,
                                <.br,
                                <.input.text(
                                    ^.width := 150.px,
                                    ^.maxLength := 100.px,
                                    ^.fontFamily := "sans-serif",
                                    if (selectedP != null)
                                        ^.value := selectedP.nameOfProcess
                                    else ^.value := "",
                                    ^.onChange ==> changeProcessName(selectedP)
                                )
                            ),
                            <.br,
                            <.br,
                            <.li(
                                "Process Author:",
                                <.br,
                                <.br,
                                <.input.text(
                                    ^.width := 150.px,
                                    ^.maxLength := 100.px,
                                    ^.fontFamily := "sans-serif",
                                    if (selectedP != null)
                                        ^.value := selectedP.authorOfProcess
                                    else ^.value := "",
                                    ^.onChange ==> changeProcessAuthor(selectedP)
                                )
                            ),
                            <.br,
                            <.br,
                            <.li(
                                "Description: ",
                                <.br,
                                <.br,
                                <.textarea(
                                    ^.fontFamily := "sans-serif",
                                    ^.minHeight := 100.px,
                                    ^.maxHeight := 100.px,
                                    ^.minWidth := 150.px,
                                    ^.maxWidth := 150.px,
                                    if (selectedP != null)
                                        ^.value := selectedP.descriptionOfProcess
                                    else ^.value := "",
                                    ^.onChange ==> changeProcessDescription(selectedP)
                                )
                            )
                        )
                    )
                )
            } else { // create new process
                <.div(
                    ^.position.relative,
                    ^.display.flex,
                    ^.marginLeft := "80%",
                    ^.width := "80%",
                    ^.height := "83%",
                    <.ul(
                        ^.listStyle := "none",
                        <.label(
                            "Create Process: ",
                            ^.fontSize := 20.px,
                            ^.textAlign.center
                        ),
                        <.br,
                        <.br,
                        <.br,
                        <.li(
                            ^.display.flex,
                            " * Process Author:  "
                        ),
                        <.input.text(
                            ^.display.flex,
                            ^.width := 200.px,
                            ^.maxLength := 100.px,
                            ^.onChange ==> getProcessAuthor
                        ),
                        <.br,
                        <.br,
                        <.li(
                            ^.display.flex,
                            " * Process Name:  "
                        ),
                        <.input.text(
                            ^.display.flex,
                            ^.width := 200.px,
                            ^.maxLength := 100.px,
                            ^.onChange ==> getProcessName
                        ),
                        <.br,
                        <.br,
                        <.li(
                            ^.display.flex,
                            "Description: "
                        ),
                        <.textarea(
                            ^.display.flex,
                            ^.fontFamily := "sans-serif",
                            ^.maxHeight := 100.px,
                            ^.minHeight := 100.px,
                            ^.maxWidth := 200.px,
                            ^.minWidth := 200.px,
                            ^.onChange ==> getDescription
                        ),
                        <.br,
                        <.br,
                        <.br,
                        <.button(
                            "confirm",
                            ^.onClick --> createProcess(s)
                        ),
                        <.button(
                            ^.marginLeft := 80.px,
                            "cancel",
                            ^.onClick --> backProcessList()
                        )
                    )
                )

            }
        }


        def cp(): Callback = {
            flag = false
            $.modState(s => State(s.processes, s.isVisible))
        }

        var deleteSet: Set[ProcessData] = Set()

        def onCheckedChange(item: ProcessData)(e: ReactEventFromInput): Callback = {
            if (!deleteSet.contains(item))
                deleteSet += item
            else deleteSet -= item
            $.modState(s => State(s.processes, s.isVisible))
        }

        def processCheckbox(pc: ProcessData, state: State) = {
            val on = deleteSet.contains(pc)
            //dom.console.info("1111111111111  " + on)
            <.input.checkbox(
                ^.alignSelf.center,
                ^.visibility := state.isVisible,
                ^.checked := on,
                ^.onChange ==> onCheckedChange(pc)
            )
        }


        def onVisibilityChange(s: State): Callback = {
            if (s.isVisible == "hidden") {
                $.modState(s => s.copy(processes = s.processes, "visible"))
            } else $.modState(s => s.copy(processes = s.processes, "hidden"))
        }

        def selectedProcess(p: ProcessData): Callback = {
            if (selectedP == null) {
                selectedP = p
            } else {
                selectedP = p
            }
            $.modState(s => State(s.processes, s.isVisible))
        }

        var currentProcessID: Int = 0

        def setProcessID(s: State): Int = {
            if (s.processes.nonEmpty) {
                currentProcessID = s.processes.maxBy(f => f.id.toInt).id.toInt + 1
            }
            currentProcessID
        }

        def createProcess(s: State): Callback = {
//            genericDate = new java.util.Date().toString
            val newID = setProcessID(s).toString
            val newItem = new ProcessData(newID, processAuthor, processName, processDescription)
            s.processes.+=:(newItem)
            ProcessManager.processMap += (newID -> newItem)
            flag = true
            $.modState(s => s)
        }

        def deleteProcess(s: State): Callback = {
            for (p <- deleteSet) {
                ProcessManager.processMap.remove(p.id)
            }
            s.processes --= deleteSet
            $.modState(s => State(s.processes, s.isVisible))
        }


        def backProcessList(): Callback = {
            flag = true
            $.modState(s => State(s.processes, s.isVisible))
        }

        var processName = "unnamed"

        def getProcessName(e: ReactEventFromInput): Callback = {
            processName = e.target.value
            Callback()
        }

        var processAuthor = "admin"

        def getProcessAuthor(e: ReactEventFromInput): Callback = {
            processAuthor = e.target.value
            Callback()
        }

        var processDescription = ""

        def getDescription(e: ReactEventFromInput): Callback = {
            processDescription = e.target.value
            Callback()
        }

        def changeProcessName(p: ProcessData)(e: ReactEventFromInput): Callback = {
            val newName = e.target.value
            p.setName(newName)
            $.modState(s => s)
        }

        def changeProcessAuthor(p: ProcessData)(e: ReactEventFromInput): Callback = {
            val newAuthor = e.target.value
            p.setAuthor(newAuthor)
            $.modState(s => s)
        }

        def changeProcessDescription(p: ProcessData)(e: ReactEventFromInput): Callback = {
            val newDescription = e.target.value
            p.setDescription(newDescription)
            $.modState(s => s)
        }


        def jumpLink(processId: String)= Callback {
            val url = "http://localhost:8080/#subjectsView/" + processId
            //      ////      dom.window.location.href = url
            dom.window.location.href = url
        }

    }
    val processList: ListBuffer[ProcessData] = ListBuffer()

    val component = ScalaComponent.builder[Unit]("ProcessList")
            .initialState(State(processList.sortBy(_.nameOfProcess), "hidden"))
            .renderBackend[Backend]
            .build


    def apply() = component().vdomElement
}
