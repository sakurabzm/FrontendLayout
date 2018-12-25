package de.tkip.sbpm.frontend.components.itmes

import japgolly.scalajs.react.vdom.html_<^.{^, _}
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import  de.tkip.sbpm.frontend.AppRouter.baseUrl

import scala.collection.mutable.ListBuffer
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterConfigDsl
import de.tkip.sbpm.frontend.Item
import de.tkip.sbpm.frontend.pages.ItemsPage
import japgolly.scalajs.react.vdom.VdomElement


object ProcessList {

  case class ItemOfProcess(id: Int, name: String)

  val a = ItemOfProcess(10, "process one")
  val b = ItemOfProcess(20, "process Two")
  val c = ItemOfProcess(30, "process Three")
  val d = ItemOfProcess(40, "process Four")
  val e = ItemOfProcess(50, "process Five")
  val f = ItemOfProcess(60, "process Six")
  val g = ItemOfProcess(70, "process Seven")
  val h = ItemOfProcess(80, "process Eight")
  val i = ItemOfProcess(90, "process Nine")
  val j = ItemOfProcess(100, "process Ten")
  var processList: ListBuffer[ItemOfProcess] = ListBuffer(a, b, c, d, e, f, g, h, i, j)


  case class State(processes: ListBuffer[ItemOfProcess], isVisible: String)

  // case class Props(processes: ListBuffer[ItemOfProcess])

  object Style extends StyleSheet.Inline {

    import dsl._

    val processStyle = style(
      position.relative,
      display.flex,
      margin.`0`,
      width :=! "100%",
      height :=! 30.px
    )
  }

  var flag = true

  class Backend(val $: BackendScope[Unit, State]) {
    def render(s: State) = {
      if (flag) {
        <.ul(
          //^.backgroundColor := "gold",
          ^.listStyle := "none",
          <.label(
            "Batch Management",
            ^.cursor.pointer,
            ^.onClick --> onVisibilityChange(s)),
          <.br,
          <.br,
          s.processes.toTagMod { item =>
            <.li(
              Style.processStyle,
              <.div(
                ^.display.flex,
                processCheckbox(item, s),
                <.p(
                  item.name,
                  ^.alignSelf.center,
                  ^.fontSize := 16.px,
                  ^.paddingLeft := 20.px,
                  ^.cursor.pointer,
                  ^.onDoubleClick --> jumpLink(item.name)
                )
              ),
            )
          },
          <.br,
          <.br,
          <.button(
            "Create",
            ^.onClick --> cp()
          ),
          <.button(
            "Delete",
            ^.marginLeft := 50.px,
            ^.onClick --> deleteProcess(s: State)
          )
        )
      } else {
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
            "Process Author:  "
          ),
          <.input.text(
            ^.display.flex,
            ^.width := 200.px,
            ^.maxLength := 100.px
          ),
          <.br,
          <.br,
          <.li(
            ^.display.flex,
            "Process Name:  "
          ),
          <.input.text(
            ^.display.flex,
            ^.width := 200.px,
            ^.maxLength := 100.px,
            ^.onChange ==> getProcessName
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
      }
    }


    def cp(): Callback = {
      flag = false
      $.modState(s => State(s.processes, s.isVisible))
    }

    var deleteSet: Set[ItemOfProcess] = Set()

    def onCheckedChange(item: ItemOfProcess)(e: ReactEventFromInput): Callback = {
      if (!deleteSet.contains(item))
        deleteSet += item
      else deleteSet -= item
      $.modState(s => State(s.processes, s.isVisible))
    }

    def processCheckbox(pc: ItemOfProcess, state: State) = {
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

    def createProcess(s: State): Callback = {
      val newID = s.processes.last.id + 1
      val newItem = ItemOfProcess(newID, processName)
      val newProcesses = s.processes :+ newItem
      flag = true
      $.modState(s => State(newProcesses, s.isVisible))
    }

    def deleteProcess(s: State): Callback = {
      s.processes --= deleteSet
      $.modState(s => State(s.processes, s.isVisible))
    }

    def backProcessList(): Callback = {
      flag = true
      $.modState(s => State(s.processes, s.isVisible))
    }

    var processName = ""

    def getProcessName(e: ReactEventFromInput): Callback = {
      processName = e.target.value
      Callback()
    }

    def jumpLink(processName: String): Callback= {
      Callback.alert("to do ")
    }

  }

  val component = ScalaComponent.builder[Unit]("ProcessList")
    .initialState(State(processList, "hidden"))
    .renderBackend[Backend]
    .build


  def apply() = component().vdomElement
}
