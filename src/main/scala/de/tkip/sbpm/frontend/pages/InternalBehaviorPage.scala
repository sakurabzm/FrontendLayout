package de.tkip.sbpm.frontend.pages

import de.tkip.sbpm.frontend.graph.{Graph, StateGraph}
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^.{^, _}
import org.scalajs.dom

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLImageElement

object InternalBehaviorPage {

  object Style extends StyleSheet.Inline {

    import dsl._

    val container = style(
      minHeight(600.px),
      width :=! "100%",
      display.flex,
      //overflowX.scroll,
      // backgroundColor :=! "#FA8072",
      borderTop :=! "1px solid rgb(223, 220, 200)",
      fontFamily :=! "Roboto, sans-serif"
    )

    val buttonStyle = style(
      border :=! "none",
      borderRadius :=! "5px 5px 5px 5px",
      width :=! 60.px,
      height :=! 30.px,
      marginTop :=! 10.px,
      marginLeft :=! 20.px,
      &.hover(backgroundColor :=! "#4682B4")
    )

    val buttonSt = style(
      borderRadius :=! "13em/3em",
      width :=! 60.px,
      height :=! 30.px,
      border :=! "1px solid transparent",
      fontSize :=! 12.px,
      &.hover(backgroundColor :=! "#4682B4")
    )
  }

  case class State(statesList: ListBuffer[StateGraph])

  var statesList: ListBuffer[StateGraph] = ListBuffer()
  var completedStateList: ListBuffer[StateGraph] = ListBuffer()
  var stateIndex = "Action"
  var isSelected: StateGraph = null
  var preState: StateGraph = null
  var preStateC: (Int, Int) = null // 只存当前state的前一个state的坐标
  var preModifyState: (Int, Boolean, (String, Boolean, String, String)) = null
  var flag = true // flag is true, state setting; flag is false, transition setting.
  val startInitialX = 680
  val startInitialY = 50
  var coordinateSet: Map[Int, (Int, Int)] = Map()
  var defaultStateList = new ArrayBuffer[StateGraph]()
  var offsetx, offsety = 0


  class Backend(val $: BackendScope[Unit, State]) {
    def render(s: State) = {
      <.div(
        Style.container,
        createMenu,
        createContent(s),
        if (flag) createRightNavS(s) else createRightNavT
      )
    }

    def createMenu = {
      <.div(
        ^.position.absolute,
        ^.backgroundColor := "#B0C4DE",
        ^.width := "100%",
        ^.height := 50.px,
        ^.marginTop := 0.px,
        ^.marginLeft := 0.px,
        ^.borderBottom := "solid 1px Silver",
        ^.float.left,
        ^.overflow.hidden,
        <.button(
          Style.buttonStyle,
          "Add",
          ^.onClick --> createCorrespondingState
        ),
        <.select(
          ^.name := "stateType",
          ^.id := "selectS",
          ^.marginTop := 10.px,
          ^.marginLeft := 20.px,
          ^.width := 100.px,
          ^.height := 30.px,
          <.option("Action"),
          <.option("Send"),
          <.option("Receive"),
          <.option("Model Join"),
          <.option("Model Split"),
          <.option("Merge"),
          <.option("Macro"),
          <.option("End"),
          //          <.option("Arrow"),
          ^.onChange ==> getOptionValue
        ),
        <.button(
          Style.buttonStyle,
          "Connect",
          ^.onClick --> connectState
        ),
        <.button(
          Style.buttonStyle,
          "Delete",
          ^.onClick --> deleteState
        ),
        <.select(
          ^.width := 150.px,
          ^.height := 30.px,
          ^.marginTop := 10.px,
          ^.marginLeft := 20.px,
          <.option("Please select layout"),
          <.option("from left to right"),
          <.option("from top to bottom")
        ),
        <.button(
          Style.buttonStyle,
          "Manual"
        ),
        <.button(
          Style.buttonStyle,
          "Clear",
          ^.onClick --> clearStates
        ),
        <.button(
          Style.buttonStyle,
          "Save"
        )
      )
    }

    def createContent(state: State) = {
      <.div(
        ^.position.relative,
        ^.display.flex,
        ^.width := "85%",
        ^.minHeight := 600.px,
        ^.maxHeight := 600.px,
        ^.marginTop := 50.px,
        ^.marginLeft := 0.px,
        //^.backgroundColor := "blue", // content color
        ^.overflowX.scroll,
        ^.overflowY.scroll,
        statesList.toTagMod { item =>
          val eventMap: Map[String, TagMod] = Map()
          eventMap += (item.onClickKey -> onClickEvent(item))
          eventMap += (item.onDragStartKey -> onDragStartEvent(item))
          eventMap += (item.onDragKey -> onDragEvent(item))
          item.content(eventMap)
        }
      )
    }

    /*
  create RightNav
   */
    def createRightNavS(s: State) = {
      <.div(
        ^.position.relative,
        ^.float.right,
        //^.display.flex,
        ^.width := "15%",
        ^.marginTop := 50.px,
        ^.marginRight := 0.px,
        ^.backgroundColor := "#E6E6E6",
        ^.overflowX.scroll,
        <.ul(
          ^.listStyle := "none",
          <.li(
            ^.fontSize := 20.px,
            "State Setting",
            ^.textAlign.center
          ),
          <.br,
          <.br,
          <.li(
            startStateCheckbox(),
            <.label(
              ^.marginLeft := 20.px,
              "start state"
            )
          ),
          <.br,
          <.li(
            "* Type : ",
            <.br,
            <.br,
            <.select(
              <.option("Action",
                if ((isSelected != null) && (isSelected.stateType == "Action")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Send",
                if ((isSelected != null) && (isSelected.stateType == "Send")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Receive",
                if ((isSelected != null) && (isSelected.stateType == "Receive")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Model Join",
                if ((isSelected != null) && (isSelected.stateType == "Model Join")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Model Split",
                if ((isSelected != null) && (isSelected.stateType == "Model Split")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Merge",
                if ((isSelected != null) && (isSelected.stateType == "Merge")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("Macro",
                if ((isSelected != null) && (isSelected.stateType == "Macro")) {
                  ^.selected := true
                } else ^.selected := false),
              <.option("End",
                if ((isSelected != null) && (isSelected.stateType == "End")) {
                  ^.selected := true
                } else ^.selected := false),
              ^.onChange ==> changeStateType(isSelected)
            )
          ),
          <.br,
          <.li(
            "* Message Type : ",
            <.br,
            <.br,
            <.textarea(
              ^.maxHeight := 60.px,
              ^.minHeight := 60.px,
              ^.maxWidth := 140.px,
              ^.minWidth := 140.px,
              if (isSelected != null)
                ^.value := isSelected.getMessageType
              else ^.value := "Input messageType",
              ^.onChange ==> changeMessageTypeValue(isSelected)

            )
          ),
          <.br,
          <.li(
            "Description : ",
            <.br,
            <.br,
            <.textarea(
              ^.maxHeight := 70.px,
              ^.minHeight := 70.px,
              ^.maxWidth := 140.px,
              ^.minWidth := 140.px,
              if (isSelected != null)
                ^.value := isSelected.getDescription
              else ^.value := "",
              ^.onChange ==> changeDescription(isSelected)
            )
          ),
          <.br,
          <.br,
          <.br,
          <.li(
            ^.float.left,
            <.button(
              Style.buttonSt,
              "update",
              ^.onClick --> updateState)),
          <.button(
            Style.buttonSt,
            ^.marginLeft := 25.px,
            "Cancel",
            ^.onClick --> cancelUpdate(isSelected))
        )

      )
    }

    def createRightNavT = {
      <.div(
        ^.position.relative,
        ^.display.flex,
        ^.width := "15%",
        ^.marginTop := 50.px,
        ^.marginLeft := 0.px,
        ^.backgroundColor := "#E6E6E6",
        <.ul(
          ^.listStyle := "none",
          <.li(
            ^.fontSize := 20.px,
            "Transition Setting",
            ^.textAlign.center
          ),
          <.br,
          <.br,
          <.li(
            "Message Type : ",
            <.br,
            <.select(
              <.option(
                "create new message type",
                ^.textOverflow.ellipsis),
              <.option("select message type")
            )
          ),
          <.br,
          <.br,
          <.li(
            "Text : ",
            <.br,
            <.input.text(^.defaultValue := "new message type")
          ),
          <.br,
          <.br,
          <.li(
            "Related Subject : ",
            <.br,
            <.select(
              <.option("please select related subject")
            )
          ),
          <.br,
          <.br,
          <.li(
            "Description : ",
            <.br,
            <.textarea(
              ^.maxHeight := 70.px,
              ^.minHeight := 70.px,
              ^.maxWidth := 140.px,
              ^.minWidth := 140.px,
            )
          ),
          <.br,
          <.br,
          <.br,
          <.button(Style.buttonSt, "update"),
          <.button(^.marginLeft := 25.px, Style.buttonSt, "Delete")
        )
      )
    }


    def getOptionValue(e: ReactEventFromInput): Callback = {
      stateIndex = e.target.value
      Callback()
    }

    var isFull = true

    def positionOfAdd(initialS: StateGraph) = {
      for (i <- defaultStateList.indices) {
        if (defaultStateList(i) == null) {
          defaultStateList(i) = initialS
          initialS.setCoordinate(10, i * 10)
          isFull = false
        }
        dom.console.info("sssssss  " + defaultStateList)
      }
      if (isFull) {
        defaultStateList += initialS
        initialS.setCoordinate(10, (defaultStateList.length) * 10)
      }
    }

    def createCorrespondingState: Callback = {
      stateIndex match {
        case "Action" => {
          val s = Graph.Action()
          dom.console.info("action action ")
          statesList += s
          positionOfAdd(s)
          $.modState(s => State(statesList))
        }
        case "Send" => {
          val s = Graph.Send()
          statesList += s
          positionOfAdd(s)
          $.modState(s => State(statesList))
        }
        case "Receive" => {
          val s = Graph.Receive()
          statesList += s
          positionOfAdd(s)
          $.modState(s => State(statesList))
        }
        //        case "Model Join" => {
        //          val s = Graph.ModelJoin()
        //          statesList += s
        //          $.modState(s => State(statesList))
        //        }
        //        case "Model Split" => {
        //          val s = Graph.ModelSplit()
        //          statesList += s
        //          $.modState(s => State(statesList))
        //        }
        //        case "Merge" => {
        //          val s = Graph.Merge()
        //          statesList += s
        //          $.modState(s => State(statesList))
        //        }
        //        case "Macro" => {
        //          val s = Graph.Macro()
        //          statesList += s
        //          $.modState(s => State(statesList))
        //        }
        case "End" => {
          val s = Graph.End()
          statesList += s
          positionOfAdd(s)
          $.modState(s => State(statesList))
        }
        //        case "Arrow" => { // 测试用
        //          val s = Graph.Arrow()
        //          statesList += s
        //          $.modState(s => State(statesList))
        //        }

      }

    }

    def clearStates: Callback = {
      statesList.clear()
      $.modState(s => State(statesList))
    }

    def deleteState: Callback = {
      statesList -= isSelected
      $.modState(s => State(statesList))
    }

    /*
    check isStartState
     */
    def startStateCheckbox() = {
      if (isSelected != null) {
        <.input.checkbox(
          ^.checked := isSelected.isStartState,
          ^.onChange --> setStartState
        )
      } else {
        <.input.checkbox(
          ^.checked := false
        )
      }
    }

    def changeStateType(s: StateGraph)(e: ReactEventFromInput): Callback = {
      val newST = e.target.value
      s.setStateType(newST)
      $.modState(s => s)
    }

    def setStartState: Callback = {
      if (isSelected.isStartState == true) {
        isSelected.setIsStartState(false)
      } else {
        isSelected.setIsStartState(true)
      }
      $.modState(s => s)
    }

    def changeMessageTypeValue(s: StateGraph)(e: ReactEventFromInput): Callback = {
      val msg = e.target.value
      s.setMessageType(msg)
      $.modState(s => s)
    }

    def changeDescription(s: StateGraph)(e: ReactEventFromInput): Callback = {
      val dsc = e.target.value
      s.setDescription(dsc)
      $.modState(s => s)
    }

    def onClickEvent(graph: StateGraph): TagMod = {
      ^.onClick --> selectedEvent(graph)
    }

    def selectedEvent(graph: StateGraph): Callback = {
      if (isSelected != null) {
        if (isSelected.ID == graph.ID) {
          isSelected.resetBorder
          //restoreStateDate(isSelected)
          isSelected = null
          preModifyState = null
        } else {
          isSelected.resetBorder
         // restoreStateDate(isSelected)
          isSelected = graph // current selected state
          preModifyState = (isSelected.ID, false,
            (isSelected.stateType, isSelected.isStartState, isSelected.messageType, isSelected.description))
          isSelected.changeBorder("solid 2px #191970")

          if ((preState == null) || (preStateC == null)) { // There is no preState to need to be connected.
          } else {
            isSelected.setCoordinate(preStateC._1, preStateC._2 + 120) // 120是箭头长度，常量
            coordinateSet += (isSelected.ID -> (preStateC._1, preStateC._2 + 120))
            defaultStateList -= isSelected
            val s = new Graph.Arrow(preState.sx, preState.sy, isSelected.sx, isSelected.sy)
            statesList += s
            preStateC = null
          }
        }
      } else {
        isSelected = graph
        // 记录state之前的数据
        preModifyState = (isSelected.ID, false,
          (isSelected.stateType, isSelected.isStartState, isSelected.messageType, isSelected.description))
        isSelected.changeBorder("solid 2px #191970")
      }
      $.modState(s => s)
    }

    def onDragStartEvent(graph: StateGraph): TagMod ={
      ^.onDragStart ==> dragStart(graph)
    }

    def dragStart(graph: StateGraph)(e: ReactMouseEvent): Callback = {
      offsetx = e.pageX.toInt - graph.sx
      offsety = e.pageY.toInt - graph.sy
      dom.console.info(s"start: $offsetx, $offsety")
      Callback()
    }

    def onDragEvent(graph: StateGraph): TagMod ={
      ^.onDrag ==> dragState(graph)
    }

    def dragState(graph: StateGraph)(e: ReactMouseEventFromHtml): Callback = {
      e.persist()

      var x = e.pageX.toInt
      var y = e.pageY.toInt
      if (x == 0 && y == 0)
        return e.preventDefaultCB
      x -= offsetx
      y -= offsety
      graph.setCoordinate(x, y)
//      dom.console.info("move to: " + x,y)
      e.preventDefaultCB >> $.modState(s => s)
    }

    def connectState: Callback = {
      preState = isSelected
      if (preState == null) {
        Callback.alert("Please select state first!")
      } else {
        if(defaultStateList.contains(preState)){
          dom.console.info(" 同在编辑区域，不连接  " + defaultStateList)
        }else{
          if (coordinateSet.contains(preState.ID)) {
            preStateC = coordinateSet(preState.ID)
            dom.console.info("This preState coordinate.  " + preStateC)
          }else dom.console.info("This state doesn't exist.")
        }
        Callback()
      }
    }

    def modifyState(s: String): StateGraph = {
      s match {
        case "Action" => Graph.Action()
        case "Receive" => Graph.Receive()
        case "Send" => Graph.Send()
        case "End" => Graph.End()
      }
    }

    def modifyStateContent(s: StateGraph): StateGraph = {
      val newSt = s.stateType
      val newState = modifyState(newSt)
      newState.setID(s.ID)
      newState.setMessageType(s.messageType)
      newState.setDescription(s.description)
      for (i <- statesList.indices) {
        if (statesList(i).ID == newState.ID)
          statesList.update(i, newState)
      }
      // 选中才变
      //newState.changeBorder("solid 2px #191970")

      newState
    }

    var existStart = false
    var lastCoordinate: (Int, Int) = null

    def updateState: Callback = {
      if (isSelected.ID == preModifyState._1) { // 确保是同一state
        if (isSelected.isStartState) { //起始点
          if (!existStart) {
            isSelected.setCoordinate(startInitialX, startInitialY)
            existStart = true
            //确保同一state数据一致
            val data = (isSelected.getStateType, isSelected.getIsStartState, isSelected.getMessageType, isSelected.getDescription)
            preModifyState = (isSelected.ID, true, data)
            //从defaultStateList删除更新好的state
            defaultStateList -= isSelected
            dom.console.info("sjfakf " + defaultStateList)
            coordinateSet += (isSelected.ID -> (startInitialX, startInitialY))
            lastCoordinate = (startInitialX, startInitialY)

          } else {
            //TODO
            dom.console.info("我不是第一个起始点")
            isSelected.setCoordinate(600, startInitialY) // 已存在起始点，并排出现， 暂不考虑
          }
          $.modState(s => s)
        } else { // 非起始点
          if (coordinateSet.size != 0) {
            val data = (isSelected.getStateType, isSelected.getIsStartState, isSelected.getMessageType, isSelected.getDescription)
            preModifyState = (isSelected.ID, true, data)
            $.modState(s => s)
          } else Callback.alert("Please select StartState!")
        }
      } else Callback()








      //      //确保同一state数据一致
      //      val data = (isSelected.getStateType, isSelected.getIsStartState, isSelected.getMessageType, isSelected.getDescription)
      //      preModifyState = (isSelected.ID, true, data)
      //      //从stateList中删除已编辑好的state，并在已完成的completedList中添加当前状态
      //      statesList -= isSelected
      //      completedStateList += isSelected


      //      if (isSelected.ID == preModifyState._1) { // 确保是同一state
      //        if ((isSelected.isStartState)) {
      //          // 是起始(isStartState = true)
      //          if (isSelected.stateType == preModifyState._3._1) { // 位置变，形状不变 stateType 不变
      //            isSelected.setCoordinate(startInitialX, startInitialY) // 给与并记录初始位置
      //          }
      //          else { // 位置变，形状也变
      //            val newStateContent = modifyStateContent(isSelected)
      //            newStateContent.setCoordinate(startInitialX, startInitialY)
      //            dom.console.info("current state " + isSelected.stateType)
      //          }
      //        } else { //TODO 位置不变，形状变. 形状变写成方法
      //          val newStateContent = modifyStateContent(isSelected)
      //          if (coordinateSet.contains(newStateContent.ID)) {
      //            val i = coordinateSet(newStateContent.ID)
      //            newStateContent.setCoordinate(i._1, i._2)
      //          }
      //        }
      //
      //
      //        // state type 不变的情况下
      //        val data = (isSelected.getStateType, isSelected.getIsStartState, isSelected.getMessageType, isSelected.getDescription)
      //        preModifyState = (isSelected.ID, true, data)
      //
      //
      //      } else dom.console.info("error control")

      //  $.modState(s => s)
    }

    def cancelUpdate(s: StateGraph): Callback = {
      restoreStateDate(s)
      $.modState(s => s)
    }

    def restoreStateDate(s: StateGraph) = {
      if (s.ID == preModifyState._1) {
        if (!preModifyState._2) {
          val oldData = preModifyState._3
          s.setStateType(oldData._1)
          s.setIsStartState(oldData._2)
          s.setMessageType(oldData._3)
          s.setDescription(oldData._4)
        } else {
          dom.console.info("PreState is updated!")
        }
      } else {
        dom.console.info("different state")
      }
    }

    /*
    test function
     */
    def testA(graph: StateGraph): Callback = {
      Callback.alert("ok  x " + width + " ok y" + height)
    }

    val width = (dom.window.innerWidth).toInt
    val height = (dom.window.innerHeight).toInt
  }


  val component = ScalaComponent.builder[Unit]("InternalBehavior")
    .initialState(State(ListBuffer()))
    .renderBackend[Backend]
    .build

  def apply() = component().vdomElement

}
