package de.tkip.sbpm.frontend.graph

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scala.collection.mutable.Map

sealed abstract class StateGraph() {
  def content(eventMap: Map[String, TagMod]): TagMod
  var ID: Int
  var stateType: String
  var isStartState = false
  var messageType: String
  var description: String
  var border: String = "solid 2px #B0C4DE"
  val onClickKey = "1"
  val onDragStartKey = "2"
  val onDragKey = "3"

  def changeBorder(b: String): Unit = {
    border = b
  }

  def resetBorder = {
    border = "solid 2px #B0C4DE"
  }

  def setID(id: Int) = {
    ID = id
  }

  def getStateType: String = {
    stateType
  }

  def setStateType(st: String) = {
    stateType = st
  }

  def getIsStartState: Boolean = {
    isStartState
  }

  def setIsStartState(ss: Boolean) = {
    isStartState = ss
  }

  def getMessageType: String = {
    messageType
  }

  def setMessageType(msgT: String) = {
    messageType = msgT
  }

  def getDescription: String = {
    description
  }

  def setDescription(comment: String) = {
    description = comment
  }

  /*
  coordinate(x, y)
   */
  var sx = 0
  var sy = 0

  def setCoordinate(x: Int, y: Int) = {
    sx = x
    sy = y
  }

  override def equals(o: Any) = o match {
    case that: StateGraph => that.ID == this.ID
    case _ => false
  }
}

/*
Graph
 */
object Graph {
  var sId = 0

  def setId: Int = {
    sId += 1
    sId
  }

  case class Receive() extends StateGraph {
    override var stateType = "Receive"
    override var ID: Int = setId
    override var messageType = "Receive state test"
    override var description = "Receive receive receive"

    def content(eventMap: Map[String, TagMod]) = {
      <.div(
        // ^.transform := "scale(0.8)",
        ^.left := sx.px,
        ^.top := sy.px,
        ^.position.absolute,
        ^.width := 40.px,
        ^.height := 40.px,
        ^.borderRadius := 40.px,
        ^.border := border,
        ^.backgroundColor := "#B0C4DE",
        ^.draggable := true,
        <.div(
          "R",
          ^.fontFamily := "Roboto, sans-serif",
          ^.fontSize := 14.px,
          ^.textAlign.center,
          ^.fontWeight.bold,
          ^.position.absolute,
          ^.width := 20.px,
          ^.height := 15.px,
          ^.margin.auto,
          ^.top := 10.px,
          ^.left := 0.px,
          ^.right := 0.px,
          ^.bottom := 0.px,
          ^.borderTop := "solid 1px #000",
          ^.borderBottom := "solid 1px #000",
          ^.borderLeft := "solid 1px #000 ",
          ^.borderRight := "solid 1px #000 "
        ),
        <.div(
          ^.position.relative,
          ^.marginTop := (12).px,
          ^.marginLeft := 2.px,
          ^.width := (36.4).px,
          ^.height := 1.px,
          ^.background := "#000",
          ^.transform := "scale(0.8)",
          <.div(
            ^.position.absolute,
            ^.width := 21.px,
            ^.height := 1.px,
            ^.top := (5).px,
            ^.left := (-2).px,
            ^.background := "#000",
            ^.transform := "rotate(30deg)"
          ),
          <.div(
            ^.position.absolute,
            ^.width := 21.px,
            ^.height := 1.px,
            ^.top := (5).px,
            ^.right := (-2).px,
            ^.background := "#000",
            ^.transform := "rotate(-30deg)"
          )
        ),
        eventMap(onClickKey),
        eventMap(onDragStartKey),
        eventMap(onDragKey)
      )
    }

    def test(): TagMod = {
      dom.console.info("7891484894849" + border)
      ""
    }

  }

  case class Send() extends StateGraph {
    override var stateType = "Send"
    override var ID: Int = setId
    override var messageType = "send state test"
    override var description = "send send send"

    def content(eventMap: Map[String, TagMod]) = {
      <.div(
        ^.position.absolute,
        ^.left := sx.px,
        ^.top := sy.px,
        ^.width := 40.px,
        ^.height := 40.px,
        ^.borderRadius := 40.px,
        ^.border := border,
        ^.backgroundColor := "#B0C4DE",
        ^.draggable := true,
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
            ^.fontFamily := "Roboto, sans-serif",
            ^.fontSize := 14.px,
            ^.textAlign.center,
            ^.fontWeight.bold,
            ^.marginTop := (-3).px
          )
        ),
        <.div(
          ^.position.relative,
          ^.marginTop := (25).px,
          ^.marginLeft := 2.px,
          ^.width := (36.4).px,
          ^.height := 1.px,
          ^.background := "#000",
          ^.transform := "scale(0.65)",
          <.div(
            ^.position.absolute,
            ^.width := 21.px,
            ^.height := 1.px,
            ^.top := (5).px,
            ^.left := (-2).px,
            ^.background := "#000",
            ^.transform := "rotate(30deg)"
          ),
          <.div(
            ^.position.absolute,
            ^.width := 21.px,
            ^.height := 1.px,
            ^.top := (5).px,
            ^.right := (-2).px,
            ^.background := "#000",
            ^.transform := "rotate(-30deg)"
          )
        ),
        eventMap(onClickKey),
        eventMap(onDragStartKey),
        eventMap(onDragKey)
      )
    }
  }


  case class Action() extends StateGraph {
    override var stateType = "Action"
    override var ID: Int = setId
    override var messageType = "Action state test"
    override var description = "Action Action Action"

    def content(eventMap: Map[String, TagMod]) = {
      <.div(
        "Action",
        ^.position.absolute,
        ^.left := sx.px,
        ^.top := sy.px,
        ^.width := 60.px,
        ^.height := 40.px,
        ^.borderRadius := 6.px,
        ^.border := border,
        ^.textAlign.center,
        ^.lineHeight := 40.px,
        ^.backgroundColor := "#B0C4DE",
        ^.draggable := true,
        eventMap(onClickKey),
        eventMap(onDragStartKey),
        eventMap(onDragKey)
      )
    }

  }


  case class End() extends StateGraph {

    override var stateType = "End"
    override var ID: Int = setId
    override var messageType = "End state test"
    override var description = "End End End"

    def content(eventMap: Map[String, TagMod]) = {
      <.div(
        ^.position.absolute,
        ^.left := sx.px,
        ^.top := sy.px,
        ^.width := 40.px,
        ^.height := 40.px,
        ^.borderRadius := 40.px,
        ^.border := "solid 5px #000",
        ^.background := "#B0C4DE",
        ^.draggable := true,
        eventMap(onClickKey),
        eventMap(onDragStartKey),
        eventMap(onDragKey)
      )
    }

  }

  //
  //  case class ModelJoin() extends StateGraph {
  //    val content =
  //      <.div(
  //        ^.position.relative,
  //        ^.width := 40.px,
  //        ^.height := 40.px,
  //        ^.borderRadius := 40.px,
  //        ^.borderTop := "solid 1px #000",
  //        ^.borderBottom := "solid 1px #000",
  //        ^.borderLeft := "solid 1px #000 ",
  //        ^.borderRight := "solid 1px #000 ",
  //        ^.background := "#58ACFA",
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.top := 8.px,
  //          ^.left := 8.px,
  //          ^.borderTop := "solid 2px #000",
  //          ^.borderBottom := "solid 2px #000",
  //          ^.borderLeft := "solid 2px #000 ",
  //          ^.borderRight := "solid 2px #000 ",
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := 0.px,
  //            ^.right := 0.px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(45deg)"
  //          ),
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := (0).px,
  //            ^.right := (0).px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(135deg)"
  //          )
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.top := 8.px,
  //          ^.right := 8.px,
  //          ^.borderTop := "solid 2px #000",
  //          ^.borderBottom := "solid 2px #000",
  //          ^.borderLeft := "solid 2px #000 ",
  //          ^.borderRight := "solid 2px #000 ",
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := 0.px,
  //            ^.right := 0.px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(45deg)"
  //          ),
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := (0).px,
  //            ^.right := (0).px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(135deg)"
  //          )
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.bottom := 8.px,
  //          ^.left := 8.px,
  //          ^.borderTop := "dotted 2px #000",
  //          ^.borderBottom := "dotted 2px #000",
  //          ^.borderLeft := "dotted 2px #000 ",
  //          ^.borderRight := "dotted 2px #000 ",
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := 0.px,
  //            ^.right := 0.px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(45deg)"
  //          ),
  //          <.div(
  //            ^.position.absolute,
  //            ^.width := 8.px,
  //            ^.height := 2.px,
  //            ^.margin.auto,
  //            ^.bottom := (0).px,
  //            ^.right := (0).px,
  //            ^.top := (0).px,
  //            ^.left := (-1).px,
  //            ^.background := "#000",
  //            ^.transform := "rotate(135deg)"
  //          )
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.bottom := 8.px,
  //          ^.right := 8.px,
  //          ^.borderTop := "dotted 2px #000",
  //          ^.borderBottom := "dotted 2px #000",
  //          ^.borderLeft := "dotted 2px #000 ",
  //          ^.borderRight := "dotted 2px #000 "
  //        )
  //      )
  //  }
  //
  //  case class ModelSplit() extends StateGraph {
  //    val content =
  //      <.div(
  //        ^.position.relative,
  //        ^.width := 40.px,
  //        ^.height := 40.px,
  //        ^.borderRadius := 40.px,
  //        ^.borderTop := "solid 1px #000",
  //        ^.borderBottom := "solid 1px #000",
  //        ^.borderLeft := "solid 1px #000 ",
  //        ^.borderRight := "solid 1px #000 ",
  //        ^.background := "#58ACFA",
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.top := 8.px,
  //          ^.left := 8.px,
  //          ^.borderTop := "solid 2px #000",
  //          ^.borderBottom := "solid 2px #000",
  //          ^.borderLeft := "solid 2px #000 ",
  //          ^.borderRight := "solid 2px #000 "
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.top := 8.px,
  //          ^.right := 8.px,
  //          ^.borderTop := "solid 2px #000",
  //          ^.borderBottom := "solid 2px #000",
  //          ^.borderLeft := "solid 2px #000 ",
  //          ^.borderRight := "solid 2px #000 "
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.bottom := 8.px,
  //          ^.right := 8.px,
  //          ^.borderTop := "dotted 2px #000",
  //          ^.borderBottom := "dotted 2px #000",
  //          ^.borderLeft := "dotted 2px #000 ",
  //          ^.borderRight := "dotted 2px #000 "
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 6.px,
  //          ^.height := 6.px,
  //          ^.bottom := 8.px,
  //          ^.left := 8.px,
  //          ^.borderTop := "dotted 2px #000",
  //          ^.borderBottom := "dotted 2px #000",
  //          ^.borderLeft := "dotted 2px #000 ",
  //          ^.borderRight := "dotted 2px #000 "
  //        )
  //      )
  //  }
  //
  //  case class Merge() extends StateGraph {
  //    val content =
  //      <.div(
  //        ^.position.relative,
  //        ^.width := 40.px,
  //        ^.height := 40.px,
  //        ^.borderRadius := 40.px,
  //        ^.borderTop := "solid 1px #000",
  //        ^.borderBottom := "solid 1px #000",
  //        ^.borderLeft := "solid 1px #000 ",
  //        ^.borderRight := "solid 1px #000 ",
  //        ^.background := "#58ACFA"
  //      )
  //  }
  //
  //  case class Macro() extends StateGraph {
  //    val content =
  //      <.div(
  //        "Macro:",
  //        ^.fontWeight.bolder,
  //        ^.textAlign.center,
  //        ^.display.`table-cell`,
  //        ^.verticalAlign.middle,
  //        ^.position.relative,
  //        ^.width := 90.px,
  //        ^.height := 60.px,
  //        ^.borderRadius := 6.px,
  //        ^.borderTop := "solid 1px #000",
  //        ^.borderBottom := "solid 1px #000",
  //        ^.borderLeft := "solid 1px #000 ",
  //        ^.borderRight := "solid 1px #000 ",
  //        ^.background := "#58ACFA"
  //      )
  //  }

  //  case class Arrow(_x1: Int, _y1: Int, _x2: Int, _y2: Int) extends StateGraph {
  //    override var stateType = "Arrow"
  //    override val ID: Int = setId
  //    override var messageType = ""
  //    override var description = ""
  //    var x1 = _x1
  //    var y1 = _y1
  //    var x2 = _x2
  //    var y2 = _y2
  //    var length = Math.sqrt(Math.pow(Math.abs(x1 - x2), 2) + Math.pow(Math.abs(y1 - y2), 2))
  //    var rdeg = Math.atan2(x1 - x2, y1 - y2) * (180 / Math.PI)
  //
  //    def setSourceCoordinate(x: Int, y: Int): Unit = {
  //      x1 = x
  //      y1 = y
  //    }
  //
  //    def setSinkCoordinate(x: Int, y: Int): Unit = {
  //      x2 = x
  //      y2 = y
  //    }
  //
  //    def content(callback: () => Callback) = {
  //      <.div(
  //        ^.position.absolute,
  //        ^.top := (x1).px,
  //        ^.left := (y1).px,
  //        ^.width := length.px,
  //        ^.background := "#000",
  //        ^.transformOrigin := "0 0",
  //        ^.transform := s"rotate(${rdeg}deg)",
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 10.px,
  //          ^.height := 2.px,
  //          ^.top := (2).px,
  //          ^.left := (0).px,
  //          ^.background := "#000",
  //          ^.transform := s"rotate(${110 - rdeg}deg)"
  //        ),
  //        <.div(
  //          ^.position.absolute,
  //          ^.width := 10.px,
  //          ^.height := 2.px,
  //          ^.top := (-2).px,
  //          ^.left := (0).px,
  //          ^.background := "#000",
  //          ^.transform := s"rotate(${70 - rdeg}deg)"
  //        )
  //      )
  //    }
  //  }
  case class Arrow(_x1: Int, _y1: Int, _x2: Int, _y2: Int) extends StateGraph {
    override var stateType = "Receive"
    override var ID: Int = setId
    override var messageType = "test test test"
    override var description = "test test test"
    var x1 = _x1
    var y1 = _y1
    var x2 = _x2
    var y2 = _y2
    var length = Math.sqrt(Math.pow(Math.abs(x1-x2),2) + Math.pow(Math.abs(y1-y2),2)).toInt
    var angle = (Math.atan((x1 - x2)/(y1 - y2)) * (180/Math.PI))
    dom.console.info(x1,y1,x2,y2,length,angle)


    def content(eventMap: Map[String, TagMod]) = {
      <.div(
        ^.position.absolute,
        ^.width := 2.px,
        ^.height := length.px,
        ^.left := x1.px,
        ^.top := y1.px,
        ^.background := "#000",
        ^.transformOrigin := "top left",  // 基点角度不对
        ^.transform := s"rotate(${angle * -1}deg)",
        <.div(
          ^.position.absolute,
          ^.width := 2.px,
          ^.height := 10.px,
          ^.top := (length -10).px,
          ^.left := (3).px,
          ^.background := "blue",
          ^.transform := "rotate(30deg)"
        ),
        <.div(
          ^.position.absolute,
          ^.width := 2.px,
          ^.height := 10.px,
          ^.top := (length -10).px,
          ^.left := (-3).px,
          ^.background := "#DC143C",
          ^.transform := "rotate(-30deg)"
        )
      )
    }
  }

}

