package de.tkip.sbpm.frontend.graph

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.collection.mutable.{ListBuffer, Map, Set}
import de.tkip.sbpm.frontend.Data.{ProcessManager, StateData, TransitionData}
import de.tkip.sbpm.frontend.pages.InternalBehaviorPage
import japgolly.scalajs.react.vdom.html_<^
import scalacss.internal.Attrs.outline

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport


sealed abstract class Graph {
    val uuid = java.util.UUID.randomUUID().hashCode()
    //def content: TagMod
    var rpst = false

    override def hashCode = uuid

    override def equals(o: Any) = o match {
        case that: Graph => that.hashCode == this.hashCode
        case _ => false
    }
}

sealed abstract class StateGraph(processid: String, subjectid: String, stateData: StateData) extends Graph {
    def content(eventMap: Map[String, TagMod]): TagMod

    var border: String = "solid 2px #B0C4DE"
    val data = stateData
    var deep: Int = -1
    var arrowsToTarget: ListBuffer[Int] = ListBuffer()
    var arrowsFromSource: ListBuffer[Int] = ListBuffer()
    var graphWidth: Int
    var parentID: Int = -1
    var childrenList: ListBuffer[Int] = ListBuffer()
    var descendantList: ListBuffer[Int] = ListBuffer()
    var sign: Boolean = false
    var lock: Boolean = false
    var dragging: Boolean = false
    var currentBackgroundColor: String = "#B0C4DE"
    var spacex, spacey = 0

    def changeBorder = {
        border = "solid 2px #DC143C"
    }

    def startPointBorder = {
        border = "solid 2px green"
    }

    def changeBackgroundColor = {
        currentBackgroundColor = "#E3C800"
    }

    def isolatedNodeColor = {
        currentBackgroundColor = "#FA6800"
    }

    def resetBackgroundColor = {
        currentBackgroundColor = "#B0C4DE"
    }

    def resetBorder = {
        border = "solid 2px #B0C4DE"
    }

    def resetAllData: Unit = {
        sign = false
    }

    def setSpaceX(x: Int): Unit = {
        spacex = x
    }

    def setSpaceY(y: Int): Unit = {
        spacey = y
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
        case that: StateGraph => that.data.ID == this.data.ID
        case _ => false
    }

    override def toString: String = {
        s"id: ${data.id}, name: ${data.stateName}, arrows to target: $arrowsToTarget, arrows frome source: $arrowsFromSource"
    }

}


/*
Graph
 */
object GraphObject {

    object Style extends StyleSheet.Inline {

        import dsl._

        val textStyle = style(
            userSelect := "none",
            cursor.pointer
        )

    }


    var symbolCenter: Map[String, (Int, Int)] = Map(
        "Receive" -> (20, 20),
        "Send" -> (20, 20),
        "ModalJoin" -> (20, 20),
        "ModalSplit" -> (20, 20),
        "Action" -> (40, 20),
        "CloseIP" -> (40, 20),
        "Tau" -> (40, 20),
        "OpenIP" -> (40, 20),
        "CloseAllIPs" -> (40, 20),
        "OpenAllIPs" -> (40, 20),
        "SelectAgents" -> (40, 20),
        "CallMacro" -> (40, 20),
        "VarMan" -> (40, 20),
        "Cancel" -> (40, 20),
        "End" -> (22, 22),
        "IsIPEmpty" -> (31, 37)
    )

    val circle: ListBuffer[String] = ListBuffer("Send", "Receive", "ModalJoin", "ModalSplit")


    val onClickKey = "1"
    val onDragStartKey = "2"
    val onDragKey = "3"
    val onDoubleClickKey = "4"
    var sId = 0

    def setId: Int = {
        sId += 1
        sId
    }

    var transitionID = 100

    def setTransition: Int = {
        transitionID += 1
        transitionID
    }

    class Receive(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "Receive receive receive"
        override var graphWidth: Int = 40

        override def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.left := sx.px,
                ^.top := sy.px,
                ^.position.absolute,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.borderRadius := 40.px,
                ^.border := border,
                ^.backgroundColor := currentBackgroundColor,
                ^.draggable := true,
                <.div(
                    "R",
                    Style.textStyle,
                    ^.position.absolute,
                    ^.textAlign.center,
                    ^.fontWeight.bold,
                    //^.fontSize := 12.px,
                    ^.width := 20.px,
                    ^.height := 15.px,
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
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }

        //    def multiTransition() = {
        //      if(!dragging){
        //        if (this.childrenList.size > 1) {
        //          val firstChildID = this.childrenList.head
        //          val lastChildID = this.childrenList.last
        //          val firstChild = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(firstChildID)
        //          val lastChild = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(lastChildID)
        //          val firstChildCenter = symbolCenter(firstChild.data.stateType)
        //          val lastChildCenter = symbolCenter(lastChild.data.stateType)
        //          val firstChildX = firstChild.sx + firstChildCenter._1
        //          val lastChildX = lastChild.sx + lastChildCenter._1
        //          val currentStateCenter = symbolCenter(data.stateType)
        //          multiArrow(firstChildX, lastChildX, currentStateCenter, data.stateType, "1", this.sx)
        //        } else {
        //          if (this.childrenList.size == 1) {
        //            val uniqueChild = this.childrenList.head
        //            val child = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(uniqueChild)
        //            val childCenter = symbolCenter(child.data.stateType)
        //            val currentStateCenter = symbolCenter(data.stateType)
        //            if (math.abs((child.sx + childCenter._1) - (this.sx + currentStateCenter._1)) <= 30) {
        //              <.div()
        //            } else {
        //              multiArrow((this.sx + currentStateCenter._1), (child.sx + childCenter._1), currentStateCenter, data.stateType, "2", this.sx)
        //            }
        //          } else {
        //            <.div()
        //          }
        //        }
        //      }else{
        //        <.div()
        //      }
        //
        //    }
    }

    class Send(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "send send send"

        override var graphWidth: Int = 40

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                ^.width := graphWidth.px,
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
                        Style.textStyle,
                        ^.textAlign.center,
                        ^.fontWeight.bold,
                        ^.fontSize := 12.px,
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
                ),
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class Action(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "Action Action Action"
        override var graphWidth: Int = changeWidth(data.stateName)

        def changeWidth(n: String): Int = {
            if (n.length > 8) {
                n.length * 10
            } else {
                80
            }

        }

        def content(eventMap: Map[String, TagMod]) = {
            <.label(
                data.stateName,
                Style.textStyle,
                ^.fontSize := 12.px,
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                ^.width := changeWidth(data.stateName).px,
                ^.minWidth := 80.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.textAlign.center,
                ^.backgroundColor := "#B0C4DE",
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }


        //    def multiTransition() = {
        //      if(!dragging){
        //        if (this.childrenList.size > 1) {
        //          val firstChildID = this.childrenList.head
        //          val lastChildID = this.childrenList.last
        //          val firstChild = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(firstChildID)
        //          val lastChild = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(lastChildID)
        //          val firstChildCenter = symbolCenter(firstChild.data.stateType)
        //          val lastChildCenter = symbolCenter(lastChild.data.stateType)
        //          val firstChildX = firstChild.sx + firstChildCenter._1
        //          val lastChildX = lastChild.sx + lastChildCenter._1
        //          val currentStateCenter = symbolCenter(data.stateType)
        //          multiArrow(firstChildX, lastChildX, currentStateCenter, data.stateType, "1", this.sx)
        //        } else {
        //          if (this.childrenList.size == 1) {
        //            val uniqueChild = this.childrenList.head
        //            val child = ProcessList.processMap(processid).subjectMap(subjectid).stateMap(uniqueChild)
        //            val childCenter = symbolCenter(child.data.stateType)
        //            val currentStateCenter = symbolCenter(data.stateType)
        //            if (math.abs((child.sx + childCenter._1) - (this.sx + currentStateCenter._1)) <= 30) {
        //              <.div()
        //            } else {
        //              multiArrow((this.sx + currentStateCenter._1), (child.sx + childCenter._1), currentStateCenter, data.stateType, "2", this.sx)
        //            }
        //          } else {
        //            <.div()
        //          }
        //        }
        //      }else{
        //        <.div()
        //      }
        //
        //    }

    }

    class End(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

       data.description = "End End End"
        override var graphWidth: Int = 44

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                ^.width := 40.px,
                ^.height := 40.px,
                ^.borderRadius := 40.px,
                ^.border := "solid 4px #000",
                ^.background := "#B0C4DE",
                ^.draggable := true,
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class ModalJoin(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "Join Join"
        override var graphWidth: Int = 40

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.borderRadius := 40.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.top := 8.px,
                    ^.left := 8.px,
                    ^.border := "solid 2px #000",
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := 0.px,
                        ^.right := 0.px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(45deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := (0).px,
                        ^.right := (0).px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(135deg)"
                    )
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.top := 8.px,
                    ^.right := 8.px,
                    ^.border := "solid 2px #000",
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := 0.px,
                        ^.right := 0.px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(45deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := (0).px,
                        ^.right := (0).px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(135deg)"
                    )
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.bottom := 8.px,
                    ^.left := 8.px,
                    ^.border := "dotted 2px #000",
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := 0.px,
                        ^.right := 0.px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(45deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 8.px,
                        ^.height := 2.px,
                        ^.margin.auto,
                        ^.bottom := (0).px,
                        ^.right := (0).px,
                        ^.top := (0).px,
                        ^.left := (-1).px,
                        ^.background := "#000",
                        ^.transform := "rotate(135deg)"
                    )
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.bottom := 8.px,
                    ^.right := 8.px,
                    ^.border := "dotted 2px #000"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }

    }

    class ModalSplit(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "Split Split"
        override var graphWidth: Int = 40

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.borderRadius := 40.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.top := 8.px,
                    ^.left := 8.px,
                    ^.border := "solid 2px #000",
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.top := 8.px,
                    ^.right := 8.px,
                    ^.border := "solid 2px #000",
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.bottom := 8.px,
                    ^.right := 8.px,
                    ^.border := "dotted 2px #000",
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.bottom := 8.px,
                    ^.left := 8.px,
                    ^.border := "dotted 2px #000"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class Tau(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "Tau Tau Tau"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "Tau",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.position.absolute,
                Style.textStyle,
                ^.fontSize := 12.px,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.p(
                    "τ",
                    ^.position.absolute,
                    ^.marginTop := (-55).px,
                    ^.marginLeft := "83%"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class OpenIP(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "OpenIP OpenIP OpenIP"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "Open IP",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                Style.textStyle,
                ^.textAlign.center,
                ^.position.absolute,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "82%",
                    ^.marginTop := (-34).px,
                    ^.width := 9.px,
                    ^.height := 4.px,
                    ^.borderRadius := 1.px,
                    ^.border := "solid 1px black",
                    ^.backgroundColor := "black",
                    <.div(
                        ^.position.absolute,
                        ^.left := 1.px,
                        ^.top := (-6).px,
                        ^.width := 3.px,
                        ^.height := 3.px,
                        ^.borderRadius := "3px 3px 0 0 ",
                        ^.borderRight := "solid 2px black",
                        ^.borderLeft := "solid 2px black",
                        ^.borderTop := "solid 2px black",
                        <.div(
                            ^.position.absolute,
                            ^.marginLeft := 3.px,
                            ^.marginTop := (1).px,
                            ^.width := 2.px,
                            ^.height := 2.px,
                            ^.backgroundColor := "#B0C4DE"
                        )
                    )
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class CloseIP(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "CloseIP CloseIP CloseIP"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "Close IP",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                Style.textStyle,
                ^.position.absolute,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "82%",
                    ^.marginTop := (-34).px,
                    ^.width := 9.px,
                    ^.height := 4.px,
                    ^.borderRadius := 1.px,
                    ^.border := "solid 1px black",
                    ^.backgroundColor := "black",
                    <.div(
                        ^.position.absolute,
                        ^.left := 1.px,
                        ^.top := (-6).px,
                        ^.width := 3.px,
                        ^.height := 3.px,
                        ^.borderRadius := "3px 3px 0 0 ",
                        ^.borderRight := "solid 2px black",
                        ^.borderLeft := "solid 2px black",
                        ^.borderTop := "solid 2px black",
                    )
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class IsIPEmpty(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "IsIPEmpty IsIPEmpty IsIPEmpty"
        override var graphWidth: Int = 60


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.position.absolute,
                ^.left := sx.px,
                ^.top := sy.px,
                Style.textStyle,
                ^.width := graphWidth.px,
                ^.height := 60.px,
                ^.background := "#B0C4DE",
                ^.border := border,
                ^.transform := "rotate(45deg)",
                <.div(
                    "IsIPEmpty",
                    ^.fontSize := 12.px,
                    ^.position.absolute,
                    ^.textAlign.center,
                    ^.width := 60.px,
                    ^.height := 28.px,
                    ^.lineHeight := 28.px,
                    ^.marginTop := (16).px,
                    ^.marginLeft := 0.px,
                    ^.transform := "rotate(-45deg)",
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class CloseAllIPs(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "CloseAllIPs CloseAllIPs CloseAllIPs"
        override var graphWidth: Int = 80

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "CloseAllIPs",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                Style.textStyle,
                ^.position.absolute,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "85%",
                    ^.marginTop := (-38).px,
                    ^.width := 6.px,
                    ^.height := 6.px,
                    ^.borderRadius := 6.px,
                    ^.border := "solid 2px black",
                    <.div(
                        ^.position.absolute,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.marginTop := (2).px,
                        ^.backgroundColor := "black",
                        ^.transform := "rotate(-60deg)"
                    )
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }

    }

    class OpenAllIPs(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "OpenAllIPs OpenAllIPs OpenAllIPs"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "OpenAllIps",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                ^.position.absolute,
                Style.textStyle,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "90%",
                    ^.marginTop := (-38).px,
                    ^.width := 0.px,
                    ^.height := 0.px,
                    ^.borderTop := "5px solid transparent",
                    ^.borderBottom := "5px solid transparent",
                    ^.borderLeft := "5px solid black"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class SelectAgents(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "SelectAgents SelectAgents SelectAgents"
        override var graphWidth: Int = 80

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "SelectAgents",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.position.absolute,
                Style.textStyle,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                ^.fontSize := 12.px,
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "85%",
                    ^.marginTop := (-36).px,
                    ^.width := 0.px,
                    ^.height := 0.px,
                    ^.borderTop := "5px solid black",
                    ^.borderRight := "5px solid transparent",
                    ^.borderLeft := "5px solid transparent"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }
    }

    class CallMacro(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "CallMacro CallMacro CallMacro"
        override var graphWidth: Int = 80
        stateData.setStateName("test")


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "CallMacro",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                ^.position.absolute,
                Style.textStyle,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.width := 8.px,
                    ^.height := 2.px,
                    ^.borderRadius := 1.px,
                    ^.marginLeft := "85%",
                    ^.marginTop := (-36).px,
                    ^.border := "solid 1px black",
                    ^.backgroundColor := "black",
                    ^.transform := "rotate(-45deg)",
                    <.div(
                        ^.position.absolute,
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.marginTop := (-1).px,
                        ^.marginLeft := (-12).px,
                        ^.borderRight := "solid 5px black",
                        ^.borderLeft := "solid 5px transparent",
                        ^.borderTop := "solid 2px transparent",
                        ^.borderBottom := "solid 2px transparent",
                    )
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey),
                eventMap(onDoubleClickKey)
            )
        }
    }

    class VarMan(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "VarMan VarMan VarMan"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "VarMan", // 变量操作
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                ^.position.absolute,
                Style.textStyle,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.marginLeft := "85%",
                    ^.marginTop := (-36).px,
                    ^.width := 5.px,
                    ^.height := 2.px,
                    ^.backgroundColor := "black",
                    <.div(
                        ^.position.absolute,
                        ^.marginTop := 3.px,
                        ^.width := 5.px,
                        ^.height := 2.px,
                        ^.backgroundColor := "black",
                        <.div(
                            ^.position.absolute,
                            ^.marginTop := 3.px,
                            ^.width := 5.px,
                            ^.height := 2.px,
                            ^.backgroundColor := "black"
                        )
                    )
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }

    }

    class Cancel(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {

        data.description = "Cancel Cancel Cancel"
        override var graphWidth: Int = 80


        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                "Cancel",
                ^.left := sx.px,
                ^.top := sy.px,
                ^.fontSize := 12.px,
                ^.position.absolute,
                Style.textStyle,
                ^.textAlign.center,
                ^.width := graphWidth.px,
                ^.height := 40.px,
                ^.lineHeight := 40.px,
                ^.borderRadius := 6.px,
                ^.border := border,
                ^.background := "#B0C4DE",
                <.div(
                    ^.position.absolute,
                    ^.width := 10.px,
                    ^.height := 2.px,
                    ^.backgroundColor := "black",
                    ^.marginLeft := "83%",
                    ^.marginTop := (-34).px,
                    ^.transform := "rotate(45deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.width := 10.px,
                    ^.height := 2.px,
                    ^.backgroundColor := "black",
                    ^.marginLeft := "83%",
                    ^.marginTop := (-34).px,
                    ^.transform := "rotate(-45deg)"
                ),
                ^.draggable := true,
                startSymbol(data.isStartState),
                multiTransition(this, processid, subjectid),
                eventMap(onClickKey),
                eventMap(onDragStartKey),
                eventMap(onDragKey)
            )
        }

    }

    class DummyNode(processid: String, subjectid: String, stateData: StateData) extends StateGraph(processid: String, subjectid: String, stateData: StateData) {
        data.description = "It's just a dummy node!"
        override var graphWidth: Int = 80

        def content(eventMap: Map[String, TagMod]) = {
            <.div(
                ^.left := sx.px,
                ^.top := sy.px,
                ^.position.absolute,
                ^.width := 40.px,
                ^.height := 40.px,
                ^.borderRadius := 40.px,
                ^.border := "dashed 2px red",
                ^.backgroundColor := "yellow"
            )
        }
    }

    class Arrow(processid: String, subjectid: String, tr: TransitionData, _x1: Int, _y1: Int, _x2: Int, _y2: Int, arrowType: String) extends Graph {
        var x1 = _x1
        var y1 = _y1
        var x2 = _x2
        var y2 = _y2
        var data = tr
        var processID = processid
        var subjectID = subjectid
        var backgroundColor = "#000"
        var borderStyle = transitionStyle(tr)
        var spacex, spacey = 0
        var aType = arrowType

        def transitionStyle(data: TransitionData): String = {
            var currentType = data.transitionType
            currentType match {
                case "" => {
                    currentType = "solid 2px #6A00FF"
                    currentType
                }

                case "implicit" => {
                    currentType = "dashed 2px black"
                    currentType
                }
                case _ => {
                    currentType = "solid 2px black"
                    currentType
                }
            }
        }

        def changeColor = {
            borderStyle = "solid 2px red"
        }

        def resetColor = {
            borderStyle = transitionStyle(tr)
        }

        def setSpaceX(x: Int): Unit = {
            spacex = x
        }

        def setSpaceY(y: Int): Unit = {
            spacey = y
        }

        def changeSourceCoordinate(x: Int, y: Int): Unit = {
            x1 = x
            y1 = y
        }

        def changeTargetCoordinate(x: Int, y: Int): Unit = {
            x2 = x
            y2 = y
        }


        def content(eventMap: Map[String, TagMod]) = {

            if(rpst){
                drawArrow(x1, y1, x2, y2, data, borderStyle, eventMap)
            }else{
                arrowType match {
                    case "1" => drawArrow(x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "2" => drawLeftDownArrowPolyline(x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "3" => drawRightTopArrowPolyline(x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "4" => drawRightDownArrowPolyline(x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "5" => drawLeftTopArrowPolyline(x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "6" => drawCircle(processID, subjectID, x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "7" => drawRightAngleRightDown(processID, subjectID, x1, y1, x2, y2, data, borderStyle, eventMap)
                    case "8" => drawRightAngleRightUp(processID, subjectID, x1, y1, x2, y2, data, borderStyle, eventMap)
                }
            }
        }

    }


    def multiTransition(currentData: StateGraph, processID: String, subjectID: String) = {
        if (!currentData.dragging && !currentData.rpst) {
            if (currentData.childrenList.size > 1) {
                val firstChildID = currentData.childrenList.head
                val lastChildID = currentData.childrenList.last
                val firstChild = ProcessManager.processMap(processID).subjectMap(subjectID).stateMap(firstChildID)
                val lastChild = ProcessManager.processMap(processID).subjectMap(subjectID).stateMap(lastChildID)
                val firstChildCenter = symbolCenter(firstChild.data.stateType)
                val lastChildCenter = symbolCenter(lastChild.data.stateType)
                val firstChildX = firstChild.sx + firstChildCenter._1
                val lastChildX = lastChild.sx + lastChildCenter._1
                val currentStateCenter = symbolCenter(currentData.data.stateType)
                multiArrow(firstChildX, lastChildX, currentStateCenter, currentData.data.stateType, "1", currentData.sx)
            } else {
                if (currentData.childrenList.size == 1) {
                    val uniqueChild = currentData.childrenList.head
                    val child = ProcessManager.processMap(processID).subjectMap(subjectID).stateMap(uniqueChild)
                    val childCenter = symbolCenter(child.data.stateType)
                    val currentStateCenter = symbolCenter(currentData.data.stateType)
                    if (math.abs((child.sx + childCenter._1) - (currentData.sx + currentStateCenter._1)) <= 30) {
                        <.div()
                    } else {
                        multiArrow((currentData.sx + currentStateCenter._1), (child.sx + childCenter._1), currentStateCenter, currentData.data.stateType, "2", currentData.sx)
                    }
                } else {
                    <.div()
                }
            }
        } else {
            <.div()
        }
    }

    def descriptionData(data: TransitionData): (Int, String) = {
        var length = 0
        var showInformation = ""
        val currentTransitionType = data.transitionType
        currentTransitionType match {
            case "normal" => {
                if (data.information.action == "Send") {
                    if ((data.information.relatedSubjectName != "" || data.information.relatedMessageType != "")) {
                        showInformation = "Msg: (" + data.information.relatedMessageType + ") To: (" + data.information.relatedSubjectName + ")"
                    } else {
                        showInformation = ""

                    }
                }
                if (data.information.action == "Receive") {
                    if ((data.information.relatedSubjectName != "" || data.information.relatedMessageType != "")) {
                        showInformation = "Msg: (" + data.information.relatedMessageType + ")  From: (" + data.information.relatedSubjectName + ")"

                    } else {
                        showInformation = ""
                    }
                }
                if (data.information.action != "Send" && data.information.action != "Receive") {
                    showInformation = data.description
                }
                length = showInformation.size
                (length, showInformation)
            }
            case "timeout" => {
                showInformation = data.timeout.toString + " s"
                length = showInformation.size
                (length, showInformation)
            }
            case "cancel" => {
                showInformation = ""
                (length, showInformation)
            }
            case "auto" => {
                showInformation = ""
                (length, showInformation)
            }
            case "implicit" => {
                showInformation = ""
                (length, showInformation)
            }
            case "" => {
                showInformation = ""
                (length, showInformation)
            }
        }
    }

    def transitionLabel(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, angle: Double) = {
        val x1 = _x1
        val x2 = _x2
        val y1 = _y1
        val y2 = _y2
        val currentAngle = angle
        val length = math.abs(y1 - y2)
        var showDescription = descriptionData(tr)
        var labelLength: Int = showDescription._1
        var actualHeight = false

        var offsetLeft = (labelLength * 8) / 2
        <.label(
            showDescription._2,
            ^.fontSize := 12.px,
            ^.position.absolute,
            ^.top := (length / 2 - 15).px,
            ^.left := (-offsetLeft).px,
            (^.left := (-120).px).when(labelLength > 30),
            ^.width := (labelLength * 8).px,
            ^.maxWidth := 240.px,
            ^.height := 30.px,
            ^.borderRadius := 6.px,
            (^.backgroundColor := "#D0D3D4").when(labelLength != 0),
            (^.border := " dashed 1px black").when(labelLength != 0),
            ^.transform := s"rotate(${-currentAngle}deg)",
            Style.textStyle,
            ^.textAlign.center,
            ^.display.`inline-block`,
            ^.whiteSpace.nowrap,
            ^.textOverflow.ellipsis,
            ^.lineHeight := 30.px,
            ^.overflow.hidden,
        )
    }

    def transitionLabelOfLeftDown(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData) = {
        val x1 = _x1
        val x2 = _x2
        val y1 = _y1
        val y2 = _y2
        val labelLength = tr.transitionType.length
        val offsetLeft = (labelLength * 8) / 2
        val times = tr.repeatTimes
        var actualHeigth = 0
        if (times <= 1) {
            actualHeigth = -15
        } else {
            actualHeigth = 15 * (times)
        }
        <.label(
            tr.transitionType,
            ^.fontSize := 12.px,
            ^.position.absolute,
            ^.top := actualHeigth.px,
            ^.left := (-offsetLeft).px,
            ^.width := (labelLength * 8).px,
            ^.height := 30.px,
            ^.borderRadius := 6.px,
            (^.backgroundColor := "#D0D3D4").when(labelLength != 0),
            (^.border := " dashed 1px black").when(labelLength != 0),
            Style.textStyle,
            ^.textAlign.center,
            ^.lineHeight := 30.px
        )
    }

    def transitionLabelOfRightDown(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, gap: Int) = {
        val x1 = _x1
        val x2 = _x2
        val y1 = _y1
        val y2 = _y2
        val w = math.abs(x1 - x2) + gap
        val h = math.abs(y1 - y2)

        val labelLength = tr.transitionType.length
        val offsetLeft = (labelLength * 8) / 2
        val times = tr.repeatTimes
        var actualHeigth = 0
        if (times <= 1) {
            actualHeigth = -15
        } else {
            actualHeigth = 15 * (times)
        }
        if (actualHeigth >= h) {
            actualHeigth = h
        }


        <.label(
            tr.transitionType,
            ^.fontSize := 12.px,
            ^.position.absolute,
            ^.top := (actualHeigth).px,
            ^.left := (w - offsetLeft).px,
            ^.width := (labelLength * 8).px,
            ^.height := 30.px,
            ^.borderRadius := 6.px,
            (^.backgroundColor := "#D0D3D4").when(labelLength != 0),
            (^.border := " dashed 1px black").when(labelLength != 0),
            Style.textStyle,
            ^.textAlign.center,
            ^.lineHeight := 30.px
        )
    }

    def transitionLabelOfLeftTop(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData) = {
        val x1 = _x1
        val x2 = _x2
        val y1 = _y1
        val y2 = _y2
        var labelLength = tr.transitionType.length
        var offsetLeft = (labelLength * 8) / 2

        val times = tr.repeatTimes
        var actualHeigth = 0
        if (times <= 1) {
            actualHeigth = 15
        } else {
            actualHeigth = 15 * (times)
        }


        <.label(
            tr.transitionType,
            ^.fontSize := 12.px,
            ^.position.absolute,
            ^.top := (-actualHeigth).px,
            ^.left := (-offsetLeft).px,
            ^.width := (labelLength * 8).px,
            ^.height := 30.px,
            ^.borderRadius := 6.px,
            (^.backgroundColor := "#D0D3D4").when(labelLength != 0),
            (^.border := " dashed 1px black").when(labelLength != 0),
            Style.textStyle,
            ^.textAlign.center,
            ^.lineHeight := 30.px
        )
    }

    def transitionLabelOfRightTop(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, gap: Int) = {
        val x1 = _x1
        val x2 = _x2
        val y1 = _y1
        val y2 = _y2

        val w = math.abs(x1 - x2) + gap
        val h = math.abs(y1 - y2)
        val labelLength = tr.transitionType.length
        val offsetLeft = (labelLength * 8) / 2
        val times = tr.repeatTimes
        var actualHeigth = 0
        if (times <= 1) {
            actualHeigth = 15
        } else {
            actualHeigth = 15 * (times)
        }
        if (actualHeigth >= h) {
            actualHeigth = h
        }

        <.label(
            tr.transitionType,
            ^.fontSize := 12.px,
            ^.position.absolute,
            ^.top := (-actualHeigth).px,
            ^.left := (gap - offsetLeft).px,
            ^.width := (labelLength * 8).px,
            ^.height := 30.px,
            ^.borderRadius := 6.px,
            (^.backgroundColor := "#D0D3D4").when(labelLength != 0),
            (^.border := " dashed 1px black").when(labelLength != 0),
            Style.textStyle,
            ^.textAlign.center,
            ^.lineHeight := 30.px
        )
    }

    def drawArrow(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        var x1 = _x1
        var y1 = _y1
        var x2 = _x2
        var y2 = _y2

        def length: Double = Math.sqrt(Math.pow(Math.abs(x1 - x2), 2) + Math.pow(Math.abs(y1 - y2), 2)).formatted("%.2f").toDouble

        def arc: Double = Math.asin((x1 - x2) / length).formatted("%.2f").toDouble

        def angle: Double = {
            if (y2 < y1) {
                -180 - (arc * (180.0 / Math.PI)).formatted("%.2f").toDouble
            } else {
                (arc * (180.0 / Math.PI)).formatted("%.2f").toDouble
            }
        }

        <.div(
            ^.position.absolute,
            ^.width := 0.px,
            ^.height := length.px,
            ^.left := x1.px,
            ^.top := (y1 + 2).px,
            ^.borderLeft := backgroundColor,
            ^.transformOrigin := "top left",
            ^.transform := s"rotate(${angle}deg)",
            transitionLabel(x1, y1, x2, y2, tr, angle),
            timeoutSymbol(x1, y1, x2, y2, tr),
            cancelSymbol(x1, y1, x2, y2, tr),
            autoSymbol(x1, y1, x2, y2, tr),

            <.div(
                ^.position.absolute,
                ^.top := (length - 10).px,
                ^.left := (-6).px,
                ^.width := 0.px,
                ^.height := 0.px,
                ^.borderLeft := "solid 5px transparent",
                ^.borderTop := "solid 10px black",
                ^.borderRight := "solid 5px transparent"
            ),
            eventMap(onClickKey)
        )
    }

    def drawLeftDownArrowPolyline(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2
        val times = tr.repeatTimes
        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)
        var gap = 0
        var smallGap = 0

        if (times == 1) {
            gap = 80
            smallGap = 0
        } else if (times > 1) {
            gap = 60 + times * 40
            smallGap = times * 4
        } else {
            gap = 80
            smallGap = 0
        }

        <.div(
            ^.position.absolute,
            ^.width := (widthP + gap).px,
            ^.height := 0.px,
            ^.top := (y1 + smallGap).px,
            ^.left := (x1 - (widthP + gap)).px,
            ^.borderBottom := backgroundColor,
            <.div(
                ^.position.absolute,
                ^.width := 0.px,
                ^.height := (heightP + 2).px, // 折线长
                ^.marginLeft := 0.px,
                ^.marginTop := 0.px,
                ^.borderLeft := backgroundColor,
                <.div(
                    ^.position.absolute,
                    ^.width := gap.px,
                    ^.height := 0.px,
                    ^.marginTop := heightP.px,
                    ^.marginLeft := 0.px,
                    ^.borderBottom := backgroundColor,
                    <.div(
                        ^.position.absolute,
                        ^.top := (-4.5).px,
                        ^.left := (gap - 10).px,
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.borderLeft := "solid 10px black",
                        ^.borderTop := "solid 5px transparent",
                        ^.borderBottom := "solid 5px transparent"
                    ),
                )
            ),
            transitionLabelOfLeftDown(x1, y1, x2, y2, tr),
            eventMap(onClickKey)
        )
    }

    def drawRightDownArrowPolyline(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2
        val times = tr.repeatTimes
        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)
        var gap = 0
        var smallGap = 0

        if (times == 1) {
            gap = 80
            smallGap = 0
        } else if (times > 1) {
            gap = 60 + times * 40
            smallGap = times * 4
        } else {
            gap = 80
            smallGap = 0
        }

        <.div(
            ^.position.absolute,
            ^.width := (widthP + gap).px,
            ^.height := 0.px,
            ^.top := (y1 + smallGap).px,
            ^.left := (x1 + 2).px,
            ^.borderBottom := backgroundColor,
            <.div(
                ^.position.absolute,
                ^.width := 0.px,
                ^.height := heightP.px,
                ^.marginLeft := (widthP + gap).px,
                ^.marginTop := 0.px,
                ^.borderRight := backgroundColor,
                <.div(
                    ^.position.absolute,
                    ^.width := gap.px,
                    ^.height := 0.px,
                    ^.marginTop := heightP.px,
                    ^.marginLeft := (-gap + 2).px,
                    ^.borderBottom := backgroundColor,
                    <.div(
                        ^.position.absolute,
                        ^.top := (-4.5).px,
                        ^.left := (-0.5).px,
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.borderBottom := "solid 5px transparent",
                        ^.borderTop := "solid 5px transparent",
                        ^.borderRight := "solid 10px black"
                    )
                )
            ),
            transitionLabelOfRightDown(x1, y1, x2, y2, tr, gap),
            eventMap(onClickKey)
        )


    }

    def drawLeftTopArrowPolyline(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2

        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)

        val times = tr.repeatTimes
        var gap = 0
        var smallGap = 0

        if (times == 1) {
            gap = 60
            smallGap = 0
        } else if (times > 1) {
            gap = 60 + times * 40
            smallGap = times * 6
        } else {
            gap = 60
            smallGap = 0
        }

        <.div(
            ^.position.absolute,
            ^.borderBottom := backgroundColor,
            ^.width := (gap).px,
            ^.height := 0.px,
            ^.top := (y1 + smallGap).px,
            ^.left := (x1 - gap).px,
            <.div(
                ^.position.absolute,
                ^.width := 0.px,
                ^.height := heightP.px,
                ^.borderLeft := backgroundColor,
                ^.marginLeft := 0.px,
                ^.marginTop := (-heightP + 2).px,
                <.div(
                    ^.position.absolute,
                    ^.borderBottom := backgroundColor,
                    ^.width := (widthP + gap).px,
                    ^.height := 0.px,
                    ^.marginTop := 0.px,
                    ^.marginLeft := (0).px,
                    <.div(
                        ^.position.absolute,
                        ^.top := (-4.5).px,
                        ^.left := (widthP + gap - 10).px,
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.borderLeft := "solid 10px black",
                        ^.borderTop := "solid 5px transparent",
                        ^.borderBottom := "solid 5px transparent"
                    )
                )
            ),
            transitionLabelOfLeftTop(x1, y1, x2, y2, tr),
            eventMap(onClickKey)
        )
    }

    def drawRightTopArrowPolyline(_x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2
        val times = tr.repeatTimes
        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)
        var gap = 0
        var smallGap = 0

        if (times == 1) {
            gap = 60
            smallGap = 0
        } else if (times > 1) {
            gap = 60 + times * 40
            smallGap = times * 4
        } else {
            gap = 60
            smallGap = 0
        }

        <.div(
            ^.position.absolute,
            ^.borderBottom := backgroundColor,
            ^.width := (gap).px,
            ^.height := 0.px,
            ^.top := (y1 + smallGap).px,
            ^.left := (x1 + 2).px,
            <.div(
                ^.position.absolute,
                ^.width := 0.px,
                ^.height := heightP.px,
                ^.borderRight := backgroundColor,
                ^.marginLeft := (gap - 2).px,
                ^.marginTop := (-heightP + 2).px,
                <.div(
                    ^.position.absolute,
                    ^.borderBottom := backgroundColor,
                    ^.width := (widthP + gap).px,
                    ^.height := 0.px,
                    ^.marginTop := 0.px,
                    ^.marginLeft := (-(widthP + gap) + 2).px,
                    <.div(
                        ^.position.absolute,
                        ^.top := (-4.5).px,
                        ^.left := (0).px,
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.borderBottom := "solid 5px transparent",
                        ^.borderTop := "solid 5px transparent",
                        ^.borderRight := "solid 10px black"
                    )
                )
            ),
            transitionLabelOfRightTop(x1, y1, x2, y2, tr, gap),
            eventMap(onClickKey)
        )
    }

    def drawCircle(processid: String, subjectid: String, _x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val s = tr.source
        val a = ProcessManager.processMap(processid).subjectMap(subjectid).stateMap(s)
        val width = a.graphWidth
        val x = _x1 + width / 2 + 3
        val y = _y1 + 10


        <.div(
            ^.position.absolute,
            ^.top := y.px,
            ^.left := x.px,
            ^.width := 20.px,
            ^.height := 2.px,
            ^.backgroundColor := backgroundColor,
            <.div(
                ^.position.absolute,
                ^.width := 2.px,
                ^.height := 20.px,
                ^.backgroundColor := backgroundColor,
                ^.marginTop := (-20).px,
                ^.marginLeft := 18.px,
                <.div(
                    ^.position.absolute,
                    ^.width := 20.px,
                    ^.height := 2.px,
                    ^.backgroundColor := backgroundColor,
                    ^.marginTop := (0).px,
                    ^.marginLeft := (-18).px,
                    <.div(
                        ^.position.absolute,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.marginLeft := 0.px,
                        ^.marginTop := 1.px,
                        ^.backgroundColor := backgroundColor,
                        ^.transform := "rotate(30deg)"
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.marginLeft := (0).px,
                        ^.marginTop := (-1).px,
                        ^.backgroundColor := backgroundColor,
                        ^.transform := "rotate(-30deg)"
                    )
                )
            ),
            eventMap(onClickKey)

        )
    }

    def drawRightAngleRightDown(processid: String, subjectid: String, _x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2

        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)

        <.div(
            ^.position.absolute,
            ^.backgroundColor := backgroundColor,
            ^.width := 2.px,
            ^.height := heightP.px,
            ^.top := (y1).px,
            ^.left := (x1).px,
            <.div(
                ^.position.absolute,
                ^.top := heightP.px,
                ^.left := (-widthP).px,
                ^.width := widthP.px,
                ^.height := 2.px,
                ^.backgroundColor := backgroundColor,
                <.div(
                    ^.position.absolute,
                    ^.backgroundColor := backgroundColor,
                    ^.top := 2.px,
                    ^.left := 0.px,
                    ^.width := 10.px,
                    ^.height := 2.px,
                    ^.transform := "rotate(30deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.backgroundColor := backgroundColor,
                    ^.top := (-2).px,
                    ^.left := 0.px,
                    ^.width := 10.px,
                    ^.height := 2.px,
                    ^.transform := "rotate(-30deg)"
                )
            )
        )
    }

    def drawRightAngleRightUp(processid: String, subjectid: String, _x1: Int, _y1: Int, _x2: Int, _y2: Int, tr: TransitionData, backgroundColor: String, eventMap: Map[String, TagMod]): TagMod = {
        val x1 = _x1
        val y1 = _y1
        val x2 = _x2
        val y2 = _y2

        val widthP = Math.abs(x2 - x1)
        val heightP = Math.abs(y2 - y1)

        <.div(
            ^.position.absolute,
            ^.backgroundColor := backgroundColor,
            ^.width := widthP.px,
            ^.height := 2.px,
            ^.top := (y1).px,
            ^.left := (x1 + 2).px,
            <.div(
                ^.position.absolute,
                ^.top := (-heightP).px,
                ^.left := (widthP - 2).px,
                ^.width := 2.px,
                ^.height := (heightP - 2).px,
                ^.backgroundColor := backgroundColor,
                <.div(
                    ^.position.absolute,
                    ^.backgroundColor := backgroundColor,
                    ^.top := 0.px,
                    ^.left := 2.px,
                    ^.width := 2.px,
                    ^.height := 10.px,
                    ^.transform := "rotate(-30deg)"
                ),
                <.div(
                    ^.position.absolute,
                    ^.backgroundColor := backgroundColor,
                    ^.top := (0).px,
                    ^.left := (-2).px,
                    ^.width := 2.px,
                    ^.height := 10.px,
                    ^.transform := "rotate(30deg)"
                )
            )
        )
    }

    def multiArrow(start: Int, end: Int, center: (Int, Int), stateType: String, auxiliaryLineType: String, sourceX: Int) = {
        var auxiliaryLineTop = 0
        if (circle.contains(stateType)) {
            auxiliaryLineTop = 2 * center._2
        }

        if (stateType == "IsIPEmpty") {
            auxiliaryLineTop = 2 * center._2
        }
        auxiliaryLineType match {
            case "1" => {
                val currentWidth = math.abs(start - end)
                val offsetX = sourceX + center._1 - start
                <.div(
                    ^.position.absolute,
                    ^.width := 2.px,
                    ^.height := 18.px, // 固定长度, border也有厚度
                    (^.marginLeft := (center._1 - 2).px).when(stateType != "IsIPEmpty"),
                    (^.marginLeft := 66.px).when(stateType == "IsIPEmpty"),
                    (^.marginTop := (auxiliaryLineTop).px).when(stateType != "IsIPEmpty"),
                    (^.marginTop := 58.px).when(stateType == "IsIPEmpty"),
                    ^.backgroundColor := "black",
                    (^.transform := "rotate(-45deg)").when(stateType == "IsIPEmpty"),
                    <.div(
                        ^.position.absolute,
                        ^.width := (currentWidth + 2).px,
                        ^.height := 2.px,
                        ^.marginTop := 18.px,
                        ^.marginLeft := (-offsetX).px,
                        ^.backgroundColor := "black"
                    )
                )
            }

            case "2" => {
                val currentWidth = (start - end)
                val lineLength = math.abs(currentWidth)
                <.div(
                    ^.position.absolute,
                    ^.width := 2.px,
                    ^.height := 18.px, // 固定长度, border也有厚度
                    (^.marginLeft := (center._1 - 2).px).when(stateType != "IsIPEmpty"),
                    (^.marginTop := (2 * center._2).px).when(stateType != "IsIPEmpty"),
                    ^.backgroundColor := "black",
                    (^.marginLeft := 66.px).when(stateType == "IsIPEmpty"),
                    (^.marginTop := 58.px).when(stateType == "IsIPEmpty"),
                    (^.transform := "rotate(-45deg)").when(stateType == "IsIPEmpty"),
                    <.div(
                        ^.position.absolute,
                        ^.width := lineLength.px,
                        ^.height := 2.px,
                        ^.marginTop := 18.px,
                        (^.marginLeft := 0.px).when(currentWidth < 0),
                        (^.marginLeft := (-currentWidth + 2).px).when(currentWidth > 0),
                        ^.backgroundColor := "black"
                    )
                )
            }
        }

    }

    def startSymbol(isStart: Boolean): TagMod = {
        if (isStart) {
            <.div(
                ^.position.absolute,
                ^.top := (0).px,
                ^.left := (-12).px,
                ^.width := 0.px,
                ^.height := 0.px,
                ^.borderBottom := "solid 10px #008A00",
                ^.borderLeft := "solid 5px transparent",
                ^.borderRight := "solid 5px transparent",
                <.div(
                    ^.position.absolute,
                    ^.width := 0.px,
                    ^.height := 0.px,
                    ^.borderTop := "solid 10px #008A00",
                    ^.borderLeft := "solid 5px transparent",
                    ^.borderRight := "solid 5px transparent",
                    ^.top := 3.px,
                    ^.left := (-5).px
                )
            )
        } else {
            <.div()
        }

    }

    def timeoutSymbol(x1: Int, y1: Int, x2: Int, y2: Int, tr: TransitionData): TagMod = {
        val h = math.abs(y1 - y2)
        if (tr.transitionType == "timeout") {
            <.div(
                ^.position.absolute,
                ^.top := (h / 6).px,
                ^.left := (-10).px,
                ^.width := 16.px,
                ^.height := 16.px,
                ^.borderRadius := "100%",
                ^.backgroundColor := "#F5F5F5",
                ^.border := "solid 2px black",
                <.div(
                    ^.position.absolute,
                    ^.top := 2.px,
                    ^.left := 6.px,
                    ^.backgroundColor := "black",
                    ^.width := 2.px,
                    ^.height := 6.px,
                    <.div(
                        ^.position.absolute,
                        ^.top := 6.px,
                        ^.left := 0.px,
                        ^.width := 6.px,
                        ^.height := 2.px,
                        ^.backgroundColor := "black"
                    )
                )
            )
        } else {
            <.div()
        }
    }

    def cancelSymbol(x1: Int, y1: Int, x2: Int, y2: Int, tr: TransitionData): TagMod = {
        val h = math.abs(y1 - y2)
        if (tr.transitionType == "cancel") {
            <.div(
                ^.position.absolute,
                ^.top := (h / 6).px,
                ^.left := (-2).px, // 直箭头的设定
                ^.width := 2.px,
                ^.height := 10.px,
                ^.backgroundColor := "red",
                ^.transform := "rotate(45deg)",
                <.div(
                    ^.position.absolute,
                    ^.top := 0.px,
                    ^.left := 0.px,
                    ^.width := 2.px,
                    ^.height := 10.px,
                    ^.backgroundColor := "red",
                    ^.transform := "rotate(90deg)",
                )
            )
        } else {
            <.div()
        }
    }

    def autoSymbol(x1: Int, y1: Int, x2: Int, y2: Int, tr: TransitionData): TagMod = {
        val h = math.abs(y1 - y2)
        if (tr.transitionType == "auto") {
            <.div(
                ^.position.absolute,
                ^.top := (h / 6).px,
                ^.left := (-10).px,
                ^.width := 16.px,
                ^.height := 16.px,
                ^.borderRadius := "100%",
                ^.border := "solid 2px green",
                ^.backgroundColor := "#F5F5F5",
                <.div(
                    ^.position.absolute,
                    ^.width := 14.px,
                    ^.height := 14.px,
                    ^.margin.auto,
                    ^.top := 0.px,
                    ^.left := 1.px,
                    "A",
                    ^.textAlign.center,
                    ^.fontWeight.bold,
                    ^.color := "green"
                )
            )
        } else {
            <.div()
        }
    }

    def createGraph(sType: String, stateData: StateData, processID: String, subjectID: String): StateGraph ={
        var graph: StateGraph = null
        println(s"state type: $sType")
        sType match{
            case "Action" => {
                graph = new Action(processID, subjectID, stateData)
            }
            case "Send" => {
                graph = new Send(processID, subjectID, stateData)
            }
            case "Receive" => {
                graph = new Receive(processID, subjectID, stateData)
            }
            case "Modal Join" => {
                graph = new ModalJoin(processID, subjectID, stateData)
            }
            case "Modal Split" => {
                graph = new ModalSplit(processID, subjectID, stateData)
            }
            case "Tau" => {
                graph = new Tau(processID, subjectID, stateData)
            }
            case "CloseIP" => {
                graph = new CloseIP(processID, subjectID, stateData)
            }
            case "OpenIP" => {
                graph = new OpenIP(processID, subjectID, stateData)
            }
            case "IsIPEmpty" => {
                graph = new IsIPEmpty(processID, subjectID, stateData)
            }
            case "CloseAllIPs" => {
                graph = new CloseAllIPs(processID, subjectID, stateData)
            }
            case "OpenAllIPs" => {
                graph = new OpenAllIPs(processID, subjectID, stateData)
            }
            case "SelectAgents" => {
                graph = new SelectAgents(processID, subjectID, stateData)
            }
            case "CallMacro" => {
                graph = new CallMacro(processID, subjectID, stateData)
            }
            case "VarMan" => {
                graph = new VarMan(processID, subjectID, stateData)
            }
            case "Cancel" => {
                graph = new Cancel(processID, subjectID, stateData)
            }
            case "End" => {
                graph = new End(processID, subjectID, stateData)
            }
        }
        graph
    }

}