package de.tkip.sbpm.frontend

//import chandu0101.scalajs.react.components.reactselect.{Select, ValueOption}
import japgolly.scalajs.react._
import vdom.html_<^._
import de.tkip.sbpm.frontend.AppRouter.SubjectPages
import japgolly.scalajs.react.extra.router.RouterConfigDsl
//import japgolly.scalajs.react.raw.ReactNode
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLOptionElement

import scala.scalajs.js
import scala.collection.mutable.{ListBuffer, Map, Set, Stack}

/**
  * Created by Wang on 2017/5/2.
  */
case class StateAttributes(st: String) {
    var stateType: String = st

    def changeStateType(nst: String): Unit = {
        stateType = nst
    }
}

case class TransitionAttributes(tt: String) {
    var testType: String = tt

    def changeStateType(ntt: String): Unit = {
        testType = ntt
    }
}

case class SubToSubMessage(subId: Int, stateId: Int, message: String = "") {
    val subid = subId
    val stateid = stateId
    var msg = message
}

class Transition(id: Int, tid: Int, sid: Int, subToSubMessage: SubToSubMessage, sx: Int, sy: Int, tx: Int, ty: Int) {

    val transitionid = id
    val targetid = tid
    var source = ""
    val stosMsg = subToSubMessage
    var sxc = sx
    var syc = sy
    var txc = tx
    var tyc = ty
    val tip = 20
    var colour = "#800"
    val colour2 = "#00bff3"

    //  def arrowView(): VdomElement
}


class State(id: Int, sx: Int, sy: Int, sType: String) {
    val radius = 50 //the size of the state
    val arrowLength = 100 //the length of the arrow
    val shortLine = 40 //the length of the polyline arrow
    val tip = 20 //the length of the arrow tip
    var selected = false
    var x: Int = sx
    var y: Int = sy
    val stateid: Int = id
    var stateType: String = sType
    var isStartState = false
    val outTransitions: ListBuffer[Transition] = new ListBuffer[Transition]()
    val inTransitions: ListBuffer[Transition] = new ListBuffer[Transition]()
    var anchorLeft = (x, y + radius / 2)
    var anchorRight = (x + radius, y + radius / 2)
    var anchorTop = (x + radius / 2, y)
    var anchorBottom = (x + radius / 2, y + radius)
    var colour = "#800"
    val colour2 = "#00bff3"
}


object SubjectViewPage {


    var subjectID = -1
    val OuterX = 1280
    val OuterY = 1800
    var stateID: Int = 3
    var newState: Int = 0
    var stateAttrMap: Map[Int, StateAttributes] = Map()
    var tranAttrMap: Map[Int, TransitionAttributes] = Map()
    var hasFocus: Boolean = true // test
    val currentValue: Option[String] = Some("test textarea")


    def onTextChange(e: ReactEventFromInput): Option[Callback] = {
        currentValue.map(before => {
            val after = e.target.value
            Callback.alert(s"Value changed from [$before] to [$after]")
        })
    }


    val OuterDiv =
        <.div(
            ^.position.relative,
            ^.backgroundColor := "#FFE4B5",
            ^.width := "100%",
            ^.height := 800.px,
            ^.border := "solid 1px #333"
        )

    val component = ScalaComponent.builder[SubjectPages]("StatePage").render { p =>
        subjectID = p.props.id
        Main()
    }.build


    //  val component = ScalaComponent.builder[SubjectPages]("StatePage").render
    //  { p =>
    //    states.foreach(f =>
    //      <.div(
    //       <.h1("test test")
    //      )
    //    )
    //  }


    class Backend($: BackendScope[Unit, Int]) {

        val options = js.Array[VdomElement]()
        options.push(<.option("111222333"))
        options.push(<.option("333"))
        options.push(<.option("222"))
        options.push(<.option("111"))

        def body =
            <.div( // body
                ^.float := "left",
                ^.width := "70%",
                ^.height := 600.px,
                ^.border := "solid 1px #333",
                ^.backgroundColor := "#528B8B",
                ^.marginLeft := "15%",
                ^.border := "solid 1px #333",
                ^.overflow.scroll
            )

        def footer =
            <.div( // footer
                ^.position.fixed,
                ^.bottom := 0.px,
                ^.left := 0.px,
                ^.height := 70.px,
                ^.width := "100%",
                (^.backgroundColor := "#8B8B00").when(hasFocus)
            )

        def leftSide =
            <.div(
                ^.position.absolute, // left side
                ^.backgroundColor := "#ddd",
                ^.border := "solid 1px #191970",
                ^.left := 0.px,
                ^.top := 0.px,
                ^.width := "15%",
                ^.height := 600.px,

                <.div(
                    ^.position.relative,
                    ^.width := "100%",
                    ^.height := "40%",
                    ^.backgroundColor := "#F5DEB3",
                    <.br,
                    <.br,
                    <.a(
                        "Dashboard",
                        ^.href := "#",
                        ^.fontSize := "120%",
                        ^.marginLeft := "10%"
                    ),
                    <.br,
                    <.br,
                    <.a(
                        "Process",
                        ^.href := "#",
                        ^.fontSize := "120%",
                        ^.marginLeft := "10%"
                    ),
                    <.br,
                    <.br,
                    <.a(
                        "Create new process",
                        ^.href := "#",
                        ^.fontSize := "120%",
                        ^.marginLeft := "10%"
                    )
                )
            )

        def rightSide =
            <.div( // right side
                ^.position.absolute,
                //     ^.tabIndex := 0,
                ^.right := 0.px,
                ^.width := "15%",
                ^.height := 600.px,
                ^.border := "solid 1px #333",
                ^.background := "#FFB6C1",

                <.div(
                    ^.position.relative,
                    ^.marginTop := 0.px,
                    ^.width := "100%",
                    ^.height := "30%",
                    ^.backgroundColor := "#EE7621",
                    ^.overflow.scroll,

                    <.label( // Layout Setting
                        ^.position.relative,
                        ^.width := "100%",
                        ^.height := "15%",
                        ^.borderBottom := "dashed 1px",
                        <.p(
                            ^.textAlign := "center",
                            ^.letterSpacing := "1px",
                            ^.fontSize := "120%",
                            "Layout Setting "
                        )
                    ),
                    <.br,
                    <.select(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        options.toTagMod
                    ),
                    <.br,
                    <.label( // Behavior Settings
                        ^.position.relative,
                        ^.width := "100%",
                        ^.height := "15%",
                        ^.marginTop := "5%",
                        ^.borderBottom := "dashed 1px",
                        <.p(
                            ^.textAlign := "center",
                            ^.letterSpacing := "1px",
                            ^.fontSize := "120%",
                            "Behavior Settings "
                        )
                    ),
                    <.br,
                    <.button(
                        "Add new state",
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.width := "80%",
                        ^.height := 20.px,
                        ^.fontSize := "90%",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.button(
                        "Reset manual position",
                        ^.position.relative,
                        ^.marginTop := 5.px,
                        ^.marginLeft := "10%",
                        ^.width := "80%",
                        ^.height := 20.px,
                        ^.fontSize := "85%",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.button(
                        "Clear all behaviors",
                        ^.position.relative,
                        ^.marginTop := 5.px,
                        ^.marginLeft := "10%",
                        ^.width := "80%",
                        ^.height := 20.px,
                        ^.fontSize := "90%",
                        ^.borderRadius := 6.px
                    )
                ),

                <.div( // State Setting
                    ^.position.relative,
                    ^.width := "100%",
                    ^.height := "70%",
                    ^.backgroundColor := "#9933FF",
                    ^.overflow.scroll,

                    <.label(
                        ^.position.relative,
                        ^.width := "100%",
                        ^.height := "8%",
                        ^.marginTop := "5%",
                        ^.borderBottom := "dashed 1px",
                        <.p(
                            ^.textAlign := "center",
                            ^.fontSize := "120%",
                            "State Settings"
                        )
                    ),
                    <.br,
                    <.input.checkbox(
                        ^.position.relative,
                        ^.marginLeft := "10%"
                    ),
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := 10.px,
                        "Start state"
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        "Type : "
                    ),
                    <.br,
                    <.select(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.onChange ==> selectStateType,
                        <.option("Action"),
                        <.option("Send"),
                        <.option("Receive"),
                        <.option("ModalJoin"),
                        <.option("ModalSplit"),
                        <.option("End")
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginTop := 10.px,
                        ^.marginLeft := "10%",
                        "Text : "
                    ),
                    <.br,
                    <.textarea(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.minWidth := "80%",
                        ^.minHeight := "10%",
                        ^.maxHeight := "10%",
                        ^.maxWidth := "80%",
                        ^.borderStyle := "solid 1px #191970",
                        ^.borderRadius := 6.px,
                        ^.value := currentValue.getOrElse(""),
                        //            ^.value  := "test textarea " ,
                        ^.onChange ==>? onTextChange
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginTop := 10.px,
                        ^.marginLeft := "10%",
                        "Comment:  "
                    ),
                    <.br,
                    <.textarea(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.minWidth := "80%",
                        ^.minHeight := "15%",
                        ^.maxHeight := "15%",
                        ^.maxWidth := "80%",
                        ^.borderStyle := "solid 1px #191970",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.button(
                        ^.position.relative,
                        ^.marginTop := 20.px,
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.marginLeft := "10%",
                        ^.fontSize := "90%",
                        ^.borderRadius := 6.px,
                        ^.onClick ==> updateStateAttr,
                        "Update state"
                    ),
                    <.button(
                        ^.position.relative,
                        ^.marginTop := 10.px,
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.marginLeft := "10%",
                        ^.fontSize := "90%",
                        ^.borderRadius := 6.px,
                        "Connect state"
                    ),
                    <.button(
                        ^.position.relative,
                        ^.marginTop := 10.px,
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.marginLeft := "10%",
                        ^.fontSize := "90%",
                        ^.borderRadius := 6.px,
                        "Delete state"
                    )
                ),

                <.div(
                    ^.position.relative, // Transition Settings
                    ^.width := "100%",
                    ^.height := "70%",
                    ^.backgroundColor := "#B03060",
                    ^.overflow.scroll,
                    ^.display.none,

                    <.label(
                        ^.position.relative,
                        ^.width := "100%",
                        ^.height := "8%",
                        ^.marginTop := "5%",
                        ^.borderBottom := "dashed 1px",
                        <.p(
                            ^.textAlign := "center",
                            ^.fontSize := "120%",
                            "Transition Settings"
                        )
                    ),
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        "Message Type: "
                    ),
                    <.select(
                        ^.position.relative,
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.marginLeft := "10%",
                        <.option(
                            "create new message type",
                            ^.textOverflow.ellipsis
                        ),
                        <.option("select message type")
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        "Text : "
                    ),
                    <.br,
                    <.textarea(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.maxHeight := "8%",
                        ^.minHeight := "8%",
                        ^.maxWidth := "80%",
                        ^.minWidth := "80%",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        "Related Subject :"
                    ),
                    <.br,
                    <.select(
                        ^.position.relative,
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.marginLeft := "10%",
                        <.option("select related subject")
                    ),
                    <.br,
                    <.label(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        "Comment : "
                    ),
                    <.br,
                    <.textarea(
                        ^.position.relative,
                        ^.marginLeft := "10%",
                        ^.maxHeight := "15%",
                        ^.minHeight := "15%",
                        ^.maxWidth := "80%",
                        ^.minWidth := "80%",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.button(
                        "Update Transition",
                        ^.position.relative,
                        ^.marginTop := "10%",
                        ^.marginLeft := "10%",
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.borderRadius := 6.px
                    ),
                    <.br,
                    <.button(
                        "Remove Transition",
                        ^.position.relative,
                        ^.marginTop := "5%",
                        ^.marginLeft := "10%",
                        ^.width := "80%",
                        ^.height := "5%",
                        ^.borderRadius := 6.px
                    )
                )
            )

        //    case class Transition(idsuccessor: Int){
        //      var target = ""
        //      var source = ""
        //    }

        class TransitionGraph(id: Int, tid: Int, sid: Int, subToSubMessage: SubToSubMessage, sx: Int, sy: Int, tx: Int, ty: Int)
                extends Transition(id, tid, sid, subToSubMessage, sx, sy, tx, ty) {

            def arrowLength: Int = ty - sy

            def arrow(ax: Int, ay: Int, color: String): VdomElement =
                <.div(
                    ^.position.absolute,
                    <.div(
                        ^.position.absolute,
                        ^.transform := "rotate(90deg)",
                        ^.width := arrowLength.px,
                        ^.height := 5.px,
                        ^.left := ax.px,
                        ^.top := ay.px,
                        ^.background := color
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.transform := "rotate(120deg)",
                        ^.width := tip.px,
                        ^.height := 5.px,
                        ^.left := (ax + arrowLength / 2 + (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                        ^.top := (ay + arrowLength / 2 - (math.cos(math.toRadians(30)) * tip / 2)).px,
                        ^.background := color
                    ),
                    <.div(
                        ^.position.absolute,
                        ^.transform := "rotate(60deg)",
                        ^.width := tip.px,
                        ^.height := 5.px,
                        ^.left := (ax + arrowLength / 2 - (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                        ^.top := (ay + arrowLength / 2 - (math.cos(math.toRadians(30)) * tip / 2)).px,
                        ^.background := color
                    ),
                    polyLine(color),
                    ^.onClick ==> clickTransition
                )

            def polyLine(color: String): VdomElement = {
                if (sx > tx) {
                    <.div(
                        ^.position.absolute,
                        ^.width := (sx - tx).px,
                        ^.height := 5.px,
                        ^.left := tx.px,
                        ^.top := sy.px,
                        ^.background := color
                    )
                } else if (sx < tx) {
                    <.div(
                        ^.position.absolute,
                        ^.width := (tx - sx).px,
                        ^.height := 5.px,
                        ^.left := sx.px,
                        ^.top := sy.px,
                        ^.background := color
                    )
                } else {
                    <.div()
                }
            }

            def convertx(x: Int): Int = x - (arrowLength + 1) / 2

            def converty(y: Int): Int = y + (arrowLength + 1) / 2

            def arrowView(): VdomElement = {
                //        dom.console.info("test arrow: " + tx + "   " + ty + "   length: " + arrowLength)
                arrow(convertx(tx), converty(sy), colour)
            }

            def clickTransition(e: ReactEventFromInput) = {
                selectedTransition(targetid)
                dom.console.info("Clicked!")
                e.preventDefaultCB >>
                        $.modState(_ + 0)
            }
        }

        class StateGraph(id: Int, sx: Int, sy: Int, sType: String)
                extends State(id, sx, sy, sType) {

            def stateGraph(xx: Int, yy: Int, color: String): VdomElement =
                <.div(
                    ^.position.absolute,
                    ^.width := radius.px,
                    ^.height := radius.px,
                    ^.lineHeight := radius.px,
                    ^.left := xx.px,
                    ^.top := yy.px,
                    ^.borderRadius := (radius / 2).px,
                    ^.background := color,
                    ^.border := 2.px,
                    ^.fontSize := "120%",
                    ^.overflow := "hidden",
                    ^.textAlign := "center",
                    stateAttrMap.getOrElseUpdate(id, StateAttributes("Action")).stateType,
                    ^.onClick ==> select
                )

            def addOutTransition(tr: TransitionGraph): Unit = {
                outTransitions += tr
            }

            def addInTransition(tr: TransitionGraph): Unit = {
                inTransitions += tr
            }


            def stateView(): VdomElement = {
                //        dom.console.info(s"test state: id: $sID x,y: $sx, $sy   anchor L R T B: $anchorLeft, $anchorRight, $anchorTop, $anchorBottom" )
                stateGraph(x, y, colour)
            }

            def select(e: ReactEventFromInput) = {
                selectedState(stateid)

                e.preventDefaultCB >>
                        $.modState(_ + 0)
            }
        }


        //class backend
        var isConnected = false
        var connectState: Int = -1
        var selectedStateID: Int = -1
        var selectedTransitionID: Int = -1
        var predStateID: Int = -1
        var selectedStateType: String = "Action"
        val states = ListBuffer[StateGraph]()

        val storeStates: ListBuffer[State] = ProcessInstance.loadSubject(subjectID)
        val transitionList = ListBuffer[TransitionGraph]()


        /*
        init
         */
        val state1 = new StateGraph(1, OuterX / 2 - 50, 20, "A")
        val state2 = new StateGraph(2, OuterX / 2 - 50, 180, "A")
        val state3 = new StateGraph(3, OuterX / 2 - 150, 340, "A")
        val state4 = new StateGraph(4, OuterX / 2 + 50, 340, "E")
        val tr1 = new TransitionGraph(1001, 2, 1, SubToSubMessage(0, 1), state1.anchorBottom._1, state1.anchorBottom._2, state2.anchorTop._1, state2.anchorTop._2)
        val tr2 = new TransitionGraph(2001, 3, 2, SubToSubMessage(1, 1), state2.anchorLeft._1, state2.anchorLeft._2, state3.anchorTop._1, state3.anchorTop._2)
        val tr3 = new TransitionGraph(2002, 4, 2, SubToSubMessage(2, 1), state2.anchorRight._1, state2.anchorRight._2, state4.anchorTop._1, state4.anchorTop._2)
        val tr4 = new TransitionGraph(2003, 1, 2, SubToSubMessage(2, 2), state2.anchorRight._1, state2.anchorRight._2, state1.anchorTop._1, state1.anchorTop._2)

        state1.isStartState = true
        state1.addOutTransition(tr1)
        state2.addOutTransition(tr2)
        state2.addOutTransition(tr3)
        state2.addOutTransition(tr4)
        state2.addInTransition(tr1)
        state3.addInTransition(tr2)
        state4.addInTransition(tr3)
        state1.addInTransition(tr4)
        transitionList += tr1
        transitionList += tr2
        transitionList += tr3
        storeStates += state1
        storeStates += state2
        storeStates += state3
        storeStates += state4
        var stateType = "A"

        var startState: State = getStartState()
        var currentState: State = getStartState()
        storeStates.map(s => states += s.asInstanceOf[StateGraph]) //测试能否互相转换
        /*
        init end
        */

        dom.console.info("loaded Subject Page1")
        private var outerRef = Ref[html.Element]

        def init: Callback =
            outerRef.foreach(_.focus())

        def getStartState(): State = {
            storeStates.find(s => s.isStartState).get
        }

        def selectStateType(e: ReactEventFromInput) = {
            selectedStateType = e.target.value
            if (stateAttrMap.contains(selectedStateID)) {
                stateAttrMap(selectedStateID).changeStateType(selectedStateType)
                dom.console.log("selected state type " + stateAttrMap(selectedStateID).stateType)
            }
            e.preventDefaultCB >>
                    $.modState(_ + 0)
        }

        def updateStateAttr(e: ReactEventFromInput) = {
            dom.console.log("selected state id " + stateAttrMap)
            if (stateAttrMap.contains(selectedStateID)) {
                stateAttrMap(selectedStateID).changeStateType(selectedStateType)
                dom.console.log("selected state type " + stateAttrMap(selectedStateID).stateType)
            }
            e.preventDefaultCB >>
                    $.modState(_ + 0)
        }

        def addState(e: ReactEventFromInput) = {
            stateID += 1
            states.find(s => s.stateid == selectedStateID) match {
                case Some(state) => {
                    val nState = new StateGraph(stateID, state.x, state.y + 160, stateType)
                    val newTr = new TransitionGraph(state.stateid * 100 + state.outTransitions.length + 1, stateID, state.stateid, SubToSubMessage(0, 1),
                        state.anchorBottom._1, state.anchorBottom._2, nState.anchorTop._1, nState.anchorTop._2)
                    state.addOutTransition(newTr)
                    nState.addInTransition(newTr)
                    transitionList += newTr
                    states += nState
                    storeStates += nState
                    stateAttrMap += stateID -> StateAttributes(selectedStateType)
                }
                case None => {
                    val nState = new StateGraph(stateID, OuterX / 4, 160 * newState, stateType)
                    states += nState
                    storeStates += nState
                    newState += 1
                    stateAttrMap += stateID -> StateAttributes(selectedStateType)
                    dom.console.info("TODO")
                }
            }


            e.preventDefaultCB >>
                    $.modState(_ + 0)
        }

        def selectedState(sid: Int): Unit = {

            if (selectedStateID == sid) {
                states.find(s => s.stateid == selectedStateID) match {
                    case Some(state) => {
                        state.colour = "#800"
                    }
                    case None => {}
                }
                predStateID = -1
                selectedStateID = -1
            } else {
                if (selectedStateID != -1) {
                    predStateID = selectedStateID
                }
                selectedStateID = sid

                states.find(s => s.stateid == selectedStateID) match {
                    case Some(state) => {
                        state.colour = "#00bff3"
                    }
                    case None => {}
                }

                if (predStateID != -1) {
                    states.find(s => s.stateid == predStateID) match {
                        case Some(state) => {
                            state.colour = "#800"
                        }
                        case None => {}
                    }
                }


            }
        }

        def selectedTransition(tid: Int): Unit = {

            if (selectedTransitionID == tid) {
                transitionList.find(t => t.targetid == selectedTransitionID) match {
                    case Some(transitionGraph) => {
                        transitionGraph.colour = "#800"
                    }
                    case None => {}
                }
                selectedTransitionID = -1
            } else {
                if (selectedTransitionID != -1) {
                    transitionList.find(t => t.targetid == selectedTransitionID) match {
                        case Some(transitionGraph) => {
                            transitionGraph.colour = "#800"
                        }
                        case None => {}
                    }
                }

                selectedTransitionID = tid

                transitionList.find(t => t.targetid == selectedTransitionID) match {
                    case Some(transitionGraph) => {
                        transitionGraph.colour = "#00bff3"
                    }
                    case None => {}
                }

            }
        }


        def stateTest(e: ReactEventFromInput) = {
            dom.console.info("self: " + this.toString)
            e.preventDefaultCB
        }

        val connectButton = <.button("Connect", ^.onClick ==> connState)

        def connState(e: ReactEventFromInput) = {

            //      states.head.xx -= 200
            LinerTimeLayout.identifyEdgeTypes(getStartState(), Map(), Map(), 0)
            e.preventDefaultCB
        }


        def saveState(e: ReactEventFromInput) = {
            ProcessInstance.saveSubject(subjectID, storeStates)
            val url = "http://localhost:8080/#subjects"
            dom.window.location.href = url
            e.preventDefaultCB
        }


        def stateChange(e: ReactEventFromInput) = {
            stateType = e.target.value
            //      states.head.xx -= 200
            dom.console.info("state type: " + stateType)
            e.preventDefaultCB >>
                    $.modState(_ + 0)
        }

        def render() = {
            <.div(
                <.select(
                    ^.id := "stateType",
                    ^.onChange ==> stateChange,
                    <.option(^.value := "A", "Action"),
                    <.option(^.value := "S", "Send"),
                    <.option(^.value := "R", "Receive"),
                    <.option(^.value := "E", "End")
                ),
                <.button("Add state", ^.onClick ==> addState),
                <.button("Connect", ^.onClick ==> connState),
                <.button("save", ^.onClick ==> saveState),
                <.br,
                <.br,
                OuterDiv.withRef(outerRef)(
                    leftSide,
                    rightSide,
//                    footer,
                    body,
                    //            State(OuterX/2 - 50, 20, "A").state,
                    //            Arrow_Vertical(OuterX/2 - 75, 120).arrow
                    states.map(_.stateView()).toVdomArray,
                    transitionList.map(_.arrowView()).toVdomArray
                )
            )
        }


        case object LinerTimeLayout {

            trait EdgeType

            case object TreeEdge extends EdgeType

            case object BackEdge extends EdgeType

            case object ForwardEdge extends EdgeType

            case object CrossEdge extends EdgeType

            val branchingSplit = ListBuffer[State]()
            val branchingjoin = ListBuffer[State]()
            val edgeType = Map[Int, EdgeType]()
            val length = Map[Int, Int]().withDefaultValue(0)
            val parent = Map[Int, Int]() //key = son, value = parent
            val branchHeight = Map[Int, Int]().withDefaultValue(0)
            val branchWidth = Map[Int, Int]().withDefaultValue(0)
            val virtualEdges: ListBuffer[Int] = new ListBuffer[Int]()
            val backEdges = Set[Int]()
            val spacex: Int = 0
            val spacey: Int = 0


            def outEdges(node: State): Int = {

                return node.outTransitions.length
            }

            def inEdges(node: State): Int = {

                return node.inTransitions.length
            }

            /*
            确定反向边
            */
            def identifyEdgeTypes(n: State, discovered: Map[Int, Int], finished: Map[Int, Int], t: Int): Int = {
                var nt = t + 1
                discovered += n.stateid -> nt
                for (tr <- n.outTransitions) {
                    val next = storeStates.find(s => s.stateid == tr.targetid).get
                    if (!discovered.contains(next.stateid)) {
                        edgeType += tr.transitionid -> TreeEdge
                        nt = identifyEdgeTypes(next, discovered, finished, nt)
                    } else if (!finished.contains(next.stateid)) {
                        edgeType += tr.transitionid -> BackEdge
                    } else if (discovered(next.stateid) > discovered(n.stateid)) {
                        edgeType += tr.transitionid -> ForwardEdge
                    } else {
                        edgeType += tr.transitionid -> CrossEdge
                    }
                }
                finished += n.stateid -> (nt + 1)
                dom.console.info("edge types " + edgeType)
                nt + 1
            }

            def topologicalSort(nodes: ListBuffer[State], n: State): ListBuffer[State] = {
                val inEdges = Map[Int, Int]().withDefaultValue(0)
                for (s <- nodes) {
                    for (e <- s.outTransitions) {
                        if (!edgeType(e.transitionid).equals(BackEdge)) {
                            inEdges += e.targetid -> (inEdges.getOrElseUpdate(e.targetid, 0) + 1)
                        }
                    }
                }
                val topology = ListBuffer[State]()
                var i = 0
                topology(i) = n
                while (i != nodes.length) {
                    val nn = topology(i)
                    i += 1
                    for (e <- nn.outTransitions) {
                        if (!edgeType(e.transitionid).equals(BackEdge)) {
                            val t = storeStates.find(s => s.stateid == e.targetid).get
                            inEdges += t.stateid -> (inEdges(t.stateid) - 1)
                            if (inEdges(t.stateid) == 0) {
                                topology(topology.length) = t
                            }
                        }
                    }
                }
                return topology
            }

            def LPSTree(topology: ListBuffer[State]): Unit = {
                for (s <- topology) {
                    for (e <- s.outTransitions) {
                        if (!edgeType(e.transitionid).equals(BackEdge)) {
                            val t = storeStates.find(s => s.stateid == e.targetid).get
                            if (length.getOrElseUpdate(t.stateid, 0) < length.getOrElseUpdate(s.stateid, 0) + spacey) {
                                length(t.stateid) = length(s.stateid) + spacey
                                parent += (t.stateid) -> (s.stateid)
                            }
                        }
                    }
                }
            }

            def computeBranchDimensions(n: State): Unit = {
                var h, oh, w, ow, k = 0
                for (e <- n.outTransitions) {
                    if (parent(e.targetid) == n.stateid) {
                        computeBranchDimensions(storeStates.find(s => s.stateid == e.targetid).get)
                        oh = branchHeight(e.targetid)
                        ow = branchWidth(e.targetid)
                    } else {
                        oh = 0
                        ow = 0
                    }
                    h = h + k * spacey + oh
                    w = math.max(w, ow)
                    k = 1
                }
                //        h = math.max()
            }

            //      def entryNode(fragment: ListBuffer[State]): State ={
            //      //return the entry
            //      }
            //
            //      def exitNode(fragment: ListBuffer[State]): State ={
            //      //return the end
            //      }

            def fragments(): Unit = {
                //todo...
            }

            def splitOffMutipleEdges(): Unit = {
                /*
                repalce the Mutiple Edges with single virtual edge
                */
                for (state <- states) {
                    for (tr <- state.outTransitions) {

                    }
                }
            }

        }

    }

    val Main = ScalaComponent.builder[Unit]("states view")
            .initialState(stateID)
            .renderBackend[Backend]
            .componentDidMount(_.backend.init)
            .build


}
