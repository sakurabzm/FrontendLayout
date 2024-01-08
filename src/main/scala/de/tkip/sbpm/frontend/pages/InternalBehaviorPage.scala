package de.tkip.sbpm.frontend.pages

import de.tkip.sbpm.frontend.graph.{Graph, GraphObject, StateGraph}
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react.{Callback, _}
import japgolly.scalajs.react.vdom.html_<^.{^, _}
import org.scalajs.dom
import de.tkip.sbpm.frontend.AppRouter.{AppPage, BehaviorPage}
import de.tkip.sbpm.frontend.Data._
import de.tkip.sbpm.frontend.LayoutAlgorithm._
import de.tkip.sbpm.frontend.graph.GraphObject.Arrow
import de.tkip.sbpm.frontend.components._
import japgolly.scalajs.react.extra.router.RouterCtl

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set


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

        val buttonStyle2 = style(
            border :=! "none",
            borderRadius :=! "5px 5px 5px 5px",
            width :=! 90.px,
            height :=! 30.px,
            marginTop :=! 10.px,
            marginLeft :=! 20.px,
            &.hover(backgroundColor :=! "#4682B4")
        )

        val buttonSt = style(
            borderRadius :=! "13em/3em",
            width :=! 60.px,
            height :=! 30.px,
            border :=! "solid 1px transparent",
            fontSize :=! 12.px,
            &.hover(backgroundColor :=! "#4682B4")
        )

        val confirmButton = style(
            border :=! "solid 1px transparent",
            fontSize :=! 12.px,
            &.hover(backgroundColor :=! "#4682B4")
        )

        val textStyle = style(userSelect := "none")

    }

    case class State(statesList: ListBuffer[StateGraph])

    var stateIndex = "Action"
    var preSelected: Graph = null
    var preState, virtualStart: StateGraph = null
    var switch = true

    var originalStateData: (Int, Boolean, RestoredData) = null
    val bodyH = dom.window.innerHeight
    val bodyW = dom.window.innerWidth
    val maxNameLength = 18
    var defaultStateList = new ArrayBuffer[StateGraph]()
    //  lazy val stateMap: Map[Int, StateGraph] = ProcessList.processMap(processID).subjectMap(subjectID).stateMap
    //  lazy val arrowMap: Map[Int, Arrow] = ProcessList.processMap(processID).subjectMap(subjectID).arrowMap
    var processID: String = ""
    var subjectID: String = ""
    var currentCallMacroID: Int = -1
    var offsetx, offsety = 0
    var newStateId, newTransitionId = 0
    var statesList: ListBuffer[StateGraph] = ListBuffer()
    var stateMap: Map[Int, StateGraph] = Map()
    var arrowMap: Map[Int, Arrow] = Map()
    var currentStartNode: ListBuffer[Int] = ListBuffer()
    var sendingMessage: Map[Int, Map[Int, Set[String]]] = Map()
    var receivingMessage: Map[Int, Map[Int, Set[String]]] = Map()
    val determineTransitionSet, leavesVisitedSet, nonleavesVisitedSet: Set[Int] = Set()
    var zoomRate = 0.05 //每次放缩比例增量
    var maxRate = 1.0 //最大放大倍数
    var minRate = 0.6 //最小缩小倍数
    var currZoom = 1.0 //当前缩放比
    val mu = 5
    var isManual = false
    var lockStates: Map[Int, (Int, Int)] = Map()
    val draggingState: Map[Int, ListBuffer[Int]] = Map() // 当前的和之前的
    var multistart = false
    var autoLayout = false

    class Backend(val $: BackendScope[Unit, State]) {

        //val statesList: ListBuffer[Graph] = ProcessList.processMap(processID).subjectMap(subjectID).stateList
        //  lazy  val statesListOfCallMacro: ListBuffer[Graph] = ProcessList.processMap(processID).subjectMap(subjectID).callMacroMap(currentCallMacroID)
        private val slt = Ref[dom.html.Select]

        def render(s: State) = {
            if (currentCallMacroID > 0) {
                statesList = ProcessManager.processMap(processID).subjectMap(subjectID).callMacroMap(currentCallMacroID)
            } else {
                statesList = ProcessManager.processMap(processID).subjectMap(subjectID).stateList
                stateMap = ProcessManager.processMap(processID).subjectMap(subjectID).stateMap
                arrowMap = ProcessManager.processMap(processID).subjectMap(subjectID).arrowMap
                currentStartNode = ProcessManager.processMap(processID).subjectMap(subjectID).startStateList
            }
            <.div(
                Style.container,
                createMenu,
                createContent(s),
                createRightNavS(s)
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
                    "Back",
                    ^.onClick --> backPage
                ),
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
                    <.option("ModalJoin"),
                    <.option("ModalSplit"),
                    <.option("Tau"),
                    <.option("CloseIP"),
                    <.option("OpenIP"),
                    <.option("IsIPEmpty"),
                    <.option("CloseAllIPs"),
                    <.option("OpenAllIPs"),
                    <.option("SelectAgents"),
                    <.option("CallMacro"),
                    <.option("VarMan"),
                    <.option("Cancel"),
                    <.option("End"),
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
                    ^.onClick --> deleteEvent
                ),
                <.button(
                    Style.buttonStyle,
                    "Clear",
                    ^.onClick --> clearStates
                ),
                <.label(
                    ^.marginTop := 10.px,
                    ^.marginLeft := 20.px,
                    "Manual"
                ),
                <.input.checkbox(
                    ^.marginLeft := 10.px,
                    (^.checked := false).when(!isManual),
                    (^.checked := true).when(isManual),
                    ^.onClick --> manualOrAuto
                ),
                <.label(
                    ^.marginTop := 10.px,
                    ^.marginLeft := 20.px,
                    "Lock"
                ),
                lockStatePosition,
                <.button(
                    Style.buttonStyle,
                    "+",
                    ^.onClick --> zoomin
                ),
                <.button(
                    Style.buttonStyle,
                    "-",
                    ^.onClick --> zoomout
                ),
                {
                    SaveButtonComponents.pid = processID
                    SaveButtonComponents.sid = subjectID
                    SaveButtonComponents.b = $
                    SaveButtonComponents.SaveButton()
                },
                <.button(
                    Style.buttonStyle2,
                    "RPSTLayout",
                    ^.onClick --> rpstLayout
                ),
                <.button(
                    Style.buttonStyle2,
                    "TreeLayout",
                    ^.onClick --> treeLayout
                ),
                {
                    ExportButtonComponents.pid = processID
                    ExportButtonComponents.sid = subjectID
                    ExportButtonComponents.b = $
                    ExportButtonComponents.ExportButton()
                }

                //          "Export",
                //          ^.onClick --> exportSubject
            )
        }

        def createContent(state: State) = {
            <.div(
                ^.position.relative,
                ^.display.flex,
                ^.width := "84%",
                ^.minHeight := 600.px,
                ^.maxHeight := 600.px,
                ^.marginTop := 50.px,
                ^.marginLeft := 0.px,
                //^.backgroundColor := "blue", // content color
                ^.overflowX.scroll,
                ^.overflowY.scroll,
                <.div(
                    ^.position.absolute,
                    ^.width := "100%",
                    ^.backgroundColor := "pink",
                    ^.transform := s"scale($currZoom)",
                    statesList.toTagMod { item =>
                        val eventMap: Map[String, TagMod] = Map()
                        eventMap += (GraphObject.onClickKey -> onClickEvent(item))
                        eventMap += (GraphObject.onDragStartKey -> onDragStartEvent(item))
                        eventMap += (GraphObject.onDragKey -> onDragEvent(item))
                        eventMap += (GraphObject.onDoubleClickKey -> onDoubleClickEvent(item))
                        item.content(eventMap)
                    },

                    arrowMap.valuesIterator.toTagMod { arrow => {
                        val eventMap: Map[String, TagMod] = Map()
                        eventMap += (GraphObject.onClickKey -> onClickEvent(arrow))
                        arrow.content(eventMap)
                        }
                    }
                ),
                <.div(
                    ^.position.relative,
                    ^.id := "tip",
                    ^.marginLeft := "83%",
                    ^.marginTop := "1%",
                    ^.height := 80.px,
                    ^.width := 150.px,
                    ^.backgroundColor := "#F5F5F5",
                    ^.border := "dashed 2px #7EA6E0",
                    ^.borderRadius := 10.px,
                    ^.display.none.when(tipRelatedInformation == ""),
                    <.div(
                        ^.position.relative,
                        ^.marginTop := 2.px,
                        ^.marginLeft := 125.px,
                        ^.height := 18.px,
                        ^.width := 18.px,
                        ^.cursor.pointer,
                        <.div(
                            ^.position.relative,
                            ^.height := 3.px,
                            ^.width := 25.px,
                            ^.top := 0.px,
                            ^.left := (2).px,
                            ^.backgroundColor := "red",
                            ^.transformOrigin := "top left",
                            ^.transform := "rotate(45deg)"
                        ),
                        <.div(
                            ^.position.relative,
                            ^.height := 3.px,
                            ^.width := 25.px,
                            ^.top := 15.px,
                            ^.left := 0.px,
                            ^.backgroundColor := "red",
                            ^.transformOrigin := "top left",
                            ^.transform := "rotate(-45deg)"
                        ),
                        ^.onClick --> closeTipInformation
                    ),
                    <.div(
                        ^.position.relative,
                        ^.paddingTop := 5.px,
                        ^.height := 55.px,
                        ^.width := 150.px,
                        ^.wordWrap.`break-word`,
                        ^.fontSize := 14.px,
                        "Warning: " + tipRelatedInformation
                    )
                )
            )
        }

        def createRightNavS(s: State) = {
            <.div(
                ^.position.relative,
                ^.float.right,
                //^.display.flex,
                ^.width := "16%",
                ^.minHeight := 600.px,
                ^.maxHeight := 600.px,
                ^.marginTop := 50.px,
                ^.marginRight := 0.px,
                ^.backgroundColor := "#E6E6E6",
                ^.overflowX.scroll,
                ^.overflowY.scroll,
                <.header(
                    "Setting",
                    ^.textAlign.center,
                    ^.fontSize := 20.px
                ),
                <.br,
                <.br,
                <.ul(
                    ^.whiteSpace.nowrap,
                    ^.listStyle := "none",
                    <.li(
                        startStateCheckbox(),
                        <.label(
                            ^.marginLeft := 20.px,
                            "start state"
                        )
                    ),
                    <.br,
                    <.li(
                        "StateID: ",
                        <.br,
                        <.br,
                        <.label(
                            if (preSelected != null && preSelected.isInstanceOf[StateGraph])
                                preSelected.asInstanceOf[StateGraph].data.id
                            else "",
                            ^.textAlign.center,
                            ^.display.block,
                            ^.width := "85%",
                            ^.height := 20.px
                        )
                    ),
                    <.br,
                    <.li(
                        ^.display.inline,
                        "Type : ",
                        optionOfSelect
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "Name: ",
                        <.br,
                        <.br,
                        <.input.text(
                            ^.width := "85%",
                            if ((preSelected != null) && (preSelected.isInstanceOf[StateGraph])) {
                                ^.value := preSelected.asInstanceOf[StateGraph].data.stateName
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> changeStateName(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "priority of action:",
                        <.input.number(
                            ^.min := 0,
                            ^.max := 100,
                            ^.marginLeft := 10.px,
                            ^.width := 35.px,
                            if ((preSelected != null) && (preSelected.isInstanceOf[StateGraph])) {
                                ^.disabled := false
                                ^.value := preSelected.asInstanceOf[StateGraph].data.priority
                            } else {
                                ^.disabled := true
                                ^.value := ""
                            },
                            ^.onChange ==> getPriorityAction(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "select agents: ",
                        <.br,
                        <.br,
                        <.select(
                            ^.width := "85%",
                            if (preSelected != null && preSelected.isInstanceOf[Arrow]) {
                                ^.disabled := false
                            } else ^.disabled := true,
                            <.option("please select agent!"),
                            //currentAgentsList(isSelected),
                            ^.onChange ==> getAgent(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "More agents: ",
                        <.br,
                        <.br,
                        <.input(
                            ^.width := "85%",
                            if (preSelected != null && preSelected.isInstanceOf[Arrow]) {
                                ^.disabled := false
                            } else ^.disabled := true,
                            ^.onChange ==> getMoreAgent(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        <.p(
                            "Variable Manipulation: ",
                            ^.fontSize := 14.px,
                            ^.fontWeight.bold
                        ),
                        <.br,
                        "varType: v1",
                        <.br,
                        <.input.text(
                            ^.width := "85%",
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow]) && checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.disabled := false
                            } else {
                                ^.disabled := true
                            },
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow]) && checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.variableOperation.v1
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getVarTypeV1(preSelected)
                        ),
                        <.br,
                        <.br,
                        "operation: ",
                        <.br,
                        <.select(
                            ^.width := "85%",
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow]) && checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.disabled := false
                            } else {
                                ^.disabled := true
                            },
                            <.option("clear",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "clear")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("Assign",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "Assign")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("Concatenation",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "Concatenation")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("Intersection",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "Intersection")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("Difference",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "Difference")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("Selection",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "Selection")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("ExtractContent",
                                if ((preSelected != null) &&
                                        preSelected.isInstanceOf[Arrow] &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "ExtracContent")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("ExtractChannel",
                                if ((preSelected != null) &&
                                        preSelected.isInstanceOf[Arrow] &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "ExtractChannel")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            <.option("ExtractCorrelationID",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.variableOperation.operation == "ExtractCorrelationID")) {
                                    ^.selected := true
                                } else ^.selected := false),
                            ^.onChange ==> getVarOperation(preSelected)
                        ),
                        <.br,
                        <.br,
                        "varType: v2",
                        <.br,
                        <.input.text(
                            ^.width := "85%",
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow]) && checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.disabled := false
                            } else {
                                ^.disabled := true
                            },
                            if ((preSelected != null) &&
                                    (preSelected.isInstanceOf[Arrow]) && checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.variableOperation.v2
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getVarTypeV2(preSelected)
                        ),
                        <.br,
                        <.br,
                        "targetType: ",
                        <.br,
                        <.input.text(
                            ^.width := "85%",
                            if ((preSelected != null) &&
                                    (preSelected.isInstanceOf[Arrow]) &&
                                    checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.disabled := false
                            } else {
                                ^.disabled := true
                            },
                            if ((preSelected != null) &&
                                    preSelected.isInstanceOf[Arrow] &&
                                    checkVarMan(preSelected.asInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.variableOperation.result
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getTarget(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        <.p(
                            "Transition: ",
                            ^.fontWeight.bold,
                            ^.fontSize := 14.px
                        ),
                        "Type: ",
                        <.select(
                            ^.width := 82.px,
                            ^.marginLeft := 20.px,
                            <.option("normal",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.transitionType == "normal")) {
                                    ^.selected := true
                                } else {
                                    ^.selected := false
                                }),
                            <.option("hidden",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.transitionType == "hidden")) {
                                    ^.selected := true
                                } else {
                                    ^.selected := false
                                }),
                            <.option("timeout",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.transitionType == "timeout")) {
                                    ^.selected := true
                                } else {
                                    ^.selected := false
                                }),
                            <.option("auto",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.transitionType == "auto")) {
                                    ^.selected := true
                                } else {
                                    ^.selected := false
                                }),
                            <.option("cancel",
                                if ((preSelected != null) &&
                                        (preSelected.isInstanceOf[Arrow]) &&
                                        (preSelected.asInstanceOf[Arrow].data.transitionType == "cancel")) {
                                    ^.selected := true
                                } else {
                                    ^.selected := false
                                }),
                            ^.onChange ==> getTransitionType(preSelected)
                        ),
                        <.br,
                        <.br,
                        <.li(
                            "Multi Transitions: ",
                            <.button(
                                ^.borderRadius := "5px 5px 5px 5px",
                                ^.width := 40.px,
                                ^.height := 20.px,
                                ^.marginTop := 10.px,
                                "Add",
                                ^.cursor.pointer,
                                if (preSelected != null && preSelected.isInstanceOf[Arrow] && checkMessageState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                ^.onClick --> addMultiTransitions
                            ),
                            <.button(
                                ^.borderRadius := "5px 5px 5px 5px",
                                ^.width := 40.px,
                                ^.height := 20.px,
                                ^.marginTop := 10.px,
                                "Del",
                                ^.cursor.pointer,
                                if (preSelected != null && preSelected.isInstanceOf[Arrow] && checkMessageState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                ^.onClick --> delMultiTransitions(preSelected)
                            ),
                            <.br,
                            "List of Transitions: ",
                            <.select(
                                ^.width := 40.px,
                                if (preSelected != null && preSelected.isInstanceOf[Arrow] && checkMessageState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                multiTransionsList,
                                ^.onChange ==> getSelectTransition(preSelected)
                            )
                        ),
                        <.br,
                        <.br,
                        <.li(
                            "Message Type: ", // transition with communication message
                            <.br,
                            <.br,
                            <.select(
                                ^.width := "85%",
                                if (preSelected != null && preSelected.isInstanceOf[Arrow] && checkMessageState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                <.option(
                                    "select message type.",
                                    ^.selected := true
                                ),
                                transitionMsgT,
                                ^.onChange ==> getMessageTypeOfTransition(preSelected)
                            )
                        ),
                        <.br,
                        <.br,
                        <.li(
                            "create new message type!",
                            <.br,
                            <.br,
                            <.input.text(
                                ^.id := "createMsg",
                                ^.width := "85%",
                                if (preSelected != null && (preSelected.isInstanceOf[Arrow]) && checkCommunicationState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                ^.onChange ==> createNewMessageType(preSelected)
                            )
                        ),
                        <.br,
                        <.br,
                        <.li(
                            "Related Subject : ",
                            <.br,
                            <.br,
                            <.select(
                                ^.width := "85%",
                                if (preSelected != null && preSelected.isInstanceOf[Arrow] && checkMessageState(preSelected.asInstanceOf[Arrow])) {
                                    ^.disabled := false
                                } else ^.disabled := true,
                                <.option("select related subject!"),
                                transitionRelatedS, // transition
                                ^.onChange ==> getRelatedSubjectNameOfTransition(preSelected)
                            )

                        ),
                        <.br,
                        <.br,
                        "Time: ",
                        <.input.number(
                            ^.width := 80.px,
                            ^.marginLeft := 20.px,
                            ^.min := 0,
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.timeout
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getTransitionTimeout(preSelected)
                        ),
                        "s",
                        <.br,
                        <.br,
                        "priority of transition :",
                        <.br,
                        <.input.number(
                            ^.min := 0,
                            if ((preSelected != null) && (preSelected.isInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.priority
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getPriorityTransition(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "Activate stateID: ",
                        <.br,
                        <.br,
                        <.input(
                            ^.width := "85%",
                            if (preSelected != null && (preSelected.isInstanceOf[Arrow])) {
                                ^.value := preSelected.asInstanceOf[Arrow].data.activatedStateID
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> getActivatedStateID(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "Label : ",
                        <.br,
                        <.br,
                        <.textarea(
                            ^.maxHeight := 70.px,
                            ^.minHeight := 70.px,
                            ^.maxWidth := 140.px,
                            ^.minWidth := 140.px,
                            if (preSelected != null) {
                                if (preSelected.isInstanceOf[StateGraph]) ^.value := preSelected.asInstanceOf[StateGraph].data.description
                                else ^.value := preSelected.asInstanceOf[Arrow].data.label
                            } else ^.value := "",
                            ^.onChange ==> changeDescription(preSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.br,
                    <.li(
                        ^.margin.auto,
                        <.button(
                            Style.buttonSt,
                            "update",
                            ^.onClick --> updateEvent(preSelected)))
                )
            )
        }

        def optionOfSelect: TagMod = {
            <.select(
                if (preSelected.isInstanceOf[StateGraph]) ^.disabled := false
                else ^.disabled := true,
                <.option(
                    "Action",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "Action"))),
                <.option(
                    "Send",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "Send"))),
                <.option(
                    "Receive",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "Receive"))),
                <.option(
                    "ModalJoin",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "ModalJoin"))),
                <.option(
                    "ModalSplit",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "ModalSplit"))),
                <.option(
                    "Tau",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "Tau"))),
                <.option(
                    "CloseIP",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "CloseIP"))),
                <.option(
                    "OpenIP",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "OpenIP"))),
                <.option(
                    "IsIPEmpty",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "IsIPEmpty"))),
                <.option(
                    "CloseAllIPs",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "CloseAllIPs"))),
                <.option(
                    "OpenAllIPs",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "OpenAllIPs"))),
                <.option(
                    "SelectAgents",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "SelectAgents"))),
                <.option(
                    "CallMacro",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "CallMacro"))),
                <.option(
                    "VarMan",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "VarMan"))),
                <.option(
                    "Cancel",
                    (^.selected := true).when((preSelected != null) && (!preSelected.isInstanceOf[Arrow]) && (preSelected.asInstanceOf[StateGraph].data.stateType == "Cancel"))),
                ^.onChange ==> changeStateType(preSelected)
            )
        }

        def manualOrAuto: Callback = {
            if (isManual) {
                isManual = false
            } else {
                isManual = true
            }
            $.modState(s => s)
        }

        def zoomout: Callback = {
            if ((currZoom - zoomRate) > minRate) {
                currZoom -= zoomRate
            } else {
                currZoom = minRate
            }
            $.modState(s => State(statesList))
        }

        def zoomin: Callback = {
            if ((currZoom + zoomRate) > 1.0) {
                currZoom = maxRate
            } else {
                currZoom += zoomRate
            }
            $.modState(s => State(statesList))
        }

        def closeTipInformation: Callback = {
            tipRelatedInformation = ""
            $.modState(s => s)
        }

        def backPage = Callback {
            stateIndex = "Action"
            preState = null

            if (preSelected != null) {
                if (preSelected.isInstanceOf[Arrow]) {
                    restoreTransitionData(preSelected.asInstanceOf[Arrow])
                    originalTransitionData = null
                    preSelected.asInstanceOf[Arrow].resetColor
                } else {
                    restoreStateData(preSelected.asInstanceOf[StateGraph])
                    originalStateData = null
                    preSelected.asInstanceOf[StateGraph].resetBorder
                }
                preSelected = null
            } else {
                originalTransitionData = null
                originalStateData = null
            }

            val url = "http://localhost:8080/#subjectsView/" + processID
            dom.window.location.href = url
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
                    initialS.setCoordinate(10, i * 100)
                    isFull = false
                }
            }
            if (isFull) {
                defaultStateList += initialS
                initialS.setCoordinate(10, (defaultStateList.length) * 10)
            }
        }

        def createCorrespondingState: Callback = {
            val sd = new StateData(getStateId())
            sd.stateType = stateIndex
            sd.setStateName(stateIndex)
            val s = GraphObject.createGraph(stateIndex, sd, processID, subjectID)
            statesList += s
            stateMap += (s.data.ID -> s)
            positionOfAdd(s)
            $.modState(s => State(statesList))
        }
        def getStateId(): Int = {
            if(newStateId == 0 && statesList.nonEmpty){
                newStateId = statesList.sortBy(_.data.id).last.data.id + 1
            }else{
                newStateId += 1
            }
            newStateId
        }

        def getTransitionId(): Int = {
            if(newTransitionId == 0 && arrowMap.nonEmpty){
                newTransitionId = arrowMap.valuesIterator.toList.sortBy(_.data.id).last.data.id + 1
            }else{
                newTransitionId += 1
            }
            newTransitionId
        }

        def delMultiTransitions(s:Graph): Callback ={
            val td = s.asInstanceOf[Arrow].data
            val state = stateMap(td.source)
            if(state.childrenList.contains(td.target)){
                state.data.removeDirectChildrenTransition(td)
            }else if(state.descendantList.contains(td.target)){
                state.data.removeNonDirectChildrenTransition(td)
            }
            $.modState(s => s)
        }

        def addMultiTransitions: Callback ={
            if (preSelected != null && preSelected.isInstanceOf[Arrow]) {
                val td = preSelected.asInstanceOf[Arrow].data
                td.setMultiTransitions(true)
                val newTransitionData = new TransitionData(getTransitionId(), td.source, td.target)
                newTransitionData.setMultiTransitions(true)
                if(stateMap(td.source).childrenList.contains(td.target)){
                    stateMap(td.source).data.addDirectChildrenTransition(newTransitionData)
                }else if(stateMap(td.source).descendantList.contains(td.target)){
                    stateMap(td.source).data.addNonDirectChildrenTransition(newTransitionData)
                }
            }
            $.modState(s => s)
        }

        def initStates: Unit = {
            preState = null
            preSelected = null
            originalStateData = null
            originalTransitionData = null
        }

        def clearStates: Callback = {
            newStateId = 0
            newTransitionId = 0
            statesList.clear()
            currentStartNode.clear()
            stateMap.clear()
            arrowMap.clear()
            defaultStateList.clear()
            leavesList.clear()
            preState = null
            preSelected = null
            existStart = false
            originalStateData = null
            originalTransitionData = null

            $.modState(s => State(statesList))
        }

        def deleteEvent: Callback = {
            if (preSelected != null) {
                if (preSelected.isInstanceOf[StateGraph]) { // Delete State
                    if (!preSelected.asInstanceOf[StateGraph].data.isStartState) {
                        // delete all data from other states
                        statesList -= preSelected.asInstanceOf[StateGraph]
                        val deleteObject = preSelected.asInstanceOf[StateGraph]
                        stateMap.remove(deleteObject.data.ID)

                        deleteObject.arrowsToTarget.foreach(id => {
                            val target = stateMap(arrowMap.remove(id).get.data.target)
                            // target 可能是直接子节点也可能后代节点
                            target.arrowsFromSource -= id
                            if (target.parentID == (deleteObject.data.id)) {
                                target.parentID = -1
                            }
                        })
                        deleteObject.arrowsFromSource.foreach(id => {
                            val source = stateMap(arrowMap.remove(id).get.data.source)
                            if (source.childrenList.contains(deleteObject.data.id)) {
                                source.childrenList -= deleteObject.data.id
                            }
                            if (source.descendantList.contains(deleteObject.data.id)) {
                                source.descendantList -= deleteObject.data.id
                            }
                            source.arrowsToTarget -= id
                            // 分别从直接子节点列表和非直接子节点列表中删除
                            val td = arrowMap(id).data
                            source.data.directChildrenTransitionsMap.foreach(t => {
                                if (t._2.contains(td)) {
                                    source.data.removeDirectChildrenTransition(td)
                                }
                            })
                            source.data.nonDirectChildrenTransitionsMap.foreach(t => {
                                if (t._2.contains(td)) {
                                    source.data.removeNonDirectChildrenTransition(td)
                                }
                            })
                        })
                        if (preState == deleteObject) {
                            preState = null
                        }
                        preSelected = null
                        val layout = checkIsolatedNode
                        if (layout) {
//                            if (currentStartNode.size > 1) {
//                                newDummyNode
//                            }
                            treeLayout(findRootNode)
                        }
                        $.modState(s => State(statesList))
                    } else {
                        Callback.alert("The current state is StartState")
                    }
                } else { // Delete transition
                    val aim = preSelected.asInstanceOf[Arrow]
                    val trData = TransitionData(aim.data.id, aim.data.source, aim.data.target)
                    val sid = aim.data.source
                    val tid = aim.data.target
                    val sourceNode = stateMap(sid)
                    val targetNode = stateMap(tid)
                    sourceNode.arrowsToTarget -= aim.data.ID
                    if (sourceNode.data.directChildrenTransitionsMap(tid).contains(trData)) {
                        sourceNode.data.removeDirectChildrenTransition(trData)
                    }
                    if (sourceNode.data.nonDirectChildrenTransitionsMap(tid).contains(trData)) {
                        sourceNode.data.removeNonDirectChildrenTransition(trData)
                    }
                    targetNode.arrowsFromSource -= aim.data.ID
                    if (targetNode.parentID == sid) { // 删除直接子节点
                        sourceNode.childrenList -= tid
                        targetNode.parentID = -1
                    } else { // 删除孙节点
                        sourceNode.descendantList -= tid
                    }
                    val isLayout = checkIsolatedNode
                    if (isLayout) {
//                        if (currentStartNode.size > 1) {
//                            newDummyNode
//                        }
                        treeLayout(findRootNode)
                    }
                    arrowMap -= aim.data.id
                    preSelected = null
                    $.modState(s => State(statesList))
                }
            } else Callback()
        }

        val isolatedNodeList: Set[Int] = Set()

        def checkIsolatedNode: Boolean = {
            var layout = true
            var suspiciousNode: ListBuffer[StateGraph] = ListBuffer()
            for (s <- statesList) {
                if (s.asInstanceOf[StateGraph].parentID == -1) {
                    isolatedNodeList += s.asInstanceOf[StateGraph].data.id
                    suspiciousNode += s.asInstanceOf[StateGraph]
                }
            }
            for (node <- suspiciousNode) {
                if (node.arrowsFromSource.nonEmpty) {
                    isolatedNodeList -= node.data.id
                }
            }
            //TODO 把孤立节点作为假起点加入currentStartState
            if (isolatedNodeList.isEmpty) {
                layout = true
            } else {
                isolatedNodeList.foreach(node => {
                    currentStartNode += node
                    val currentIsolatedNode = stateMap(node)
                    currentIsolatedNode.parentID = -100
                    currentIsolatedNode.isolatedNodeColor
                })
                layout = false
            }
            layout
        }

        def lockStatePosition: TagMod= {
            if ((preSelected != null) && (preSelected.isInstanceOf[StateGraph])) {
                <.input.checkbox(
                    ^.checked := preSelected.asInstanceOf[StateGraph].lock,
                    ^.onChange --> changeLockState
                )
            } else {
                <.input.checkbox(
                    ^.checked := false
                )
            }
        }

        def changeLockState: Callback = {
            if ((preSelected.asInstanceOf[StateGraph].lock)) {
                preSelected.asInstanceOf[StateGraph].lock = false
                preSelected.asInstanceOf[StateGraph].resetBackgroundColor
                preSelected.asInstanceOf[StateGraph].resetBackgroundColor
                lockStates.remove(preSelected.asInstanceOf[StateGraph].data.id)
            } else {
                preSelected.asInstanceOf[StateGraph].lock = true
                preSelected.asInstanceOf[StateGraph].isolatedNodeColor
                val currentCoordinateX = preSelected.asInstanceOf[StateGraph].sx
                val currentCoordinateY = preSelected.asInstanceOf[StateGraph].sy
                lockStates += preSelected.asInstanceOf[StateGraph].data.id -> (currentCoordinateX, currentCoordinateY)
                preSelected.asInstanceOf[StateGraph].changeBackgroundColor
            }
            $.modState(s => s)
        }

        def startStateCheckbox() = {
            if ((preSelected != null) && (preSelected.isInstanceOf[StateGraph])) {
                <.input.checkbox(
                    ^.checked := preSelected.asInstanceOf[StateGraph].data.isStartState,
                    ^.onChange --> setStartState
                )
            } else {
                <.input.checkbox(
                    ^.checked := false
                )
            }
        }

        def changeStateType(s: Graph)(e: ReactEventFromInput): Callback = {
            originalStateData = (originalStateData._1, false, originalStateData._3)
            val newST = e.target.value
            if (s.isInstanceOf[StateGraph]) {
                s.asInstanceOf[StateGraph].data.setStateType(newST)
            }
            $.modState(s => s)
        }

        def setStartState: Callback = {
            if ((preSelected.asInstanceOf[StateGraph].data.isStartState)) {
                preSelected.asInstanceOf[StateGraph].data.isStartState = false
            } else {
                preSelected.asInstanceOf[StateGraph].data.isStartState = true
            }
            $.modState(s => s)
        }

        def onDragStartEvent(graph: StateGraph): TagMod = {
            if (isManual) {
                ^.onDragStart ==> dragStart(graph)
            } else {
                <.div()
            }
        }

        def dragStart(graph: StateGraph)(e: ReactMouseEvent): Callback = {
            offsetx = e.pageX.toInt - graph.sx
            offsety = e.pageY.toInt - graph.sy
            Callback()
        }

        def onDragEvent(graph: StateGraph): TagMod = {
            if (isManual) {
                ^.onDrag ==> dragState(graph)
            } else {
                <.div()
            }

        }

        def dragState(graph: StateGraph)(e: ReactMouseEventFromHtml): Callback = {
            e.persist()

            var x = e.pageX.toInt
            var y = e.pageY.toInt
            if (x <= 0 || y <= 0) {
                return e.preventDefaultCB
            } else {
                x -= offsetx
                y -= offsety
            }
            graph.dragging = true
            if (!draggingState.contains(graph.data.id)) {
                draggingState += graph.data.id -> ListBuffer()
            }
            graph.setCoordinate(x, y)
            val graphCenter = getSymbolCenter(graph)
            graph.arrowsFromSource.foreach(f => {
                arrowMap(f).changeTargetCoordinate(x + graphCenter._1, y)
                val preNode = stateMap(arrowMap(f).data.source)
                val preNodeCenter = getSymbolCenter(preNode)
//                preNode.dragging = true
                draggingState(graph.data.id) += preNode.data.id
                val entireList: ListBuffer[ListBuffer[TransitionData]] = ListBuffer()
                entireList ++= preNode.data.directChildrenTransitionsMap.valuesIterator
                entireList ++= preNode.data.nonDirectChildrenTransitionsMap.valuesIterator
                if (entireList.size > 1) {
                    for (elem <- entireList) {
                        elem.foreach(f => arrowMap(f.id).changeSourceCoordinate(preNode.sx + preNodeCenter._1, preNode.sy + preNodeCenter._2 * 2)
                        )
                    }
                }
            })
            graph.arrowsToTarget.foreach(f => {
                arrowMap(f).changeSourceCoordinate(x + graphCenter._1, y + graphCenter._2 * 2)
            })

            e.preventDefaultCB >> $.modState(s => s)
        }

        def changeStateName(s: Graph)(e: ReactEventFromInput): Callback = {
            originalStateData = (originalStateData._1, false, originalStateData._3)
            var offset = 0
            val newName = e.target.value
            if (s.isInstanceOf[StateGraph]) {
                val a = s.asInstanceOf[StateGraph]
                val currentState = s.asInstanceOf[StateGraph]
                if (currentState.isInstanceOf[GraphObject.Action]) {
                    if (!defaultStateList.contains(currentState)) {
                        if(newName.length <= maxNameLength){
                            if (newName.length > 8) {
                                offset = (newName.length - currentState.data.stateName.length) * 5
                                val x = currentState.sx
                                val y = currentState.sy
                                currentState.setCoordinate(x - offset, y)
                                currentState.graphWidth = currentState.asInstanceOf[GraphObject.Action].changeWidth(newName)
                            } else {
                                currentState.graphWidth = 80
                                val a = currentState.data.stateName.length //原本就大于8
                                if (a >= 8) {
                                    val newOffset = (Math.min(a, maxNameLength) - 8) * 5
                                    val x = currentState.sx
                                    val y = currentState.sy
                                    currentState.setCoordinate(x + newOffset, y)
                                }
                            }
                        }
                    }
                }
                a.data.setStateName(newName)
            }
            $.modState(s => s)
        }

        def changeDescription(s: Graph)(e: ReactEventFromInput): Callback = { // ok
            val dsc = e.target.value
            if (s.isInstanceOf[StateGraph]) {
                originalStateData = (originalStateData._1, false, originalStateData._3)
                s.asInstanceOf[StateGraph].data.description = dsc
            } else {
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                s.asInstanceOf[Arrow].data.setDescription(dsc)
            }
            $.modState(s => s)
        }

        def getAgent(s: Graph)(e: ReactEventFromInput): Callback = {
            val newAgent = e.target.value
            if (s.isInstanceOf[Arrow]) {
                s.asInstanceOf[Arrow].data.agent = newAgent
            }
            $.modState(s => s)
        }

        def getMoreAgent(s: Graph)(e: ReactEventFromInput): Callback = {
            val newAgent = e.target.value
            if (s.isInstanceOf[Arrow] && (newAgent != "please select agent!")) {
                s.asInstanceOf[Arrow].data.agent = newAgent
            }
            $.modState(s => s)
        }

        def getVarTypeV1(s: Graph)(e: ReactEventFromInput): Callback = {
            val newVarType = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val edge = s.asInstanceOf[Arrow]
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                val source = stateMap(edge.data.source)
                if (source.isInstanceOf[GraphObject.VarMan]) {
                    edge.data.variableOperation.setV1(newVarType)
                }
            }
            $.modState(s => s)
        }

        def getVarTypeV2(s: Graph)(e: ReactEventFromInput): Callback = {
            val newVarType = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val edge = s.asInstanceOf[Arrow]
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                val source = stateMap(edge.data.source)
                if (source.isInstanceOf[GraphObject.VarMan]) {
                    edge.data.variableOperation.setV2(newVarType)
                }
            }
            $.modState(s => s)
        }

        def getTarget(s: Graph)(e: ReactEventFromInput): Callback = {
            val newResultType = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val edge = s.asInstanceOf[Arrow]
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                val source = stateMap(edge.data.source)
                if (source.isInstanceOf[GraphObject.VarMan]) {
                    edge.data.variableOperation.setResult(newResultType)
                }
            }
            $.modState(s => s)
        }

        def getVarOperation(s: Graph)(e: ReactEventFromInput): Callback = {
            val newOperation = e.target.value
            if (s.isInstanceOf[Arrow]) {
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                val edge = s.asInstanceOf[Arrow]
                val source = stateMap(edge.data.source)
                if (source.isInstanceOf[GraphObject.VarMan]) {
                    edge.data.variableOperation.setOperation(newOperation)
                }
            }
            $.modState(s => s)
        }

        def onClickEvent(graph: Graph): TagMod = {
            ^.onClick --> selectedEvent(graph)
        }

        //查找树
        val aLittleTree: ListBuffer[Int] = ListBuffer()

        def searchTree(currentNode: StateGraph): Unit = {
            if (currentNode.childrenList.isEmpty) {
                // 子节点为空，后代节点先不考虑
                aLittleTree += currentNode.data.id
            } else {
                currentNode.childrenList.foreach(chl => {
                    val childrenNode = stateMap(chl)
                    childrenNode.deep = currentNode.deep + 1
                    searchTree(childrenNode)
                })
                aLittleTree += currentNode.data.id
            }
        }

        // 产生新的虚拟节点
        def newDummyNode {
            val specialStateID = -100
            val sd = new StateData(getStateId())
            val dummyState = new GraphObject.DummyNode(processID, subjectID, sd)
            dummyState.deep = -1
            statesList += dummyState
            stateMap += dummyState.data.id -> dummyState
            currentStartNode.foreach(start => dummyState.childrenList += start)
        }

        def findRootNode: Int = {
            var rootNode = 0
            val startList = currentStartNode
            if (startList.size == 1) {
                rootNode = startList.head
            } else {
                // 给出虚拟节点
                rootNode = -100
            }
            rootNode
        }

        val leavesList: ListBuffer[Int] = ListBuffer()

        def findLeaf(currentNode: StateGraph): Unit = {
            if(!leavesVisitedSet.contains(currentNode.data.id)){
                leavesVisitedSet.add(currentNode.data.id)
                var isTemp = false
                val allDescendNodes = (currentNode.childrenList ++ currentNode.descendantList).clone()
                if (allDescendNodes.isEmpty) {
                    leavesList += currentNode.data.id
                    return
                }
                if (currentNode.childrenList.isEmpty && currentNode.descendantList.nonEmpty) {
                    isTemp = true
                }
                for (node <- allDescendNodes) {
                    val descendNode = stateMap(node)
                    // 直接子节点
                    if (descendNode.parentID == currentNode.data.id) {
                        findLeaf(descendNode)
                    }
                    // 非直接子节点，有连接
                    if (descendNode.parentID == -1 && !currentStartNode.contains(descendNode.data.id)) {
                        descendNode.parentID = currentNode.data.id
                        descendNode.deep = currentNode.deep + 1
                        if(!currentNode.childrenList.contains(descendNode.data.id)){
                            currentNode.childrenList += descendNode.data.id
                        }
                        currentNode.descendantList -= descendNode.data.id
                        descendNode.childrenList.foreach(chl => {
                            stateMap(chl).deep = descendNode.deep + 1
                        })
                        findLeaf(descendNode)
                    }
                }
                if (isTemp) {
                    if (currentNode.childrenList.isEmpty) {
                        leavesList += currentNode.data.id
                    }
                }
            }
        }

        def allocateNonLeavesCoordinate(currentID: Int): Unit = {
            if(!nonleavesVisitedSet.contains(currentID)){
                nonleavesVisitedSet.add(currentID)
                val currentNode =
                    if(currentID == -100)
                        virtualStart
                    else
                        stateMap(currentID)
                val currentChildrenList = currentNode.childrenList
                if (!currentNode.sign) {
                    currentChildrenList.foreach(chl => {
                        allocateNonLeavesCoordinate(chl)
                    })

                    val firstChild = stateMap(currentNode.childrenList.head)
                    val lastChild = stateMap(currentNode.childrenList.last)
                    val rootX = (firstChild.sx + getSymbolCenter(firstChild)._1 + lastChild.sx + getSymbolCenter(lastChild)._1) / 2 - getSymbolCenter(currentNode)._1
                    val rootY = (currentNode.deep + 1) * 150
                    currentNode.setCoordinate(rootX, rootY)
                    currentNode.sign = true
                    println(s"分配节点坐标 id: ${currentNode.data.id}, x: $rootX, y: $rootY")
                    println(s"子节点 x1: ${firstChild.sx}, x2: ${lastChild.sx}")
                }
            }
        }

        def resetAllDataOfState: Unit = {
            statesList.foreach(s => {
                if (s.isInstanceOf[StateGraph]) {
                    val action = s
                    action.resetAllData
                }
            })
        }

        // todo 需要分类么？
        // Map(stateDeep, Map(stateX, stateID))
        val classifyNodes: Map[Int, Map[Int, Int]] = Map()
        val classificationResult: Map[Int, ListBuffer[Int]] = Map()
        //    def sortNodes: Unit ={
        //      statesList.foreach( node => {
        //        val currentState = node.asInstanceOf[StateGraph]
        //        val currentStateDeep = currentState.deep
        //        val currentStateX = currentState.sx
        //        if(classifyNodes.contains(currentStateDeep)){
        //          classifyNodes(currentStateDeep) += (currentStateX -> currentState.data.id)
        //        }else{
        //          classifyNodes += currentStateDeep -> Map()
        //          classifyNodes(currentStateDeep) += (currentStateX -> currentState.data.id)
        //        }
        //      })
        //      classifyNodes.foreach(states => {
        //        val currentDeepAllNodesX = states._2.keySet.toList.sorted // 同层节点大小从小到大排序
        //        classificationResult += states._1 -> ListBuffer()
        //        currentDeepAllNodesX.foreach( n => {
        //          val stateID = classifyNodes(states._1)(n)
        //          classificationResult(states._1) += stateID
        //        })
        //      })
        //    }

        // 树形布局
        def treeLayout(rootID: Int): Unit = {
            if (!autoLayout) {
                val rootState =
                    if(rootID == -100)
                        virtualStart
                    else
                        stateMap(rootID)

                arrowMap.clear()
                determineTransitionSet.clear()
                leavesVisitedSet.clear()
                nonleavesVisitedSet.clear()
                resetAllDataOfState
                leavesList.clear()
                //      classifyNodes.clear()
                //      classificationResult.clear()
                findLeaf(rootState)
                var blank = (bodyW / 5).toInt
                if (leavesList.size > 5) {
                    blank = (bodyW / 8).toInt
                }
                val stepX = 250
                val stepY = 150
                for (i <- 0 until leavesList.size) {
                    val leafID = leavesList(i)
                    val leafState = stateMap(leafID)
                    val leafDeep = leafState.deep
                    val stateCoordinateX = blank + (stepX * (i + 1))
                    val stateCoordinateY = stepY * (leafDeep + 1)
                    leafState.setCoordinate(stateCoordinateX, stateCoordinateY)
                    leafState.sign = true
                }
                val startNode = findRootNode
                allocateNonLeavesCoordinate(startNode) // 深度优先遍历
                //sortNodes
                // todo 删除虚拟节点
                if (stateMap.contains(-100)) {
                    statesList -= stateMap(-100)
                    stateMap.remove(-100)
                }
//                statesList.foreach(s => {
//                    if (s.isInstanceOf[StateGraph]) {
//                        val currentAction = s
//                        determineTransition(currentAction)
//                    }
//                })

                currentStartNode.foreach(s =>{
                    determineTransitionRecursion(stateMap(s))
                    if(!determineTransitionSet.contains(s)){
                        determineTransition(stateMap(s))
                    }
                })
            }
        }

        def determineTransitionRecursion(s: StateGraph): Unit ={
            for(c <- s.childrenList){
                if(!determineTransitionSet.contains(c)){
                    determineTransitionSet.add(c)
                    determineTransitionRecursion(stateMap(c))
                    determineTransition(stateMap(c))
                }
            }
        }

        val onlyOneTransition: ListBuffer[String] = ListBuffer("Send", "CloseIP", "OpenIP", "CloseAllIPs", "OpenAllIPs", "SelectAgents", "VarMan", "CallMacro")
        val onlyTwoTransitions = "IsIPEmpty"
        val multipleTransition: ListBuffer[String] = ListBuffer("Receive", "Action", "Tau", "ModalJoin", "ModalSplit", "Cancel")
        val noneTransition = "End"
        var tipRelatedInformation = ""

//        def tipInformation(source: StateGraph, edgeData: TransitionData): (Boolean, Int) = {
//            val sourceStateType = source.data.stateType
//            var isUpdated = false
//            var num = 0
//            // 只有一条边
//            if (onlyOneTransition.contains(sourceStateType)) {
//                val allTransitions = source.data.directChildrenTransitionsMap ++ source.data.nonDirectChildrenTransitionsMap
//                //allTransitions -= edgeData
//                allTransitions.foreach(edge => {
//                    if (edge.transitionType == "normal") {
//                        num += 1
//                    }
//                })
//                if (num >= 2) {
//                    isUpdated = false
//                } else {
//                    isUpdated = true
//                }
//            }
//            // 只有两条边
//            if (onlyTwoTransitions == sourceStateType) {
//                val allTransitions = source.data.directChildrenTransitionsMap ++ source.data.nonDirectChildrenTransitionsMap
//                //allTransitions -= edgeData
//                allTransitions.foreach(edge => {
//                    if (edge.transitionType == "normal") {
//                        num += 1
//                    }
//                })
//                if (num <= 2) {
//                    isUpdated = true
//                } else {
//                    isUpdated = false
//                }
//            }
//
//            // 多条边
//            if (multipleTransition.contains(sourceStateType)) {
//                isUpdated = true
//                num = 0
//            }
////            (isUpdated, num)
//            //自改
//            (true, num)
//        }

        /*
        选中事件比较复杂，包括state和transition
         */
        def selectedEvent(graph: Graph): Callback = {
            if (graph.isInstanceOf[Arrow]) {
                println(s"label状态: ${graph.asInstanceOf[Arrow]}, name: ${graph.asInstanceOf[Arrow].data.information.relatedSubjectName}, msg: ${graph.asInstanceOf[Arrow].data.information.relatedMessageType}")
                preState = null
                if (preSelected == null) {
                    preSelected = graph
                    val target = preSelected.asInstanceOf[Arrow]
                    recordTransitionInformation(target)
                    target.changeColor
                } else {
                    if (!preSelected.isInstanceOf[Arrow]) { // 前一个是state，现在选中的是Transition
                        restoreStateData(preSelected.asInstanceOf[StateGraph])
                        checkIsStartPoint(preSelected.asInstanceOf[StateGraph])
                        preSelected = graph
                        val target = preSelected.asInstanceOf[Arrow]
                        recordTransitionInformation(target)
                        target.changeColor
                    } else { //前一个是Transition，现在也是Transition
                        if (preSelected.asInstanceOf[Arrow].data.ID == graph.asInstanceOf[Arrow].data.ID) { // 消除Transition选中状态
                            restoreTransitionData(preSelected.asInstanceOf[Arrow])
                            preSelected.asInstanceOf[Arrow].resetColor
                            preSelected = null
                            originalTransitionData = null
                        } else {
                            restoreTransitionData(preSelected.asInstanceOf[Arrow])
                            preSelected.asInstanceOf[Arrow].resetColor
                            preSelected = graph
                            val target = preSelected.asInstanceOf[Arrow]
                            recordTransitionInformation(target)
                            target.changeColor
                        }
                    }
                }
            } else {
                println(s"state状态: ${graph.asInstanceOf[StateGraph]}")
                // 当前选中的是state状态
                if (preSelected != null) {
                    if (preSelected.isInstanceOf[StateGraph]) { // 前一个是state,当前也是一个state
                        if (preSelected.asInstanceOf[StateGraph].data.ID == graph.asInstanceOf[StateGraph].data.ID) {
                            if (preState == null) {
                                checkIsStartPoint(preSelected.asInstanceOf[StateGraph])
                                restoreStateData(preSelected.asInstanceOf[StateGraph])
                                preSelected = null
                                originalStateData = null
                            } else {
                                // loop还没做好
                                val target = graph.asInstanceOf[StateGraph]
                                if (defaultStateList.contains(target)) {
                                    Callback.alert("The current state must be connected by other states.")
                                } else {
                                    //determineTransition(preState, target)
                                    target.parentID += target.data.id
                                    preState = null // 完成连接
                                }
                            }
                        } else { // 前一个state和现在不一样，还原之前的
                            restoreStateData(preSelected.asInstanceOf[StateGraph]) // 前一个state的数据还原
                            checkIsStartPoint(preSelected.asInstanceOf[StateGraph])
                            preSelected = graph
                            // 开始自己的state操作
                            val target = preSelected.asInstanceOf[StateGraph]
                            target.changeBorder
                            recordStateData(target)
                            if (preState == null) {
                            } else {
                                if (defaultStateList.contains(target) && !currentStartNode.contains(target.data.id)) {
                                    defaultStateList -= target
                                    target.deep = preState.deep + 1
                                    target.parentID = preState.data.id
                                    preState.childrenList += target.data.id // 直接子节点列表
                                    // 先创建TransitionData,在根据它绘制箭头
                                    val newTransitionData = TransitionData(getTransitionId(), preState.data.ID, target.data.ID)
                                    // 加入直接子节点
                                    preState.data.addDirectChildrenTransition(newTransitionData)
                                    // 下面的需要重复，当加入非直接子节点时
                                    preState.arrowsToTarget += newTransitionData.id
                                    target.arrowsFromSource += newTransitionData.id
                                } else {
                                    // 移除假的起点，加入主树
                                    if (isolatedNodeList.contains(target.data.id)) {
                                        currentStartNode -= target.data.id
                                        isolatedNodeList -= target.data.id
                                        target.parentID = preState.data.id
                                        target.deep = (preState.deep + 1)
                                        preState.childrenList += target.data.id
                                        searchTree(target)
                                        // todo 连接孤立节点，create a new transition,当前孤立节点变成preState的直接子节点
                                        val newTransitionData = new TransitionData(getTransitionId(), preState.data.ID, target.data.ID)
                                        preState.data.addDirectChildrenTransition(newTransitionData)
                                        preState.arrowsToTarget += newTransitionData.id
                                        target.arrowsFromSource += newTransitionData.id

                                    } else {
                                        // 先做没有孤立节点的情况
                                        // 当前节点已经在之前的节点的子孙列表中出现过
                                        if (preState.descendantList.contains(target.data.id) || currentStartNode.contains(target.data.id)) {
                                            var num = 0
                                            preState.descendantList.foreach(d => {
                                                if (d == target.data.id) {
                                                    num += 1
                                                }
                                            })
                                            val newTransitionData = new TransitionData(getTransitionId(), preState.data.ID, target.data.ID)
                                            newTransitionData.repeatTimes = (num + 1)
                                            preState.data.addNonDirectChildrenTransition(newTransitionData)
                                            preState.arrowsToTarget += newTransitionData.id
                                            target.arrowsFromSource += newTransitionData.id
                                        } else {
                                            val newTransitionData = new TransitionData(getTransitionId(), preState.data.ID, target.data.ID)
                                            newTransitionData.repeatTimes = 1
                                            preState.data.addNonDirectChildrenTransition(newTransitionData)
                                            preState.arrowsToTarget += newTransitionData.id
                                            target.arrowsFromSource += newTransitionData.id
                                        }
                                        preState.descendantList += target.data.id
                                    }
                                }
//                                if (currentStartNode.size > 1) {
//                                    newDummyNode
//                                }
                                treeLayout(findRootNode)
                                preState = null // 完成连接
                            }
                        }
                    } else {
                        // 前一个是Transition，现在是state
                        restoreTransitionData(preSelected.asInstanceOf[Arrow])
                        preSelected.asInstanceOf[Arrow].resetColor
                        preSelected = graph
                        // 开始自己的state操作
                        val target = preSelected.asInstanceOf[StateGraph]
                        target.changeBorder
                        recordStateData(target)
                    }
                } else { // 当前isSelected是null
                    preSelected = graph
                    if (preSelected.isInstanceOf[StateGraph]) {
                        val target = preSelected.asInstanceOf[StateGraph]
                        recordStateData(target)
                        target.changeBorder
                    } else {
                        val target = preSelected.asInstanceOf[Arrow]
                        recordTransitionInformation(target)
                        target.changeColor
                    }
                }
            }
            $.modState(s => s)
        }

        def onDoubleClickEvent(graph: StateGraph): TagMod = {
//            ^.onDoubleClick --> ctl.refresh
            ^.onDoubleClick --> editCallMacro(graph)
        }

        def editCallMacro(graph: StateGraph) = Callback {
            println("macro test.")
            ProcessManager.processMap(processID).subjectMap(subjectID).callMacroMap += (graph.data.id -> ListBuffer[StateGraph]())
            val url = "http://localhost:8080/#subjectsView/processidtest/" + subjectID + "/" + graph.data.id
            dom.window.location.href = url
        }

        def checkIsStartPoint(s: StateGraph): Unit = {
            if (s.data.isStartState) {
                s.startPointBorder
            } else {
                s.resetBorder
            }
        }

        def symbolCenter(t: String): (Int, Int) =
            t match {
                case "Receive" => (20, 20)
                case "Send" => (20, 20)
                case "ModalJoin" => (20, 20)
                case "ModalSplit" => (20, 20)
                case "Action" => (40, 20)
                case "CloseIP" => (40, 20)
                case "Tau" => (40, 20)
                case "OpenIP" => (40, 20)
                case "CloseAllIPs" => (40, 20)
                case "OpenAllIPs" => (40, 20)
                case "SelectAgents" => (40, 20)
                case "CallMacro" => (40, 20)
                case "VarMan" => (40, 20)
                case "Cancel" => (40, 20)
                case "End" => (22, 22)
                case "IsIPEmpty" => (31, 37)
                case _ => (0,0)
            }




        def getSymbolCenter(s: StateGraph): (Int, Int) = {
            if (s.data.stateType == "Action") {
                if (s.data.stateName.length <= 8) {
                    symbolCenter(s.data.stateType)
                } else {
                    ((Math.min(s.data.stateName.length, maxNameLength) * 8) / 2, 20)
                }
            } else {
                symbolCenter(s.data.stateType)
            }
        }


        def determineTransition(currentState: StateGraph) = {
            val startCenter = getSymbolCenter(currentState)
            var arrowSourceX = 0
            var arrowSourceY = 0
            var arrowTargetX = 0
            var arrowTargetY = 0
            var childSource = -2
            var childTarget = -2
            val directChildrenMap: Map[(Int, Int), ListBuffer[TransitionData]] = Map()
            val nonDirectChildrenMap: Map[(Int, Int), ListBuffer[TransitionData]] = Map()
            val directChildren = currentState.data.directChildrenTransitionsMap.values
            val nonDirectChildren = currentState.data.nonDirectChildrenTransitionsMap.values

            for (elem <- directChildren) {
                directChildrenMap += (elem.head.source, elem.head.target) -> elem
            }
            for (elem <- nonDirectChildren) {
                val s = elem.head.source
                val t = elem.head.target
                if (nonDirectChildrenMap.contains((s, t))) {
                    nonDirectChildrenMap((s, t)) ++= elem
                } else {
                    nonDirectChildrenMap += (s, t) -> ListBuffer()
                    nonDirectChildrenMap((s, t)) ++= elem
                }
            }

            // 先完成直接子节点
            if (!currentState.childrenList.isEmpty) {
                val currentChildrenList = currentState.childrenList
                if (currentChildrenList.size == 1) {
                    val end = stateMap(currentChildrenList.head)
                    val endCenter = getSymbolCenter(end)
                    arrowTargetX = end.sx + endCenter._1
                    arrowSourceY = currentState.sy + startCenter._2 * 2
                    arrowSourceX = arrowTargetX
                    if (end.data.stateType == "IsIPEmpty") {
                        arrowTargetY = end.sy - 12
                    } else {
                        arrowTargetY = end.sy
                    }
                    val fineTuningStartX = arrowSourceX - startCenter._1
                    currentState.setCoordinate(fineTuningStartX, currentState.sy)
                    println(s"start: ${currentState.data.id}, x: $arrowSourceX. end: ${end.data.id}, x: $arrowTargetX")
                    childSource = currentState.data.id
                    childTarget = end.data.id
                    recordStateData(end)
                    if (directChildrenMap.contains((childSource, childTarget))) {
                        val edgeData = directChildrenMap((childSource, childTarget))
                        val arrow = new GraphObject.Arrow(processID, subjectID, edgeData.head, arrowSourceX, arrowSourceY, arrowTargetX, arrowTargetY, "1")
                        arrowMap += (arrow.data.ID -> arrow)
                    } else {
                        dom.console.info("这是画唯一直接子节点， TransitionData 和现在 start and end 不一样")
                    }
                } else {
                    val firstChild = stateMap(currentChildrenList.head)
                    val lastChild = stateMap(currentChildrenList.last)
                    val x = (firstChild.sx + getSymbolCenter(firstChild)._1 + lastChild.sx + getSymbolCenter(lastChild)._1) / 2 - getSymbolCenter(currentState)._1
                    currentState.setX(x)
                    currentChildrenList.foreach(chl => {
                        val currentChild = stateMap(chl)
                        val childCenter = getSymbolCenter(currentChild)
                        arrowSourceX = currentChild.sx + childCenter._1
                        if (currentState.data.stateType == "IsIPEmpty") {
                            arrowSourceY = (currentChild.sy - 58)
                        } else {
                            arrowSourceY = (currentChild.sy - 90) //暂定 80箭头固定长度
                        }

                        arrowTargetX = currentChild.sx + childCenter._1
                        arrowTargetY = currentChild.sy
                        childSource = currentState.data.id
                        childTarget = currentChild.data.id
                        if (directChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = directChildrenMap((childSource, childTarget))
                            val arrow = new GraphObject.Arrow(processID, subjectID, edgeData.head, arrowSourceX, arrowSourceY, arrowTargetX, arrowTargetY, "1")
                            arrowMap += (arrow.data.ID -> arrow)
                            recordStateData(currentChild)
                        } else {
                            dom.console.info("这是画唯一直接子节点， TransitionData 和现在 start and end 不一样")
                        }
                        dom.console.info("坐标出现问题了 " + arrowSourceX + "  Y  " + currentChild.sy)
                    })
                }
            }

            if (!currentState.descendantList.isEmpty) {
                val currentDescendantList = currentState.descendantList
                currentDescendantList.foreach(d => {
                    val dsc = stateMap(d)
                    val dscDeep = dsc.deep
                    val dscX = dsc.sx
                    val dscCenter = getSymbolCenter(dsc)
                    childSource = currentState.data.id
                    childTarget = dsc.data.id

                    // 左横向箭头
                    if((dscDeep == currentState.deep) && (currentState.sx > dsc.sx + mu)){
                        val arrowStartX = currentState.sx
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx + + dscCenter._1 * 2
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "9")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("左箭头有问题")
                        }
                    }

                    // 右横向箭头
                    if((dscDeep == currentState.deep) && (currentState.sx + mu < dsc.sx)){
                        val arrowStartX = currentState.sx + startCenter._1 * 2
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "10")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("右箭头有问题")
                        }
                    }
                    //右下左箭头
                    if (((dscDeep - currentState.deep) >= 1) && ((currentState.sx + startCenter._1 - dsc.sx - dscCenter._1 <= mu) && (currentState.sx + startCenter._1 - dsc.sx - dscCenter._1 >= mu * -1))) {
                        val arrowStartX = currentState.sx + startCenter._1 * 2
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx + dscCenter._1 * 2
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            dom.console.info("右下左箭头")
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "4")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("右下左箭头有问题")
                        }
                    }
                    // 左下箭头
                    if (((dscDeep - currentState.deep) >= 1) && (currentState.sx + startCenter._1 - dsc.sx - dscCenter._1 > mu )) {
                        val arrowStartX = currentState.sx + startCenter._1
                        val arrowStartY = currentState.sy + startCenter._2 * 2
                        val arrowEndX = dsc.sx + dscCenter._1 * 2
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "2")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("左下箭头有问题")
                        }
                    }
                    // todo 边界右下箭头
                    if (((dscDeep - currentState.deep) >= 1) && (currentState.sx < dscX)) {
                        val arrowStartX = currentState.sx + (startCenter._1 * 2)
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx + (dscCenter._1 * 2)
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "4")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("右下箭头有问题")
                        }

                    }
                    // todo 边界左上箭头
                    if ((dscDeep - currentState.deep <= -1) && (currentState.sx + startCenter._1  <=  dscX + dscCenter._1 + mu)) {
                        val arrowStartX = currentState.sx
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "5")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("左上箭头有问题")
                        }
                    }
                    // todo 边界右上箭头
                    if ((dscDeep - currentState.deep <= -1) && (currentState.sx + startCenter._1 > dscX + dscCenter._1 + mu)) {
                        val arrowStartX = currentState.sx + (startCenter._1 * 2)
                        val arrowStartY = currentState.sy + startCenter._2
                        val arrowEndX = dsc.sx + (dscCenter._1 * 2)
                        val arrowEndY = dsc.sy + dscCenter._2

                        if (nonDirectChildrenMap.contains((childSource, childTarget))) {
                            val edgeData = nonDirectChildrenMap((childSource, childTarget))
                            for (elem <- edgeData) {
                                val arrow = new GraphObject.Arrow(processID, subjectID, elem, arrowStartX, arrowStartY, arrowEndX, arrowEndY, "3")
                                arrowMap += (arrow.data.ID -> arrow)
                            }
                        } else {
                            dom.console.info("右上箭头有问题")
                        }
                    }
                })
            }
        }

        val circle: ListBuffer[String] = ListBuffer("Send", "Receive", "ModalJoin", "ModalSplit")

        def connectState: Callback = {
            if (preSelected.isInstanceOf[StateGraph]) {
                preState = preSelected.asInstanceOf[StateGraph]
                if (defaultStateList.contains(preState)) {
                    Callback.alert("Please select the previous action first!")
                } else {
                    $.modState(s => s)
                }
            } else Callback.alert("Please select the associated action!")
        }

        def modifyState(s: String, sd: StateData): StateGraph = {
            s match {
                case "Action" => new GraphObject.Action(processID, subjectID, sd)
                case "Receive" => new GraphObject.Receive(processID, subjectID, sd)
                case "Send" => new GraphObject.Send(processID, subjectID, sd)
                case "ModalJoin" => new GraphObject.ModalJoin(processID, subjectID, sd)
                case "ModalSplit" => new GraphObject.ModalSplit(processID, subjectID, sd)
                case "Tau" => new GraphObject.Tau(processID, subjectID, sd)
                case "OpenIP" => new GraphObject.OpenIP(processID, subjectID, sd)
                case "CloseIP" => new GraphObject.CloseIP(processID, subjectID, sd)
                case "IsIPEmpty" => new GraphObject.IsIPEmpty(processID, subjectID, sd)
                case "CloseAllIPs" => new GraphObject.CloseAllIPs(processID, subjectID, sd)
                case "OpenAllIPs" => new GraphObject.OpenAllIPs(processID, subjectID, sd)
                case "SelectAgents" => new GraphObject.SelectAgents(processID, subjectID, sd)
                case "CallMacro" => new GraphObject.CallMacro(processID, subjectID, sd)
                case "VarMan" => new GraphObject.VarMan(processID, subjectID, sd)
                case "Cancel" => new GraphObject.Cancel(processID, subjectID, sd)
                case "End" => new GraphObject.End(processID, subjectID, sd)
            }
        }

        var existStart = false

        def updateEvent(selectedObject: Graph): Callback = {
            if (selectedObject.isInstanceOf[StateGraph]) {
                val target = selectedObject.asInstanceOf[StateGraph]
                if (target.data.ID == originalStateData._1) { // 确保是同一state
                    if (defaultStateList.contains(target)) { //还在编辑区
                        if (target.data.isStartState) { // 是起始点
                            if (!existStart) {
                                // 是否已存在起始点
                                existStart = true
                                target.asInstanceOf[StateGraph].deep = 0
                                target.asInstanceOf[StateGraph].parentID = (-100)
                                currentStartNode += target.data.id // startNode
                                preSelected = updateStateData(target) // 第一个节点改变，坐标不在坐标集合中
                                treeLayout(preSelected.asInstanceOf[StateGraph].data.id)
                                defaultStateList -= target
                            } else {
                                // todo 产生虚拟节点
                                multistart = true
                                val specialStateID = -100
                                val sd = new StateData(specialStateID)
                                val dummyState = new GraphObject.DummyNode(processID, subjectID, sd)
                                virtualStart = dummyState
//                                statesList += dummyState
//                                stateMap += dummyState.data.id -> dummyState
                                currentStartNode += target.data.id
                                currentStartNode.foreach(start => {
                                    dummyState.childrenList += start
                                    stateMap(start).parentID = specialStateID
                                })
                                preSelected = updateStateData(target)
                                preState = null
                                target.deep = 0
                                treeLayout(specialStateID)
                                defaultStateList -= target
                            }
                            $.modState(s => s)
                        } else { // 非起始点并且也存在其他节点
                            if (existStart) {
                                preSelected = updateStateData(target)
                                $.modState(s => s)
                            } else Callback.alert("Please select StartState!")
                        }
                    } else {
                        // State has already positioned. 已经不在编辑区的情况
                        if (target.data.isStartState != originalStateData._3.isStartState) {
                            //起始点可能被修改
                            Callback.alert(" The current state is StartState. After modifying the flow chart can't execute normally!")
                        } else {
                            preSelected = updateStateData(target)
                            $.modState(s => s)
                        }
                    }
                }else{
                    Callback()
                }
            } else { // update transition
                val target = selectedObject.asInstanceOf[Arrow]
                val currentData = target.data
                val sourceState = stateMap(currentData.source)
//                val checkResult = tipInformation(sourceState, target.data)
//                if (checkResult._1) {
                    val newTransitionData: RestoreTransitionData = RestoreTransitionData()
                    newTransitionData.copy(currentData)
                    originalTransitionData = (currentData.ID, true, newTransitionData)
//                } else {
//                    tipRelatedInformation = sourceState.data.stateType + " can only have " + (checkResult._2 - 1).toString + " normal transition."
//                    if (originalTransitionData._1 == target.data.id) {
//                        target.data.copy(originalTransitionData._3)
//                    }
//                    originalTransitionData = (currentData.ID, true, originalTransitionData._3)
//                }

                $.modState(s => s)
            }
        }

        def updateStateData(s: StateGraph): StateGraph = { // 只修改数据
            var createNewState: StateGraph = null
            val oldStateType = originalStateData._3.stateType
            var advancedFlag = false
            if (oldStateType != s.data.stateType) { // modify symbol
                val newStateType = s.data.stateType
                createNewState = modifyState(newStateType, s.data)
                for (i <- statesList.indices) {
                    if (statesList(i).asInstanceOf[StateGraph].data.ID == createNewState.data.ID) {
                        statesList.update(i, createNewState)
                    }
                }
                stateMap(createNewState.data.id) = createNewState
                for (i <- defaultStateList.indices) {
                    if (defaultStateList(i).data.ID == createNewState.data.ID) {
                        defaultStateList.update(i, createNewState)
                        advancedFlag = true // 说明在编辑去
                    }
                }
                if (!advancedFlag) { // 说明不在编辑区，坐标可能改变
                    val oldWidth = s.graphWidth
                    val newWidth = createNewState.graphWidth
                    val oldCoordinateX = originalStateData._3.coordinateX
                    val oldCoordinateY = originalStateData._3.coordinateY
                    val newCoordinateX = oldCoordinateX + oldWidth / 2 - newWidth / 2
                    createNewState.setCoordinate(newCoordinateX, oldCoordinateY)
                }
                // 在编辑区坐标不变
                createNewState.deep = s.deep
                createNewState.parentID = s.parentID
                createNewState.changeBorder
                createNewState.arrowsFromSource = s.arrowsFromSource
                createNewState.arrowsToTarget = s.arrowsToTarget
                createNewState.childrenList = s.childrenList
                createNewState.descendantList = s.descendantList
                createNewState.sign = s.sign
                val oldDirectTransitions = s.data.directChildrenTransitionsMap.values
                val oldNonDirectTransitions = s.data.nonDirectChildrenTransitionsMap.values
                //reset all transition's data
                oldDirectTransitions.foreach(edge => {
                    edge.head.resetTransition
                    arrowMap(edge.head.id).resetColor
                })
                oldNonDirectTransitions.foreach(edge => {
                    edge.head.resetTransition
                    arrowMap(edge.head.id).resetColor
                })
                createNewState.data.directChildrenTransitionsMap = s.data.directChildrenTransitionsMap
                createNewState.data.nonDirectChildrenTransitionsMap = s.data.nonDirectChildrenTransitionsMap

            } else {
                // state's symbol has no change
                createNewState = s
            }
            val newStateData: RestoredData = RestoredData()
            newStateData.copy(createNewState)
            originalStateData = (createNewState.data.ID, true, newStateData)
            createNewState
        }

        //    def saveSubject(): Callback = {
        //      val xhr = new dom.XMLHttpRequest()
        //      val stateListJ = ListBuffer[StateJsonData]()
        //      statesList.foreach(s => {
        //        val f = s.asInstanceOf[StateGraph]
        //        stateListJ += new StateJsonData(f.data.ID, f.data.stateType, f.data.isStartState,
        //          f.data.description, f.data.transitionsList, f.sx, f.sy)
        //      })
        //
        //      val stateJson= new SubjectContent(subjectID, stateListJ).asJson
        //      xhr.open("Post", "http://localhost:8081/store", true)
        //      xhr.onload = { (e: dom.Event) =>
        //        if (xhr.status == 200) {
        //          dom.console.info("sucess")
        //        }
        //      }
        //      dom.console.info(stateJson.noSpaces)
        //      xhr.send(stateJson.noSpaces)
        //      Callback()
        //    }

        def treeLayout(): Callback = {
            statesList.foreach(_.rpst = false)
            arrowMap.valuesIterator.foreach(_.rpst = false)
            autoLayout = false
            treeLayout(findRootNode)
            $.modState(s => s)
        }


        def rpstLayout(): Callback = {
            //      val xhr = new dom.XMLHttpRequest()
            //      xhr.onreadystatechange = {(e: dom.Event) =>
            //        if(xhr.readyState == 200){
            //          val r = JSON.parse(xhr.responseText)
            //          dom.console.info(r)
            //        }
            //      }
            //      xhr.open("Get", "http://localhost:8081/load", true)
            //      xhr.send()
            autoLayout = true
            val g = new RGraph
            g.createMateInfo()
            val s2v: Map[StateGraph, Vertex] = Map()
            val v2s: Map[Vertex, StateGraph] = Map()
            for (s <- statesList) {
                var v: Vertex = null
                if (s2v.contains(s)) {
                    v = s2v(s)
                } else {
                    v = new Vertex(s.data.stateName)
                    g.addVertex(v)
                    s2v += (s -> v)
                    v2s += (v -> s)
                }

                for (id <- s.arrowsToTarget) {
                    val cid = arrowMap(id).data.target
                    val child = ProcessManager.processMap(processID).subjectMap(subjectID).stateMap(cid)
                    var w: Vertex = null
                    if (s2v.contains(child)) {
                        w = s2v(child)
                    } else {
                        w = new Vertex(child.data.stateName)
                        g.addVertex(w)
                        s2v += (child -> w)
                        v2s += (w -> child)
                    }
                    g.addEdge(v, w)
                }
            }
            g.splitFreeVertices()
            statesList.foreach(_.rpst = true)
            val rpst = new RPST(g, s2v, v2s, arrowMap)
            rpst.start()
            Layout.init(rpst)
            Layout.PreProcess(statesList, arrowMap.valuesIterator.toList)
            Layout.start()
            $.modState(s => s)
        }

        def recordStateData(s: StateGraph) = {
            val oldStateData: RestoredData = RestoredData()
            oldStateData.copy(s)
            originalStateData = (oldStateData.id, false, oldStateData)
        }

        def restoreStateData(s: StateGraph) = {

            if (s.data.ID == originalStateData._1) {
                if (!originalStateData._2) { // modify state, but not confirmed
                    s.data.copy(originalStateData._3) // 已还原
                    if (!autoLayout) {
                        val x = originalStateData._3.coordinateX
                        val y = originalStateData._3.coordinateY
                        s.setCoordinate(x, y)
                    }
                    if (s.data.stateType == "Action") {
                        val length = originalStateData._3.name.length
                        if (length <= 8) {
                            s.graphWidth = 80
                        } else {
                            s.graphWidth = s.asInstanceOf[GraphObject.Action].changeWidth(originalStateData._3.name)
                        }
                    }
                    originalStateData = null
                } else {
                    dom.console.info("It is not necessary to restore stateData.")
                }
            } else {
                dom.console.info("restoreStateData occurs error")
            }
        }

        def getPriorityAction(s: Graph)(e: ReactEventFromInput): Callback = {
            val newPriority = e.target.value
            if (s.isInstanceOf[StateGraph]) {
                originalStateData = (originalStateData._1, false, originalStateData._3)
                s.asInstanceOf[StateGraph].data.priority = newPriority.toInt
            }
            $.modState(s => s)
        }


        // Transition
        var allSubjectsSet: Set[String] = Set() // subjectList
        var relatedSubject = ""
        var originalTransitionData: (Int, Boolean, RestoreTransitionData) = null

        def recordTransitionInformation(s: Arrow) = {
            val trID = s.data.ID
            val trData = s.data
            val oldTransitionData: RestoreTransitionData = RestoreTransitionData()
            oldTransitionData.copy(trData)
            originalTransitionData = (trID, false, oldTransitionData)
        }

        def restoreTransitionData(s: Arrow) = {
            if (s.data.ID == originalTransitionData._1) {
                if (!originalTransitionData._2) { // modify transition, but not confirmed
                    val oldData = originalTransitionData._3
                    s.data.copy(oldData)
                } else {
                    dom.console.info("modify and confirm transition.")
                }
            } else {
                dom.console.info("when restoring Transition Data, different state")
            }
        }

        val allMessageTypeSet: Set[String] = Set()

        def collectAllMessageTypes: Unit = {
            ProcessManager.processMap(processID).subjectList.foreach(sub => {
                sub.stateList.foreach(state => {
                    if (state.isInstanceOf[GraphObject.Send] || state.isInstanceOf[GraphObject.Receive]) {
                        val communicationState = state.asInstanceOf[StateGraph]
                        communicationState.data.directChildrenTransitionsMap.foreach(edge => {
                            edge._2.foreach(f => allMessageTypeSet += f.information.relatedMessageType)
//                            val exchangedMessage = edge.information.relatedMessageType
//                            if (exchangedMessage.nonEmpty) {
//                                allMessageTypeSet += exchangedMessage
//                            }
                        })
                        communicationState.data.nonDirectChildrenTransitionsMap.foreach(edge => {
                            edge._2.foreach(f => allMessageTypeSet += f.information.relatedMessageType)
//                            val exchangedMessage = edge.information.relatedMessageType
//                            if (exchangedMessage.nonEmpty) {
//                                allMessageTypeSet += exchangedMessage
//                            }
                        })
//                        communicationState.arrowsToTarget.foreach(edge => {
//                            val exchangedMessage = arrowMap(edge).data.information.relatedMessageType
//                            if (exchangedMessage != "") {
//                                allMessageTypeSet += exchangedMessage
//                            }
//                        })
                    }
                })
            })
        }

        def createNewMessageType(s: Graph)(e: ReactEventFromInput): Callback = {
            val msg = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val currentTransition = s.asInstanceOf[Arrow]
                val sourceStateID = currentTransition.data.source
                val sourceState = stateMap(sourceStateID)
                if (sourceState.data.stateType == "Send" || sourceState.data.stateType == "Receive") {
                    currentTransition.data.information.addRelatedMessageType(msg)
                    currentTransition.data.information.setActionType(sourceState.data.stateType)
                }
            }
            $.modState(s => s)
        }

        def getMessageTypeOfTransition(s: Graph)(e: ReactEventFromInput): Callback = {
            val newMessageType = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val currentTransition = s.asInstanceOf[Arrow]
                val sourceStateID = currentTransition.data.source
                val sourceState = stateMap(sourceStateID)
                if (availableState.contains(sourceState.data.stateType)) {
                    originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                    if (sourceState.data.stateType == "Send" || sourceState.data.stateType == "Receive") {
                        currentTransition.data.information.addRelatedMessageType(newMessageType)
                        currentTransition.data.information.setActionType(sourceState.data.stateType)
                    } else {
                        currentTransition.data.inputPoolOperationObject.setRelatedMessageType(newMessageType)
                    }
                }
            }
            $.modState(s => s)
        }

        def getRelatedSubjectNameOfTransition(s: Graph)(e: ReactEventFromInput): Callback = {
            val newSubjectName = e.target.value
            if (s.isInstanceOf[Arrow]) {
                val currentTransition = s.asInstanceOf[Arrow]
                val sourceStateID = currentTransition.data.source
                val sourceState = stateMap(sourceStateID)
                // 只有之前的state是Send or Receive 时才有用
                if (availableState.contains(sourceState.data.stateType)) {
                    originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                    if (sourceState.data.stateType == "Send" || sourceState.data.stateType == "Receive") {
                        currentTransition.data.information.addRelatedSubjectName(newSubjectName)
                        currentTransition.data.information.setActionType(sourceState.data.stateType)
                    } else {
                        currentTransition.data.inputPoolOperationObject.setRelatedSubjectName(newSubjectName)
                    }
                }

            }
            $.modState(s => s)
        }

        val availableState: ListBuffer[String] = ListBuffer("Send", "Receive", "CloseIP", "OpenIP", "CloseAllIPs", "OpenAllIPs")

        def checkSourceState(edge: Arrow, require: Int): String = {
            var msg = ""
            var sub = ""
            val sourceState = stateMap(edge.data.source)
            val sourceType = sourceState.data.stateType
            if (availableState.contains(sourceType)) {
                if (sourceType == "Send" || sourceType == "Receive") {
                    msg = edge.data.information.relatedMessageType
                    sub = edge.data.information.relatedSubjectName
                } else {
                    msg = edge.data.inputPoolOperationObject.messageType
                    sub = edge.data.inputPoolOperationObject.subjectName
                }
                if (require == 1) {
                    msg
                } else {
                    sub
                }
            } else {
                ""
            }
        }

        def checkCommunicationState(edge: Arrow): Boolean = {
            var isCommunication = false
            val sourceState = stateMap(edge.data.source)
            if (sourceState.data.stateType == "Send" || sourceState.data.stateType == "Receive") {
                isCommunication = true
            } else {
                isCommunication = false
            }
            isCommunication
        }

        def checkVarMan(edge: Arrow): Boolean = {
            var isVarMan = false
            val sourceState = stateMap(edge.data.source)
            if (sourceState.isInstanceOf[GraphObject.VarMan]) {
                isVarMan = true
            } else {
                isVarMan = false
            }
            isVarMan
        }

        def checkMessageState(edge: Arrow): Boolean = {
            var isMessage = false
            val sourceState = stateMap(edge.data.source)
            if (availableState.contains(sourceState.data.stateType)) {
                isMessage = true
            } else {
                isMessage = false
            }
            isMessage
        }

        def multiTransionsList: TagMod = {
            if((preSelected != null) && (preSelected.isInstanceOf[Arrow])){
                val td = preSelected.asInstanceOf[Arrow].data
                val sd = stateMap(td.source).data
                if(sd.directChildrenTransitionsMap.contains(td.target)){
                    sd.directChildrenTransitionsMap(td.target).toTagMod{mt =>
                        <.option(sd.directChildrenTransitionsMap(td.target).indexOf(mt))
                    }
                }else if(sd.nonDirectChildrenTransitionsMap.contains(td.target)){
                    sd.nonDirectChildrenTransitionsMap(td.target).toTagMod{mt =>
                        <.option(sd.nonDirectChildrenTransitionsMap(td.target).indexOf(mt))
                    }
                }else{
                    <.option()
                }
            }else{
                <.option()
            }
        }

        def getSelectTransition(s: Graph)(e: ReactEventFromInput): Callback = {
            if((s != null) && (s.isInstanceOf[Arrow])){
                val td = s.asInstanceOf[Arrow].data
                val sd = stateMap(td.source).data
                if(sd.directChildrenTransitionsMap.contains(td.target)){
                    val selectedTransitionData: TransitionData = sd.directChildrenTransitionsMap(td.target)(e.target.value.toInt)
                    s.asInstanceOf[Arrow].changeData(selectedTransitionData)
                }else if(sd.nonDirectChildrenTransitionsMap.contains(td.target)){
                    val selectedTransitionData: TransitionData = sd.nonDirectChildrenTransitionsMap(td.target)(e.target.value.toInt)
                    s.asInstanceOf[Arrow].changeData(selectedTransitionData)
                }else{
                    //
                }
            }else{
                //
            }
            $.modState(s => s)
        }

        def transitionMsgT = {
            allMessageTypeSet.clear()
            collectAllMessageTypes
            allMessageTypeSet.toTagMod { mt =>
                <.option(
                    mt,
                    ^.selected := false
                )
            }
        }

        def transitionRelatedS = {
            allSubjectsSet.clear()
            ProcessManager.processMap(processID).subjectList.foreach(s => allSubjectsSet += s.name)
            allSubjectsSet.toTagMod { s =>
                <.option(
                    s,
                    if ((preSelected != null) && (preSelected.isInstanceOf[Arrow])) {
                        val msg = checkSourceState(preSelected.asInstanceOf[Arrow], 2)
                        if (msg == s) {
                            ^.selected := true
                        } else {
                            ^.selected := false
                        }
                    } else ^.selected := false
                )
            }
        }

        def getPriorityTransition(t: Graph)(e: ReactEventFromInput): Callback = {
            val priority = e.target.value
            if (t.isInstanceOf[Arrow]) {
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                t.asInstanceOf[Arrow].data.setPriority(priority.toInt)
            }
            $.modState(s => s)
        }

        def getTransitionType(t: Graph)(e: ReactEventFromInput): Callback = {
            val newTransitionType = e.target.value
            if (t.isInstanceOf[Arrow]) {
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                t.asInstanceOf[Arrow].data.setTransitionType(newTransitionType)
            }
            $.modState(s => s)
        }

        def getTransitionTimeout(t: Graph)(e: ReactEventFromInput): Callback = {
            val newTimeout = e.target.value
            if (t.isInstanceOf[Arrow]) {
                originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                t.asInstanceOf[Arrow].data.timeout = newTimeout.toInt
                t.asInstanceOf[Arrow].data.label = newTimeout + "s"
            }
            $.modState(s => s)
        }

        def getActivatedStateID(t: Graph)(e: ReactEventFromInput): Callback = {
            val activatedStateID = e.target.value
            if (t.isInstanceOf[Arrow]) {
                val source = stateMap(t.asInstanceOf[Arrow].data.source)
                if (source.isInstanceOf[GraphObject.Cancel]) {
                    originalTransitionData = (originalTransitionData._1, false, originalTransitionData._3)
                    t.asInstanceOf[Arrow].data.activatedStateID = activatedStateID.toInt
                }
            }
            $.modState(s => s)
        }

    }


    val Main = ScalaComponent.builder[Unit]("InternalBehavior")
            .initialState(State(ListBuffer()))
            .renderBackend[Backend]
            .build

    val component = ScalaComponent.builder[BehaviorPage]("StatePage").render { p =>
        processID = p.props.processid
        subjectID = p.props.subjectid
        currentCallMacroID = p.props.callMarco
        Main()
    }.build
}
