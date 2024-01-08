package de.tkip.sbpm.frontend.pages

import de.tkip.sbpm.frontend.AppRouter.{Home, SubjectsVP, config}
import de.tkip.sbpm.frontend.Data._
import de.tkip.sbpm.frontend.graph._
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import org.scalajs.dom

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Set}

object SubjectsV {

    val (router, lgc) = Router.componentAndLogic(BaseUrl.fromWindowOrigin_/, config)
    val ctl = lgc.ctl

    object Style extends StyleSheet.Inline {

        import dsl._

        val content = style(
            position.relative,
            display.flex,
            width :=! "100%",
            minHeight(600.px),
            //      backgroundColor :=! "#FA8072",
            borderTop :=! "1px solid rgb(223, 220, 200)",
            fontFamily :=! "Roboto, sans-serif"
        )

        val rightNav = style(
            display.flex,
            //position.absolute,
            width :=! "16%",
            //height :=! "100%",
            borderLeft :=! "1px solid rgb(223, 220, 200)",
            backgroundColor :=! c"#C0C0C0")

        val buttonStyle = style(
            &.active(backgroundColor :=! c"#6495ED"),
            border :=! "1px solid transparent",
            fontSize :=! 11.px
        )


        val subjectData = style(
            //display.flex,
            position.absolute,
            minWidth :=! 100.px,
            maxWidth :=! 100.px,
            minHeight :=! 150.px,
            maxHeight :=! 150.px,
            borderRadius :=! 10.px,
            border :=! "solid 4px #ADD8E6",
            backgroundColor :=! "#B1DDF0",
            cursor.pointer
        )

        val textAreaStyle = style(
            resize.none
        )
        val textStyle = style(userSelect := "none")
    }

    var processID: String = ""

    case class State(subjects: ListBuffer[SubjectData])

    var isSelected: SubjectData = null
    var subjectReceiveState: Map[Int, Set[Int]] = Map() // (subjectID, from relatedSubjectID)
    var subjectSendState: Map[Int, Set[Int]] = Map() // (subjectID, to relatedSubjectID)
    // (sourceSubjectId, (targetSubjectId, messageList))
    var subjectReceiveMessages: Map[Int, Map[Int, Set[String]]] = Map()
    var subjectSendMessages: Map[Int, Map[Int, Set[String]]] = Map()

    var transitionOfSendingMsg: Map[Int, Map[Int, Set[Int]]] = Map()
    var transitionOfReceiveMsg: Map[Int, Map[Int, Set[Int]]] = Map()

    var subjectPositionReversedMap: Map[Int, Int] = Map() // subject的位置和id交换
    var arrowPair: ListBuffer[VdomElement] = ListBuffer()
    val communicationList: ListBuffer[InteractionData] = ListBuffer()
    val bottomArrowMap: Map[Int, Map[Int, Int]] = Map()
    val finalBottomArrowMap: Map[Int, Map[Int, Int]] = Map()
    val upArrowMap: Map[Int, Map[Int, Int]] = Map()
    val finalUpArrowMap: Map[Int, Map[Int, Int]] = Map()
    var bottomArrowCounter: Int = 0
    var bottomArrowFlag = true
    var bottomArrowOk = false
    var bottomArrowIsAdded = true
    var upArrowCounter: Int = 0
    var upArrowFlag = true
    var upArrowIsAdded = true
    var upArrowOk = false
    var subjectPosition: Map[Int, Int] = Map() // (subjectId, subjectPosition)
    var subIDMax = 0


    case class InteractionData() {
        var direction: String = _
        var start: (Int, Int) = _
        var end: (Int, Int) = _
        var startID: Int = _
        var endID: Int = _
        var layer: Int = _
        var msg: Set[String] = _
    }

    class Backend(val $: BackendScope[Unit, State]) {
        if(!ProcessManager.processMap.contains(processID)){
//            ctl.set(Home).runNow()
            val url = "http://localhost:8080/"
            dom.window.location.href = url
        }
        val subjectsList: ListBuffer[SubjectData] = ProcessManager.processMap(processID).subjectList

        def render(s: State) = {
            <.div(
                Style.content,
                <.div(
                    ^.width := "84%",
                    ^.position.relative,
                    //^.backgroundColor := "gold",
                    ^.overflowX.scroll,
                    ^.position.relative,
                    ^.display.`inline-flex`,
                    s.subjects.toTagMod {
                        item => {
                            createContent(item, s)
                        }
                    },
                    communicationRelationShip.toVdomArray,
                    labelMap.toTagMod { item => {
                        item._2.content
                    }
                    }
                ),
                createRightNav(s)
            )
        }

        def createContent(s: SubjectData, state: State) = {
            if (!s.isMultiSubject) {
                createSingleSubjectSymbol(s)
            } else {
                createMultiSubjectSymbol(s)
            }
        }

        def createRightNav(state: State) = {
            <.div(
                Style.rightNav,
                ^.overflowY.scroll,
                <.ul(
                    ^.position.relative,
                    //^.backgroundColor := "pink",
                    ^.listStyle := "none",
                    <.p(
                        " Subject Settings",
                        ^.textAlign.left,
                        ^.overflow.hidden,
                        ^.marginLeft := 0.px,
                        ^.paddingTop := 20.px,
                        ^.fontSize := 20.px
                    ),
                    <.button(
                        "Add ",
                        Style.buttonStyle,
                        ^.borderRadius := "13em/3em",
                        ^.width := "22%",
                        ^.height := 30.px,
                        ^.overflow.hidden,
                        ^.onClick --> addSubject
                    ),
                    <.button(
                        "Delete",
                        Style.buttonStyle,
                        ^.borderRadius := "13em/3em",
                        ^.marginLeft := 10.px,
                        ^.width := "22%",
                        ^.height := 30.px,
                        ^.overflow.hidden,
                        ^.onClick --> deleteSubject
                    ),
                    <.button(
                        "Refresh",
                        Style.buttonStyle,
                        ^.borderRadius := "13em/3em",
                        ^.marginLeft := 10.px,
                        ^.width := "22%",
                        ^.height := 30.px,
                        ^.overflow.hidden,
                        ^.onClick --> refresh
                    ),
//                    {
//                        LoadButtonComponents.pid = processID
//                        LoadButtonComponents.b = $
//                        LoadButtonComponents.LoadButton()
//                    },

//                    <.button(
//                        "Save",
//                        Style.buttonStyle,
//                        ^.borderRadius := "13em/3em",
//                        ^.overflow.hidden,
//                        ^.marginLeft := 10.px,
//                        ^.width := "22%",
//                        ^.height := 30.px
//                    ),
                    <.br,
                    <.li(
                        ^.marginTop := 20.px,
                        "Subject ID: ",
                        <.br,
                        <.br,
                        <.label(
                            if (isSelected != null)
                                isSelected.id
                            else "",
                            ^.textAlign.center,
                            ^.display.block,
                            ^.width := "86%",
                            ^.height := 30.px
                        )
                    ),
                    <.li(
                        startStateCheckbox(),
                        <.label(
                            ^.marginLeft := 20.px,
                            "start subject"
                        )
                    ),
                    <.li(
                        ^.marginTop := 20.px,
                        "Subject Name: ",
                        <.br,
                        <.br,
                        <.input.text(
                            ^.width := "86%",
                            if (isSelected != null)
                                ^.value := isSelected.name
                            else ^.value := "",
                            ^.onChange ==> getSubjectName(isSelected)
                        )
                    ),
                    <.li(
                        ^.marginTop := 20.px,
                        "Type:",
                        <.br,
                        <.br,
                        <.select(
                            ^.width := "86%",
                            <.option(
                                "default",
                                (^.selected := true).when((isSelected != null) && (isSelected.subjectType == "default"))
                            ),
                            <.option(
                                "interface",
                                (^.selected := true).when((isSelected != null) && (isSelected.subjectType == "interface"))
                            ),
                            ^.onChange ==> setSubjectType(isSelected)
                        )
                    ),
                    <.li(
                        ^.marginTop := 20.px,
                        "InputPool: ",
                        <.br,
                        <.br,
                        <.input.number(
                            ^.min := 0,
                            ^.width := "86%",
                            if (isSelected != null)
                                ^.value := isSelected.inputPool
                            else ^.value := 0,
                            ^.onChange ==> changeInputPool(isSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "number of subject:  ",
                        <.input.number(
                            ^.width := 35.px,
                            ^.min := 1,
                            if (isSelected != null)
                                ^.value := isSelected.num
                            else ^.value := 1,
                            ^.onChange ==> changeNum(isSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.li(
                        "Description: ",
                        <.textarea(
                            Style.textAreaStyle,
                            ^.marginTop := 20.px,
                            ^.width := "86%",
                            ^.height := 80.px,
                            if (isSelected != null) {
                                ^.value := isSelected.description
                            } else {
                                ^.value := ""
                            },
                            ^.onChange ==> setDescription(isSelected)
                        )
                    ),
                    <.br,
                    <.br,
                    <.button(
                        "Confirm",
                        Style.buttonStyle,
                        ^.borderRadius := "50%",
                        ^.width := "30%",
                        ^.height := 30.px,
                        ^.onClick --> confirmContent(isSelected)
                    )
                )
            )
        }

        def getSubjectName(s: SubjectData)(e: ReactEventFromInput): Callback = {
            if (originalSubjectData != null && (s.id == originalSubjectData._1)) { // 确保当前的更新数据一定是选中的Subject
                originalSubjectData = (originalSubjectData._1, false, originalSubjectData._3)
                s.setSubjectName(e.target.value)
                $.modState(s => s)
            } else {
                Callback()
            }
        }

//        var id: Int = 0 //subject id should be got from backend.

        def startStateCheckbox() = {
            if ((isSelected != null)) {
                <.input.checkbox(
                    ^.checked := isSelected.startSubject,
                    ^.onChange --> setStartState
                )
            } else {
                <.input.checkbox(
                    ^.checked := false
                )
            }
        }

        def setStartState: Callback = {
            if (isSelected.startSubject) {
                isSelected.setStartSubject(false)
            } else {
                isSelected.setStartSubject(true)
            }
            $.modState(s => s)
        }

        def checkSubjectId(sn: String): Int = {
            var currentID = -1
            subjectsList.foreach(s => if (s.name == sn) currentID = s.id)
            if (currentID == -1) {
                dom.console.info("the current subject doesn't exist.")
            }
            currentID
        }

        def getNewId(): Int = {
            if (subjectsList.isEmpty){
                return 1
            }else{
                return subjectsList.sortBy(f => f.id).last.id + 1
            }
        }

        def addSubject(): Callback = {
            val newSubjectData = new SubjectData(getNewId, s"Subject$getNewId", false, "default", 0, 1, "")
            ProcessManager.processMap(processID).addSubject(newSubjectData) // newSubject has been added.
            if (subjectsList.size == 1) {
                newSubjectData.subjectX = 150
                newSubjectData.subjectY = 200
            } else {
                val gap = 150
                val subjectWidth = 100
                val marginTop = 200
                val p = subjectsList.size * gap + (subjectsList.size - 1) * subjectWidth
                newSubjectData.subjectX = p
                newSubjectData.subjectY = marginTop
            }

            $.modState(s => State(subjectsList))
        }

        def createSingleSubjectSymbol(s: SubjectData) = {
            <.div(
                Style.subjectData,
                ^.border := isInterfaceSubject(s),
                ^.marginLeft := s.subjectX.px,
                ^.marginTop := s.subjectY.px,
                startSymbol(s),
                <.div(
                    ^.marginLeft := 0.px,
                    ^.width := 100.px,
                    ^.height := 95.px,
                    <.div(
                        ^.marginTop := 10.px,
                        ^.width := 100.px,
                        ^.height := 45.px,
                        ^.fontSize := 12.px,
                        ^.fontWeight.bold,
                        s.name,
                        ^.textAlign.center,
                        ^.overflow.hidden,
                        ^.backgroundColor := "#FFE6CC"
                    ),
                    <.div(
                        ^.width := 85.px,
                        ^.height := 50.px,
                        ^.paddingLeft := 10.px,
                        ^.textAlign.left,
                        ^.fontSize := 12.px,
                        "InputPool: " + s.inputPool,
                        <.br,
                        <.br,
                        "[  " + s.subjectType + "  ]"
                    ),
                    Style.textStyle,
                    ^.wordWrap.`break-word`,
                    ^.wordBreak.`break-all`
                ),
                (^.border := "solid 6px #10739E").when((isSelected != null) && (isSelected.id == s.id)),
                ^.onClick --> selected(s),
                ^.onDoubleClick --> jumpLink(s)
            )
        }

        def createMultiSubjectSymbol(s: SubjectData) = {
            <.div(
                ^.position.relative,
                ^.marginLeft := s.subjectX.px,
                ^.marginTop := (s.subjectY - 10).px,
                ^.minWidth := 100.px,
                ^.maxWidth := 100.px,
                ^.minHeight := 150.px,
                ^.maxHeight := 150.px,
                ^.borderRadius := 10.px,
                ^.border := isInterfaceSubject(s),
                (^.border := "solid 6px #10739E").when((isSelected != null) && (isSelected.id == s.id)),
                ^.backgroundColor := "#B1DDF0",
                <.div(
                    ^.position.absolute,
                    ^.marginTop := (5).px,
                    ^.marginLeft := (5).px,
                    ^.minWidth := 100.px,
                    ^.maxWidth := 100.px,
                    ^.minHeight := 150.px,
                    ^.maxHeight := 150.px,
                    ^.borderRadius := 10.px,
                    ^.border := isInterfaceSubject(s),
                    (^.border := "solid 6px #10739E").when((isSelected != null) && (isSelected.id == s.id)),
                    ^.backgroundColor := "#B1DDF0",
                    <.div(
                        ^.position.absolute,
                        ^.marginTop := (5).px,
                        ^.marginLeft := (5).px,
                        ^.minWidth := 100.px,
                        ^.maxWidth := 100.px,
                        ^.minHeight := 150.px,
                        ^.maxHeight := 150.px,
                        ^.borderRadius := 10.px,
                        ^.backgroundColor := "#B1DDF0",
                        ^.cursor.pointer,
                        ^.border := isInterfaceSubject(s),
                        (^.border := "solid 6px #10739E").when((isSelected != null) && (isSelected.id == s.id)),
                        <.div(
                            startSymbol(s),
                            <.div(
                                ^.marginLeft := 0.px,
                                ^.width := 100.px,
                                ^.height := 95.px,
                                <.div(
                                    ^.marginTop := 10.px,
                                    ^.width := 100.px,
                                    ^.height := 45.px,
                                    ^.fontSize := 12.px,
                                    ^.fontWeight.bold,
                                    s.name,
                                    ^.textAlign.center,
                                    ^.overflow.hidden,
                                    ^.backgroundColor := "#FFE6CC"
                                ),
                                <.div(
                                    ^.width := 85.px,
                                    ^.height := 50.px,
                                    ^.paddingLeft := 10.px,
                                    ^.textAlign.left,
                                    ^.fontSize := 12.px,
                                    "InputPool: " + s.inputPool,
                                    <.br,
                                    "[  num:  " + s.num + "  ]",
                                    <.br,
                                    "[  " + s.subjectType + "  ]"
                                ),
                                Style.textStyle,
                                ^.wordWrap.`break-word`,
                                ^.wordBreak.`break-all`
                            )
                        )
                    )
                ),
                (^.border := "solid 6px #10739E").when((isSelected != null) && (isSelected.id == s.id)),
                ^.onClick --> selected(s),
                ^.onDoubleClick --> jumpLink(s)
            )
        }

        def changeInputPool(s: SubjectData)(e: ReactEventFromInput): Callback = {
            if (originalSubjectData != null && (s.id == originalSubjectData._1)) { // 确保当前的更新数据一定是选中的Subject
                originalSubjectData = (originalSubjectData._1, false, originalSubjectData._3)
                s.setInputPool(e.target.value.toInt)
                $.modState(s => s)
            } else {
                Callback()
            }
        }

        def changeNum(s: SubjectData)(e: ReactEventFromInput): Callback = {
            if (originalSubjectData != null && (s.id == originalSubjectData._1)) { // 确保当前的更新数据一定是选中的Subject
                originalSubjectData = (originalSubjectData._1, false, originalSubjectData._3)
                if (e.target.value.toInt > 1) {
                    s.setNum(e.target.value.toInt)
                    s.isMultiSubject = true
                } else {
                    s.setNum(e.target.value.toInt)
                    s.isMultiSubject = false
                }
                $.modState(s => s)
            } else {
                Callback()
            }
        }

        def setDescription(s: SubjectData)(e: ReactEventFromInput): Callback = {
            if (originalSubjectData != null && (s.id == originalSubjectData._1)) { // 确保当前的更新数据一定是选中的Subject
                originalSubjectData = (originalSubjectData._1, false, originalSubjectData._3)
                s.setDescription(e.target.value)
                $.modState(s => s)
            } else {
                Callback()
            }
        }

        def setSubjectType(s: SubjectData)(e: ReactEventFromInput): Callback = {
            if (originalSubjectData != null && (s.id == originalSubjectData._1)) { // 确保当前的更新数据一定是选中的Subject
                originalSubjectData = (originalSubjectData._1, false, originalSubjectData._3)
                s.setSubjectType(e.target.value)
                $.modState(s => s)
            } else {
                Callback()
            }
        }

        def isInterfaceSubject(s: SubjectData): String = {
            if (s.startSubject) {
                if (s.subjectType == "interface") {
                    "dashed 6px #60A917"
                } else {
                    "solid 6px #60A917"
                }
            } else {
                if (s.subjectType == "interface") {
                    "dashed 2px #0050EF"
                } else {
                    "solid 1px black"
                }
            }
        }

        def startSymbol(s: SubjectData) = {
            if (s.startSubject) {
                <.div(
                    ^.position.relative,
                    ^.marginLeft := 10.px,
                    ^.marginTop := 10.px,
                    ^.width := 20.px,
                    ^.height := 20.px,
                    ^.borderRadius := 20.px,
                    ^.border := "solid 3px #2D7600",
                    <.div(
                        ^.position.absolute,
                        ^.top := 0.px,
                        ^.bottom := 0.px,
                        ^.left := "35%",
                        ^.width := 0.px,
                        ^.height := 0.px,
                        ^.borderTop := "9px solid transparent",
                        ^.borderBottom := "9px solid transparent",
                        ^.borderRight := "9px solid transparent",
                        ^.borderLeft := "9px solid #000"
                    )
                )
            } else {
                <.div(
                    ^.position.relative,
                    ^.marginLeft := 10.px,
                    ^.marginTop := 10.px,
                    ^.width := 20.px,
                    ^.height := 20.px,
                    ^.borderRadius := 20.px,
                    ^.border := "solid 3px transparent",
                )
            }

        }

        def deleteSubject(): Callback = {
            //modify coordinate of each subject
            var keepX = 0
            var keepY = 0
            var newX = 0
            var newY = 0
            var flag = true
            var position = 0
            for (i <- 0 until subjectsList.size if flag) {
                if (subjectsList(i).id == isSelected.id) {
                    keepX = isSelected.subjectX
                    keepY = isSelected.subjectY
                    flag = false
                    position = i
                }
            }
            subjectsList -= isSelected
            while (position != subjectsList.size) {
                newX = subjectsList(position).subjectX
                newY = subjectsList(position).subjectY
                subjectsList(position).subjectX = keepX
                subjectsList(position).subjectY = keepY
                keepX = newX
                keepY = newY
                position += 1
            }
            // remove related data from the other subject
            transitionOfSendingMsg.foreach(interaction => {
                val currentSub = ProcessManager.processMap(processID).subjectMap(interaction._1.toString)
                interaction._2.foreach(relatedInformation => {
                    if (relatedInformation._1 == isSelected.id) {
                        relatedInformation._2.foreach(tr => {
                            currentSub.arrowMap(tr).data.information.addRelatedSubjectName("")
                        })
                    }
                })
            })
            transitionOfReceiveMsg.foreach(interaction => {
                val currentSub = ProcessManager.processMap(processID).subjectMap(interaction._1.toString)
                interaction._2.foreach(relatedInformation => {
                    if (relatedInformation._1 == isSelected.id) {
                        relatedInformation._2.foreach(tr => {
                            currentSub.arrowMap(tr).data.information.addRelatedSubjectName("")
                        })
                    }
                })
            })
            labelMap.clear()
            isSelected = null
            $.modState(s => s)
        }

        def refresh(): Callback = {
            subjectsList.clear()
            subjectsList ++= ProcessManager.processMap(processID).subjectList
            subIDMax = subjectsList.sortBy(f => f.id).last.id
//            sl.head.name += "test"
            val subState = State(subjectsList)
            $.setState(subState)
        }

        def selected(s: SubjectData): Callback = {
            if (isSelected != null) {
                if (isSelected.id == s.id) {
                    isSelected = null // cancel the selected state
                    originalSubjectData = null
                } else {
                    restoreOriginalData(isSelected)
                    isSelected = s
                    keepOriginalData(isSelected)
                }
            } else {
                isSelected = s
                keepOriginalData(isSelected)
            }
            $.modState(s => s)
        }

        var originalSubjectData: (Int, Boolean, RecordSubjectData) = null

        def keepOriginalData(s: SubjectData): Unit = {
            val oldData = RecordSubjectData()
            oldData.copy(s)
            originalSubjectData = (s.id, false, oldData)
        }

        def restoreOriginalData(s: SubjectData): Unit = {
            if (s.id == originalSubjectData._1) {
                if (!originalSubjectData._2) { // 没有点击确认
                    s.copy(originalSubjectData._3)
                }
            } else {
                dom.console.info("需要还原的数据没有记录")
            }
        }

        def confirmContent(s: SubjectData): Callback = {
            if (s != null) {
                if (s.id == originalSubjectData._1) {
                    if (!originalSubjectData._2) { // 没有更新
                        val newSubjectData: RecordSubjectData = RecordSubjectData()
                        newSubjectData.copy(s)
                        originalSubjectData = (s.id, true, newSubjectData)
                        $.modState(s => s)
                    } else { // 已更新
                        Callback()
                    }
                } else { // 记录的数据和当前选中的不一样
                    Callback()
                }
            } else { // 不做出反应
                Callback()
            }
        }

        def jumpLink(s: SubjectData) = Callback {
            isSelected = null
            originalSubjectData = null
            val url = s"http://localhost:8080/#subjectsView/$processID/${s.id}/-1"
            dom.window.location.href = url
        }

        def interactionRelationship = {
            val num = ProcessManager.processMap(processID).subjectList.size
            for (i <- 0 until num) {
                val currentSubject = ProcessManager.processMap(processID).subjectList(i)
                currentSubject.stateList.foreach(actionState => {
                    if (actionState.isInstanceOf[GraphObject.Receive]) {
                        val currentReceiveState = actionState.asInstanceOf[GraphObject.Receive]
                        val entireTransitionList: ListBuffer[TransitionData] = ListBuffer()
                        currentReceiveState.data.directChildrenTransitionsMap.values.foreach(f => entireTransitionList ++= f)
                        currentReceiveState.data.nonDirectChildrenTransitionsMap.values.foreach(f => entireTransitionList ++= f)
                        for (elem <- entireTransitionList) {
                            //elem is TransitionData
                            val msgType = elem.information.relatedMessageType
                            val subName = elem.information.relatedSubjectName
                            if (msgType != "" && subName != "") {
                                val relatedSubjectID = checkSubjectId(subName)
                                if (subjectReceiveMessages.contains(currentSubject.id)) {
                                    if (subjectReceiveMessages(currentSubject.id).contains(relatedSubjectID)) {
                                        subjectReceiveMessages(currentSubject.id)(relatedSubjectID) += msgType
                                    } else {
                                        subjectReceiveMessages(currentSubject.id) += relatedSubjectID -> Set(msgType)
                                    }

                                } else {
                                    subjectReceiveMessages += currentSubject.id -> Map()
                                    subjectReceiveMessages(currentSubject.id) += relatedSubjectID -> Set(msgType)
                                }
                                // recordRelatedTransition
                                if (transitionOfReceiveMsg.contains(currentSubject.id)) {
                                    if (transitionOfReceiveMsg(currentSubject.id).contains(relatedSubjectID)) {
                                        transitionOfReceiveMsg(currentSubject.id)(relatedSubjectID) += elem.id
                                    } else {
                                        transitionOfReceiveMsg(currentSubject.id) += relatedSubjectID -> Set(elem.id)
                                    }
                                } else {
                                    transitionOfReceiveMsg += currentSubject.id -> Map()
                                    transitionOfReceiveMsg(currentSubject.id) += relatedSubjectID -> Set(elem.id)
                                }

                                // record receivedSubjectID
                                if (subjectReceiveState.contains(currentSubject.id)) {
                                    subjectReceiveState(currentSubject.id) += relatedSubjectID
                                } else {
                                    subjectReceiveState += currentSubject.id -> Set(relatedSubjectID)
                                }
                                // record sentRelatedSubjectID
                                if (subjectSendState.contains(relatedSubjectID)) {
                                    subjectSendState(relatedSubjectID) += currentSubject.id
                                } else {
                                    subjectSendState += (relatedSubjectID -> Set(currentSubject.id))
                                }
                            }
                        }
                    }

                    if (actionState.isInstanceOf[GraphObject.Send]) {
                        val currentSendState = actionState.asInstanceOf[GraphObject.Send]
                        val entireTransitionList: ListBuffer[TransitionData] = ListBuffer()
                        currentSendState.data.directChildrenTransitionsMap.values.foreach(f => entireTransitionList ++= f)
                        currentSendState.data.nonDirectChildrenTransitionsMap.values.foreach(f => entireTransitionList ++= f)
                        for (elem <- entireTransitionList) {
                            val msgType = elem.information.relatedMessageType
                            val subName = elem.information.relatedSubjectName
                            if (msgType != "" && subName != "") {
                                val relatedSubjectID = checkSubjectId(subName)
                                if (subjectSendMessages.contains(currentSubject.id)) {
                                    if (subjectSendMessages(currentSubject.id).contains(relatedSubjectID)) {
                                        subjectSendMessages(currentSubject.id)(relatedSubjectID) += msgType
                                    } else {
                                        subjectSendMessages(currentSubject.id) += relatedSubjectID -> Set(msgType)
                                    }
                                } else {
                                    subjectSendMessages += currentSubject.id -> Map()
                                    subjectSendMessages(currentSubject.id) += relatedSubjectID -> Set(msgType)
                                }
                                // record relatedTransition
                                if (transitionOfSendingMsg.contains(currentSubject.id)) {
                                    if (transitionOfSendingMsg(currentSubject.id).contains(relatedSubjectID)) {
                                        transitionOfSendingMsg(currentSubject.id)(relatedSubjectID) += elem.id
                                    } else {
                                        transitionOfSendingMsg(currentSubject.id) += relatedSubjectID -> Set(elem.id)
                                    }
                                } else {
                                    transitionOfSendingMsg += currentSubject.id -> Map()
                                    transitionOfSendingMsg(currentSubject.id) += relatedSubjectID -> Set(elem.id)
                                }
                                // record sentSubjectID
                                if (subjectSendState.contains(currentSubject.id)) {
                                    subjectSendState(currentSubject.id) += relatedSubjectID
                                } else {
                                    subjectSendState += currentSubject.id -> Set(relatedSubjectID)
                                }
                                // record receivedRelatedSubjectID
                                if (subjectReceiveState.contains(relatedSubjectID)) {
                                    subjectReceiveState(relatedSubjectID) += currentSubject.id
                                } else {
                                    subjectReceiveState += (relatedSubjectID -> Set(currentSubject.id))
                                }
                            }
                        }
                    }
                })
            }
        }

        // sort points of each subject
        def sortPoints = { // case class Point()
            subjectSendState.foreach(s => {
                val currentStartSubject = ProcessManager.processMap(processID).subjectMap(s._1.toString)
                currentStartSubject.exitPoints += "up" -> ListBuffer()
                currentStartSubject.exitPoints += "bottom" -> ListBuffer()
                currentStartSubject.exitPoints += "left" -> ListBuffer()
                currentStartSubject.exitPoints += "right" -> ListBuffer()
                s._2.foreach(ts => {
                    val startPoint = subjectPosition(s._1) // startSubjectPosition
                    val endPoint = subjectPosition(ts) // endSubjectPosition
                    val directionStartPoint = startPoint - endPoint
                    if (directionStartPoint == 1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 0
                        currentPoint.pointPosition = "left"
                        currentStartSubject.exitPoints("left") += currentPoint
                    }
                    if (directionStartPoint == -1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 0
                        currentPoint.pointPosition = "right"
                        currentStartSubject.exitPoints("right") += currentPoint
                    }
                    if (directionStartPoint < -1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 0
                        currentPoint.pointPosition = "bottom"
                        currentStartSubject.exitPoints("bottom") += currentPoint
                    }
                    if (directionStartPoint > 1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 0
                        currentPoint.pointPosition = "up"
                        currentStartSubject.exitPoints("up") += currentPoint
                    }
                })
            })
            subjectReceiveState.foreach(s => {
                val currentEndSubject = ProcessManager.processMap(processID).subjectMap(s._1.toString)
                currentEndSubject.entryPoint += "up" -> ListBuffer()
                currentEndSubject.entryPoint += "bottom" -> ListBuffer()
                currentEndSubject.entryPoint += "left" -> ListBuffer()
                currentEndSubject.entryPoint += "right" -> ListBuffer()
                s._2.foreach(r => {
                    val endPoint = subjectPosition(s._1) // endSubjectPosition
                    val startPoint = subjectPosition(r) // startSubjectPosition
                    val directionEndPoint = startPoint - endPoint
                    if (directionEndPoint == 1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 1
                        currentPoint.pointPosition = "right"
                        currentEndSubject.entryPoint("right") += currentPoint
                    }
                    if (directionEndPoint == -1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 1
                        currentPoint.pointPosition = "left"
                        currentEndSubject.entryPoint("left") += currentPoint
                    }
                    if (directionEndPoint < -1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 1
                        currentPoint.pointPosition = "bottom"
                        currentEndSubject.entryPoint("bottom") += currentPoint
                    }
                    if (directionEndPoint > 1) {
                        var currentPoint = Point()
                        currentPoint.pointType = 1
                        currentPoint.pointPosition = "up"
                        currentEndSubject.entryPoint("up") += currentPoint
                    }
                })
            })
        }

        // allocate coordinate to each point
        def allocateCoordinate: Unit = {
            ProcessManager.processMap(processID).subjectList.foreach(s => {
                // up arrow
                if (s.exitPoints.contains("up")) {
                    var numUp = 0
                    if (s.entryPoint.contains("up")) {
                        //有出有入
                        numUp = s.exitPoints("up").size + s.entryPoint("up").size
                    } else {
                        // 有出无入
                        numUp = s.exitPoints("up").size
                    }
                    val lengthUp = 100 / (numUp + 1)
                    val exitNumUp = s.exitPoints("up").size
                    for (i <- 0 until exitNumUp) {
                        val newPointX = s.subjectX + (i + 1) * lengthUp
                        val newPointY = s.subjectY
                        s.exitPoints("up")(i).pointX = newPointX
                        s.exitPoints("up")(i).pointY = newPointY
                        s.exitPoints("up")(i).order = i
                    }
                    for (i <- 0 until (numUp - exitNumUp)) {
                        val newPointX = s.subjectX + (i + exitNumUp + 1) * lengthUp
                        val newPointY = s.subjectY
                        s.entryPoint("up")(i).pointX = newPointX
                        s.entryPoint("up")(i).pointY = newPointY
                        s.entryPoint("up")(i).order = i
                    }
                } else { // 没有outgoing edges
                    if (s.entryPoint.contains("up")) {
                        val entryNum = s.entryPoint("up").size
                        val length = 100 / (entryNum + 1)
                        for (i <- 0 until entryNum) {
                            val newPointX = s.subjectX + (i + 1) * length
                            val newPointY = s.subjectY
                            s.entryPoint("up")(i).pointX = newPointX
                            s.entryPoint("up")(i).pointY = newPointY
                            s.entryPoint("up")(i).order = i
                        }
                    }
                }
                // bottom arrow
                if (s.exitPoints.contains("bottom")) {
                    var numBottom = 0
                    var entryNumBottom = 0
                    if (s.entryPoint.contains("bottom")) {
                        numBottom = s.exitPoints("bottom").size + s.entryPoint("bottom").size
                        entryNumBottom = s.entryPoint("bottom").size
                    } else {
                        numBottom = s.exitPoints("bottom").size
                        entryNumBottom = 0
                    }
                    val lengthBottom = 100 / (numBottom + 1)
                    for (i <- 0 until entryNumBottom) {
                        val newPointX = s.subjectX + (i + 1) * lengthBottom
                        val newPointY = s.subjectY + 150
                        s.entryPoint("bottom")(i).pointX = newPointX
                        s.entryPoint("bottom")(i).pointY = newPointY
                        s.entryPoint("bottom")(i).order = i
                    }
                    for (i <- 0 until (numBottom - entryNumBottom)) {
                        val newPointX = s.subjectX + (i + entryNumBottom + 1) * lengthBottom
                        val newPointY = s.subjectY + 150
                        s.exitPoints("bottom")(i).pointX = newPointX
                        s.exitPoints("bottom")(i).pointY = newPointY
                        s.exitPoints("bottom")(i).order = i
                    }
                } else { // there is no outgoing edges.
                    if (s.entryPoint.contains("bottom")) {
                        val entryNum = s.entryPoint("bottom").size
                        val length = 100 / (entryNum + 1)
                        for (i <- 0 until entryNum) {
                            val newPointX = s.subjectX + (i + 1) * length
                            val newPointY = s.subjectY + 150
                            s.entryPoint("bottom")(i).pointX = newPointX
                            s.entryPoint("bottom")(i).pointY = newPointY
                            s.entryPoint("bottom")(i).order = i
                        }
                    }
                }
                // left arrow
                if (s.exitPoints.contains("left")) {
                    var numLeft = 0
                    var entryNumLeft = 0
                    if (s.entryPoint.contains("left")) {
                        numLeft = s.exitPoints("left").size + s.entryPoint("left").size
                        entryNumLeft = s.entryPoint("left").size
                    } else {
                        numLeft = s.exitPoints("left").size
                        entryNumLeft = 0
                    }
                    val lengthLeft = 150 / (numLeft + 1)
                    for (i <- 0 until entryNumLeft) {
                        val newPointX = s.subjectX
                        val newPointY = s.subjectY + (i + 1) * lengthLeft
                        s.entryPoint("left")(i).pointX = newPointX
                        s.entryPoint("left")(i).pointY = newPointY
                        s.entryPoint("left")(i).order = i
                    }
                    val a = numLeft - entryNumLeft
                    for (i <- 0 until a) {
                        val newPointX = s.subjectX
                        val newPointY = s.subjectY + (i + entryNumLeft + 1) * lengthLeft
                        s.exitPoints("left")(i).pointX = newPointX
                        s.exitPoints("left")(i).pointY = newPointY
                        s.exitPoints("left")(i).order = i
                    }
                } else {
                    if (s.entryPoint.contains("left")) {
                        val entryNum = s.entryPoint("left").size
                        val length = 150 / (entryNum + 1)
                        for (i <- 0 until entryNum) {
                            val newPointX = s.subjectX
                            val newPointY = s.subjectY + (i + 1) * length
                            s.entryPoint("left")(i).pointX = newPointX
                            s.entryPoint("left")(i).pointY = newPointY
                            s.entryPoint("left")(i).order = i
                        }
                    }

                }
                // right arrow
                if (s.exitPoints.contains("right")) {
                    var numRight = 0
                    val exitNumRight = s.exitPoints("right").size
                    if (s.entryPoint.contains("right")) {
                        numRight = s.exitPoints("right").size + s.entryPoint("right").size
                    } else {
                        numRight = s.exitPoints("right").size
                    }
                    val lengthRight = 150 / (numRight + 1)
                    for (i <- 0 until exitNumRight) {
                        val newPointX = s.subjectX + 100
                        val newPointY = s.subjectY + (i + 1) * lengthRight
                        s.exitPoints("right")(i).pointX = newPointX
                        s.exitPoints("right")(i).pointY = newPointY
                        s.exitPoints("right")(i).order = i
                    }
                    val a = numRight - exitNumRight
                    for (i <- 0 until a) {
                        val newPointX = s.subjectX + 100
                        val newPointY = s.subjectY + (i + exitNumRight + 1) * lengthRight
                        s.entryPoint("right")(i).pointX = newPointX
                        s.entryPoint("right")(i).pointY = newPointY
                        s.entryPoint("right")(i).order = i
                    }
                } else {
                    if (s.entryPoint.contains("right")) {
                        val entryNum = s.entryPoint("right").size
                        val length = 150 / (entryNum + 1)
                        for (i <- 0 until entryNum) {
                            val newPointX = s.subjectX + 100
                            val newPointY = s.subjectY + (i + 1) * length
                            s.entryPoint("right")(i).pointX = newPointX
                            s.entryPoint("right")(i).pointY = newPointY
                            s.entryPoint("right")(i).order = i
                        }
                    }
                }
            })
        }

        def reverse = {
            subjectPosition.foreach(idP => {
                subjectPositionReversedMap += (idP._2 -> idP._1)
            })
        }

        // subjectSendState
        def sortSubject(s: Map[Int, Set[Int]]): Map[Int, Map[String, ListBuffer[Int]]] = {
            //subjectID, "up" -> ListBuffer[subjectId]
            val temporaryMap: Map[Int, Map[String, ListBuffer[Int]]] = Map()
            s.foreach(startSubject => {
                temporaryMap += (startSubject._1 -> Map())
                val temporaryUpList: ListBuffer[Int] = ListBuffer()
                val temporaryBottomList: ListBuffer[Int] = ListBuffer()
                var leftMap: Map[String, ListBuffer[Int]] = Map()
                var rightMap: Map[String, ListBuffer[Int]] = Map()
                var bottomMap: Map[String, ListBuffer[Int]] = Map()
                var upMap: Map[String, ListBuffer[Int]] = Map()
                val allEndSubject = startSubject._2 // Set()
                for (item <- allEndSubject) {
                    val distance = subjectPosition(startSubject._1) - subjectPosition(item)
                    if (distance == 1) {
                        leftMap += ("left" -> ListBuffer(item))
                    }
                    if (distance == -1) {
                        rightMap += ("right" -> ListBuffer(item))
                    }
                    if (distance < -1) {
                        if (bottomMap.isEmpty) {
                            temporaryBottomList += subjectPosition(item) // targetSubject的位置
                            bottomMap += ("bottom" -> ListBuffer())
                        } else {
                            temporaryBottomList += subjectPosition(item)
                        }
                    }
                    if (distance > 1) {
                        if (upMap.isEmpty) {
                            temporaryUpList += subjectPosition(item)
                            upMap += ("up" -> ListBuffer())
                        } else {
                            temporaryUpList += subjectPosition(item)
                        }
                    }
                }
                temporaryBottomList.sorted.reverse.foreach(p => {
                    bottomMap("bottom") += subjectPositionReversedMap(p)
                })
                temporaryUpList.sorted.reverse.foreach(p => {
                    upMap("up") += subjectPositionReversedMap(p)
                })
                if (!leftMap.isEmpty) {
                    temporaryMap(startSubject._1) += ("left" -> leftMap("left"))
                }
                if (!rightMap.isEmpty) {
                    temporaryMap(startSubject._1) += ("right" -> rightMap("right"))
                }
                if (!bottomMap.isEmpty) {
                    temporaryMap(startSubject._1) += ("bottom" -> bottomMap("bottom"))
                }
                if (!upMap.isEmpty) {
                    temporaryMap(startSubject._1) += ("up" -> upMap("up"))
                }
            })
            temporaryMap // exitSubjectSequence
        }

        def pair(exitSubjectSequence: Map[Int, Map[String, ListBuffer[Int]]]) = {
            val direction = List("up", "bottom", "left", "right")
            subjectsList.foreach(s => {
                val sourceSubjectID = s.id
                val sourceSubject = s
                var msgList: Set[String] = Set()
                if (exitSubjectSequence.contains(sourceSubjectID)) {
                    val correspondingArrowList = exitSubjectSequence(sourceSubjectID)
                    for (i <- 0 until direction.size) {
                        val d = direction(i)
                        if ((sourceSubject.exitPoints(d).size != 0) && correspondingArrowList.contains(d)) {
                            var temporarySubjectSequence: ListBuffer[Int] = correspondingArrowList(d)
                            sourceSubject.exitPoints(d).foreach(p => {
                                if (temporarySubjectSequence != null) {
                                    val endSID = temporarySubjectSequence.head // 距离currentSubject最近的一个
                                    val endSub = ProcessManager.processMap(processID).subjectMap(endSID.toString)
                                    val determinedEndPoint = endSub.getAvailablePoint(d)
                                    val determinedStartPointCoordinate = (p.pointX, p.pointY)
                                    val determinedEndPointCoordinate = (determinedEndPoint.pointX, determinedEndPoint.pointY)
                                    temporarySubjectSequence = temporarySubjectSequence.tail

                                    if (subjectSendMessages.contains(sourceSubjectID)) {
                                        if (subjectSendMessages(sourceSubjectID).contains(endSID)) {
                                            msgList = subjectSendMessages(sourceSubjectID)(endSID)
                                        }
                                    } else {
                                        msgList = subjectReceiveMessages(endSID)(sourceSubjectID)
                                    }
                                    matchMessage
                                    if ((d == "bottom") || (d == "up")) {
                                        sortArrow(d, sourceSubjectID, endSID)
                                    }
                                    val newArrow = InteractionData()
                                    newArrow.direction = d
                                    newArrow.start = determinedStartPointCoordinate
                                    newArrow.end = determinedEndPointCoordinate
                                    newArrow.startID = sourceSubjectID
                                    newArrow.endID = endSID
                                    newArrow.layer = 0
                                    newArrow.msg = msgList
                                    communicationList += newArrow
                                }
                            })
                        }
                    }
                } else {
                    // 不包含当前subject
                }
            })
        }

        def computerLayer: Unit = {
            communicationList.foreach(a => {
                if ((a.direction == "bottom") || (a.direction == "up")) {
                    val start = a.startID
                    val end = a.endID
                    val newLayer = searchArrow(a.direction, start, end)
                    a.layer = newLayer
                }
            })
        }

        val errorMatchMessage: Map[Int, Set[Int]] = Map()

        def matchMessage: Unit = {
            subjectSendMessages.foreach(startSub => {
                startSub._2.foreach(endSub => {
                    val end = endSub._1 // endID
                    if (subjectReceiveMessages.contains(end)) {
                        if (subjectReceiveMessages(end).contains(startSub._1)) {
                            val sendMsgSize = endSub._2.size
                            val receiveMsgSize = subjectReceiveMessages(end)(startSub._1).size
                            if (sendMsgSize != receiveMsgSize) {
                                if (errorMatchMessage.contains(startSub._1)) {
                                    errorMatchMessage(startSub._1) += end
                                } else {
                                    errorMatchMessage += startSub._1 -> Set(end)
                                }
                            }
                        } else {
                            if (errorMatchMessage.contains(startSub._1)) {
                                errorMatchMessage(startSub._1) += end
                            } else {
                                errorMatchMessage += startSub._1 -> Set(end)
                            }
                        }
                    } else {
                        if (errorMatchMessage.contains(startSub._1)) {
                            errorMatchMessage(startSub._1) += end
                        } else {
                            errorMatchMessage += startSub._1 -> Set(end)
                        }
                    }
                })
            })
            subjectReceiveMessages.foreach(startSub => {
                startSub._2.foreach(endSub => {
                    val end = endSub._1
                    if (subjectSendMessages.contains(end)) {
                        if (subjectSendMessages(end).contains(startSub._1)) {
                            val sendMsgSize = endSub._2.size
                            val receiveMsgSize = subjectSendMessages(end)(startSub._1).size
                            if (sendMsgSize != receiveMsgSize) {
                                if (errorMatchMessage.contains(end)) {
                                    errorMatchMessage(end) += startSub._1
                                } else {
                                    errorMatchMessage += end -> Set(startSub._1)
                                }
                            }
                        } else {
                            if (errorMatchMessage.contains(end)) {
                                errorMatchMessage(end) += startSub._1
                            } else {
                                errorMatchMessage += end -> Set(startSub._1)
                            }
                        }
                    } else {
                        if (errorMatchMessage.contains(end)) {
                            errorMatchMessage(end) += startSub._1
                        } else {
                            errorMatchMessage += end -> Set(startSub._1)
                        }
                    }
                })
            })
        }

        def sortArrow(direction: String, startSub: Int, endSub: Int): Unit = {
            direction match {
                case "bottom" => {
                    bottomArrowMap.foreach(a => {
                        a._2.foreach(b => {
                            if (b._2 == startSub) {
                                val layer = bottomArrowMap.size - a._1
                                finalBottomArrowMap += layer -> Map(b._1 -> b._2)
                                if (bottomArrowFlag) { // 需要替换
                                    bottomArrowMap(a._1) = Map(startSub -> endSub)
                                    bottomArrowFlag = false
                                    bottomArrowOk = true
                                } else {
                                    // 第一个已经替换了，之后的删除
                                    bottomArrowMap(a._1) = Map()
                                }
                            }
                        })
                    })

                    if (!bottomArrowOk) {
                        if (bottomArrowMap.isEmpty) {
                            bottomArrowMap += bottomArrowCounter -> Map()
                            bottomArrowMap(bottomArrowCounter) += startSub -> endSub
                            bottomArrowCounter += 1
                        } else {
                            bottomArrowMap.foreach(c => { // 寻找空位
                                if (c._2 == null && bottomArrowIsAdded) {
                                    bottomArrowMap(c._1) += (startSub -> endSub)
                                    bottomArrowIsAdded = false
                                }
                            })

                            if (bottomArrowIsAdded) { // 说明没有空位子
                                bottomArrowMap += bottomArrowCounter -> Map()
                                bottomArrowMap(bottomArrowCounter) += startSub -> endSub
                                bottomArrowCounter += 1
                            } else {
                                // 已添加，没有后续操作
                            }
                        }
                    }
                    // 还原
                    bottomArrowFlag = true
                    bottomArrowOk = false
                    bottomArrowIsAdded = true
                    //          bottomArrowMap.foreach(a => {
                    //          })
                }

                case "up" => {
                    upArrowMap.foreach(a => { // a._1 是层数
                        a._2.foreach(b => { // (start ,end )
                            if (b._1 == endSub) {
                                val layer = a._1 + 1
                                finalUpArrowMap += layer -> Map(b._1 -> b._2)
                                if (upArrowFlag) { // 需要替换
                                    upArrowMap(a._1) = Map(startSub -> endSub)
                                    upArrowFlag = false
                                    upArrowOk = true
                                } else {
                                    // 第一个已经替换了，之后的删除
                                    upArrowMap(a._1) = Map()
                                }
                            }
                        })
                    })

                    if (!upArrowOk) {
                        if (upArrowMap.isEmpty) {
                            upArrowMap += upArrowCounter -> Map()
                            upArrowMap(upArrowCounter) += startSub -> endSub
                            upArrowCounter += 1
                        } else {
                            upArrowMap.foreach(c => { // 寻找空位
                                if (c._2 == null && upArrowIsAdded) {
                                    upArrowMap(c._1) += (startSub -> endSub)
                                    upArrowIsAdded = false
                                }
                            })

                            if (upArrowIsAdded) { // 说明没有空位子
                                upArrowMap += upArrowCounter -> Map()
                                upArrowMap(upArrowCounter) += startSub -> endSub
                                upArrowCounter += 1
                            } else {
                                // 已添加，没有后续操作
                            }
                        }
                    }
                    // 还原
                    upArrowFlag = true
                    upArrowOk = false
                    upArrowIsAdded = true
                    //          upArrowMap.foreach(a => {
                    //
                    //          })
                }
            }
        }

        def searchArrow(direction: String, sId: Int, eId: Int): Int = {
            val newMap: Map[Int, Int] = Map()
            var layer = 0
            direction match {
                case "bottom" => {
                    newMap += (sId -> eId)
                    bottomArrowMap.foreach(a => {
                        if (a._2.equals(newMap)) {
                            layer = bottomArrowMap.size - a._1
                        }
                    })
                    finalBottomArrowMap.foreach(b => {
                        if (b._2.equals(newMap)) {
                            layer = b._1
                        }
                    })
                    layer
                }
                case "up" => {
                    newMap += (sId -> eId)
                    upArrowMap.foreach(a => {
                        if (a._2.equals(newMap)) {
                            layer = a._1 + 1
                        }
                    })
                    finalUpArrowMap.foreach(b => {
                        if (b._2.equals(newMap)) {
                            layer = b._1
                        }
                    })
                    layer
                }
            }
        }

        def drawArrowOfSubject(a: InteractionData): VdomElement = { // 箭头方向，起点坐标， 终点坐标
            //todo 暂时右拐
//            a.direction = "right"
            val startX = a.start._1
            val startY = a.start._2
            val endX = a.end._1
            val endY = a.end._2
            val startID = a.startID
            val endID = a.endID
            var arrow: VdomElement = null
            var arrowColor = "black"
            if (errorMatchMessage.contains(startID)) {
                if (errorMatchMessage(startID).contains(endID)) {
                    arrowColor = "red"
                } else {
                    arrowColor = "black"
                }
            } else {
                arrowColor = "black"
            }
            a.direction match {
                case "up" => {
                    val n = searchArrow("up", a.startID, a.endID)
                    val gap = 40
                    val actualLength = 10 + (gap * n)
                    val arrowWidth = math.abs(endX - startX)
                    arrow =
                            <.div(
                                ^.position.absolute,
                                ^.left := (startX).px,
                                ^.top := (startY - actualLength).px,
                                ^.width := 2.px,
                                ^.height := actualLength.px,
                                ^.backgroundColor := arrowColor,
                                <.div(
                                    ^.position.absolute,
                                    ^.left := (-arrowWidth).px,
                                    ^.top := 0.px,
                                    ^.width := arrowWidth.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    <.div(
                                        ^.position.absolute,
                                        ^.left := 0.px,
                                        ^.top := 0.px,
                                        ^.width := 2.px,
                                        ^.height := actualLength.px,
                                        ^.backgroundColor := arrowColor,
                                        <.div(
                                            ^.position.absolute,
                                            ^.left := 2.px,
                                            ^.top := (actualLength - 7).px,
                                            ^.width := 2.px,
                                            ^.height := 7.px,
                                            ^.backgroundColor := arrowColor,
                                            ^.transform := "rotate(28deg)"
                                        ),
                                        <.div(
                                            ^.position.absolute,
                                            ^.left := (-2).px,
                                            ^.top := (actualLength - 7).px,
                                            ^.width := 2.px,
                                            ^.height := 7.px,
                                            ^.backgroundColor := arrowColor,
                                            ^.transform := "rotate(-28deg)"
                                        )
                                    )
                                )
                            )
                    val newLabel = new MessageLabel(setLabelID, "up", startX, startY, arrowWidth, actualLength, a.msg)
                    if (!labelMap.contains((a.startID, a.endID))) {
                        labelMap += (a.startID, a.endID) -> newLabel
                    }
                }
                case "bottom" => {
                    val n = searchArrow("bottom", a.startID, a.endID)
                    val gap = 40
                    val actualLength = 10 + (gap * n)
                    val arrowWidth = math.abs(endX - startX)
                    arrow =
                            <.div(
                                ^.position.absolute,
                                ^.left := (startX).px,
                                ^.top := (startY + 1).px,
                                ^.width := 2.px,
                                ^.height := actualLength.px,
                                ^.backgroundColor := arrowColor,
                                <.div(
                                    ^.position.absolute,
                                    ^.left := 0.px,
                                    ^.top := actualLength.px,
                                    ^.width := arrowWidth.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    <.div(
                                        ^.position.absolute,
                                        ^.left := (arrowWidth - 2).px,
                                        ^.top := (-actualLength).px,
                                        ^.width := 2.px,
                                        ^.height := actualLength.px,
                                        ^.backgroundColor := arrowColor,
                                        <.div(
                                            ^.position.absolute,
                                            ^.left := 2.px,
                                            ^.top := (0).px,
                                            ^.width := 2.px,
                                            ^.height := 7.px,
                                            ^.backgroundColor := arrowColor,
                                            ^.transform := "rotate(-28deg)"
                                        ),
                                        <.div(
                                            ^.position.absolute,
                                            ^.left := (-2).px,
                                            ^.top := 0.px,
                                            ^.width := 2.px,
                                            ^.height := 7.px,
                                            ^.backgroundColor := arrowColor,
                                            ^.transform := "rotate(28deg)"
                                        )
                                    )
                                )
                            )

                    val newLabel = new MessageLabel(setLabelID, "bottom", startX, startY, arrowWidth, actualLength, a.msg)
                    if (!labelMap.contains((a.startID, a.endID))) {
                        labelMap += (a.startID, a.endID) -> newLabel
                    }
                }
                case "left" => {
                    val arrowWidth = math.abs(endX - startX)
                    arrow =
                            <.div(
                                ^.position.absolute,
                                ^.left := (startX - arrowWidth).px,
                                ^.top := startY.px,
                                ^.width := arrowWidth.px,
                                ^.height := 2.px,
                                ^.backgroundColor := arrowColor,
                                <.div(
                                    ^.position.absolute,
                                    ^.left := 0.px,
                                    ^.top := 3.px,
                                    ^.width := 10.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    ^.transform := "rotate(28deg)"
                                ),
                                <.div(
                                    ^.position.absolute,
                                    ^.left := 0.px,
                                    ^.top := (-3).px,
                                    ^.width := 10.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    ^.transform := "rotate(-28deg)"
                                )
                            )
                    val newLabel = new MessageLabel(setLabelID, "left", (startX - arrowWidth), a.start._2, arrowWidth, 0, a.msg)
                    if (!labelMap.contains((a.startID, a.endID))) {
                        labelMap += (a.startID, a.endID) -> newLabel
                    }
                }
                case "right" => {
                    val arrowWidth = math.abs(endX - startX)
                    arrow =
                            <.div(
                                ^.position.absolute,
                                ^.left := (startX + 1).px,
                                ^.top := startY.px,
                                ^.width := arrowWidth.px,
                                ^.height := 2.px,
                                ^.backgroundColor := arrowColor,
                                <.div(
                                    ^.position.absolute,
                                    ^.left := (arrowWidth - 10).px,
                                    ^.top := 3.px,
                                    ^.width := 10.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    ^.transform := "rotate(-28deg)"
                                ),
                                <.div(
                                    ^.position.absolute,
                                    ^.left := (arrowWidth - 10).px,
                                    ^.top := (-3).px,
                                    ^.width := 10.px,
                                    ^.height := 2.px,
                                    ^.backgroundColor := arrowColor,
                                    ^.transform := "rotate(28deg)"
                                )
                            )
                    val newLabel = new MessageLabel(setLabelID, "right", a.start._1, a.start._2, arrowWidth, 0, a.msg)
                    if (!labelMap.contains((a.startID, a.endID))) {
                        labelMap += (a.startID, a.endID) -> newLabel
                    }
                }
            }
            return arrow
        }

        def restoreInitialization: Unit = {
            // 他们都是全局变量
            bottomArrowCounter = 0 // 每次都归零，重新计算
            upArrowCounter = 0
            arrowPair.clear()
            bottomArrowMap.clear()
            upArrowMap.clear()
            finalUpArrowMap.clear()
            finalBottomArrowMap.clear()
            communicationList.clear()
            subjectPosition.clear() // 实时定位，如果没有这一步，之前删除的subject的Id仍然保留
            var i = 1
            for (item <- (subjectsList.sortBy(s => s.id.toInt))) {
                subjectPosition += item.id -> i
                i += 1
            }
            subjectReceiveState.clear()
            subjectSendState.clear()
            subjectSendMessages.clear()
            subjectReceiveMessages.clear()
            transitionOfSendingMsg.clear()
            transitionOfReceiveMsg.clear()
        }

        def communicationRelationShip: ListBuffer[VdomElement] = {
            restoreInitialization
            interactionRelationship
            if ((subjectReceiveState.size != 0) && (subjectSendState.size != 0)) {
                sortPoints
                allocateCoordinate
                reverse
                val exitSubjectSequence = sortSubject(subjectSendState)
                pair(exitSubjectSequence)
                computerLayer
                communicationList.foreach(a => {
                    arrowPair += drawArrowOfSubject(a)
                })
                arrowPair
            } else {
                ListBuffer()
            }
        }

        var collectorId: Int = 300

        def setLabelID: Int = {
            collectorId += 1
            collectorId
        }

        val labelMap: Map[(Int, Int), MessageLabel] = Map()

        case class MessageLabel(id: Int, direction: String, arrowStartX: Int, arrowStartY: Int, arrowLength: Int, arrowHeight: Int, msg: Set[String]) {
            var height = 30
            var flag = true

            def setHeight(h: Int): Unit = {
                height = h
            }

            def determineActualLeft(d: String): Int = {
                if ((d == "left") || (d == "right")) {
                    ((arrowLength / 2) - 50)
                } else if (d == "bottom") {
                    40
                } else {
                    -140
                }
            }

            def determineHeight(d: String): Int = {
                if (d == "left" || d == "right") {
                    (arrowStartY - height / 2)
                } else if (d == "bottom") {
                    (arrowStartY + arrowHeight - height / 2)
                } else {
                    (arrowStartY - arrowHeight) - (height / 2)
                }
            }

            var actualLeft = determineActualLeft(direction) // left and right
            var actualTop = determineHeight(direction)

            val changedDIV = Ref[dom.html.Div]

            def content = <.div(
                ^.position.absolute,
                ^.left := (arrowStartX + actualLeft).px,
                ^.top := determineHeight(direction).px,
                ^.width := 100.px,
                ^.height := height.px,
                ^.border := "dashed 1px black",
                ^.backgroundColor := "#EEEEEE",
                ^.overflow.hidden,
                <.div(
                    ^.position.absolute,
                    ^.left := 87.px,
                    ^.top := (0).px,
                    ^.width := 10.px,
                    ^.height := 10.px,
                    unfold,
                    ^.cursor.pointer,
                    ^.onClick --> changeHeight(this)
                ),
                msg.toTagMod(item => {
                    <.li(
                        ^.paddingLeft := 5.px,
                        item,
                        ^.position.relative,
                        ^.top := 12.px,
                        ^.fontSize := 11.px,
                        ^.wordWrap.`break-word`,
                        ^.wordBreak.`break-all`
                    )
                })
            ).withRef(changedDIV)
        }

        def changeHeight(label: MessageLabel): Callback = {
            val actualH = label.changedDIV.unsafeGet().scrollHeight
            if (actualH > 30) {
                if (label.flag) {
                    label.setHeight(actualH)
                    label.flag = false
                } else {
                    label.setHeight(30)
                    label.flag = true
                }
            } else {
                label.setHeight(30)
            }

            $.modState(s => s)
        }

        var unfold =
            <.div(
                ^.position.relative,
                ^.marginTop := 3.px,
                ^.width := 0.px,
                ^.height := 0.px,
                ^.borderRight := "solid 5px transparent",
                ^.borderLeft := "solid 5px transparent",
                ^.borderTop := "solid 5px black"
            )

    }

    val Main = ScalaComponent.builder[Unit]("SubjectsPage")
            .initialState(State(ProcessManager.processMap(processID).subjectList.sortBy(f =>{f.id})))
            .renderBackend[Backend]
            .build

    val component = ScalaComponent.builder[SubjectsVP]("SubjectsPage").render { p =>
        processID = p.props.processid
        Main()
    }.build

}
