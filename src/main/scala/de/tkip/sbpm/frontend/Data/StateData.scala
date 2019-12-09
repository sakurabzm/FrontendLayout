package de.tkip.sbpm.frontend.Data

import de.tkip.sbpm.frontend.graph.StateGraph
import org.scalajs.dom

import scala.collection.mutable.ListBuffer
import scala.scalajs.js

case class StateJsonData(id: Int, stateType: String, isStartState: Boolean, description: String,
                         transitionsList: ListBuffer[TransitionData], xoffset: Int, yoffset: Int)

case class SubjectContent(subjectid: String, stateList: ListBuffer[StateJsonData])

case class Message() {
    var currentMessageType: String = ""
    var currentRelatedSubjectName: String = ""
    var num: Int = 1

    def setRelatedMessageType(msg: String): Unit = {
        currentMessageType = msg
    }

    def setRelatedSubjectName(subName: String) = {
        currentRelatedSubjectName = subName
    }

    def setSendingNum(n: Int) = {
        num = n
    }

    def copy(msgData: Message): Unit = {
        this.currentMessageType = msgData.currentMessageType
        this.currentRelatedSubjectName = msgData.currentRelatedSubjectName
        this.num = msgData.num
    }

}

case class RestoredData() {
    var id: Int = _
    var name: String = ""
    var stateType: String = ""
    var isStartState: Boolean = false
    var priority: Int = 0
    var description: String = ""
    var coordinateX: Int = -1
    var coordinateY: Int = -1

    def copy(s: StateGraph): Unit = {
        this.id = s.data.id
        this.name = s.data.stateName
        this.stateType = s.data.stateType
        this.isStartState = s.data.isStartState
        this.priority = s.data.priority
        this.description = s.data.description
        this.coordinateX = s.sx
        this.coordinateY = s.sy
    }
}

//@SerialVersionUID(100L)
case class StateData(id: Int) {
    val ID = id
    var stateName: String = ""
    var stateType: String = ""
    var isStartState = false
    var priority: Int = 0
    var description: String = ""
    var directChildrenTransitionsList: ListBuffer[TransitionData] = ListBuffer()
    var nonDirectChildrenTransitionsList: ListBuffer[TransitionData] = ListBuffer()

    def init(name: String, stype: String, start: Boolean, prio: Int, desc: String): Unit ={
        stateName = name
        stateType = stype
        isStartState = start
        priority = prio
        description = desc
    }

    def copy(msg: RestoredData): Unit = {
        this.stateName = msg.name
        this.stateType = msg.stateType
        this.isStartState = msg.isStartState
        this.priority = msg.priority
        this.description = msg.description
    }

    def setStateType(s: String): Unit = {
        stateType = s
    }

    def setStateName(n: String) = {
        stateName = n
    }

    def addDirectChildrenTransition(tr: TransitionData): Unit = {
        directChildrenTransitionsList += tr
    }

    def removeDirectChildrenTransition(tr: TransitionData): Unit = {
        directChildrenTransitionsList -= tr
    }

    def addNonDirectChildrenTransition(tr: TransitionData) = {
        nonDirectChildrenTransitionsList += tr
    }

    def removeNonDirectChildrenTransition(tr: TransitionData) = {
        nonDirectChildrenTransitionsList -= tr
    }
}

