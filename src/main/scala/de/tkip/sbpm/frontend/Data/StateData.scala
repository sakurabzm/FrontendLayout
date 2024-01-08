package de.tkip.sbpm.frontend.Data

import de.tkip.sbpm.frontend.graph.StateGraph
import org.scalajs.dom

import scala.collection.mutable.{ListBuffer,LinkedHashMap}
import scala.scalajs.js

case class StateJsonData(id: Int, stateType: String, isStartState: Boolean, description: String,
                         transitionsList: ListBuffer[TransitionData], xoffset: Int, yoffset: Int)

case class SubjectContent(subjectid: String, stateList: ListBuffer[StateJsonData])

abstract class StateType
case object Action extends StateType
case object Send extends  StateType
case object Receive extends StateType
case object End extends StateType

abstract class FunctionType extends StateType
case object ModalJoin extends FunctionType
case object ModalSplit extends FunctionType
case object CloseAllIPs extends FunctionType
case object OpenAllIPs extends FunctionType
case object CloseIP extends FunctionType
case object OpenIP extends FunctionType
case object IsIPEmpty extends FunctionType
case object CallMacro extends FunctionType
case object VarMan extends FunctionType
case object Tau extends FunctionType
case object Cancel extends FunctionType
case object SelectAgents extends FunctionType

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
//    var stateType1: StateType = null
    var stateType: String = ""
    var isStartState = false
    var priority: Int = 0
    var description: String = ""
    var directChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionData]] = LinkedHashMap() //targetid, list of transition data
    var nonDirectChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionData]] = LinkedHashMap() //targetid, list of transition data

    def init(name: String, stype: String, start: Boolean, prio: Int, desc: String): Unit ={
        stateName = name
        stateType = stype
        isStartState = start
        priority = prio
        description = desc
//        stateType1 = stype match {
//            case "Action" => Action
//            case "Send" => Send
//            case "Receive" => Receive
//            case "End" => End
//            case "ModalSplit" => ModalSplit
//            case "ModalJoin" => ModalJoin
//            case "Tau" => Tau
//            case "CloseIP" => CloseIP
//            case "OpenIP" => OpenIP
//            case "IsIPEmpty" => IsIPEmpty
//            case "CloseAllIPs" => CloseAllIPs
//            case "OpenAllIPs" => OpenAllIPs
//            case "SelectAgents" => SelectAgents
//            case "CallMacro" => CallMacro
//            case "VarMan" => VarMan
//            case "Cancel" => Cancel
//        }
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

    def getStateType: StateType = {
        stateType match {
            case "Action" => Action
            case "Send" => Send
            case "Receive" => Receive
            case "End" => End
            case "ModalSplit" => ModalSplit
            case "ModalJoin" => ModalJoin
            case "Tau" => Tau
            case "CloseIP" => CloseIP
            case "OpenIP" => OpenIP
            case "IsIPEmpty" => IsIPEmpty
            case "CloseAllIPs" => CloseAllIPs
            case "OpenAllIPs" => OpenAllIPs
            case "SelectAgents" => SelectAgents
            case "CallMacro" => CallMacro
            case "VarMan" => VarMan
            case "Cancel" => Cancel
            case _ => null
        }
    }
    def addDirectChildrenTransition(tr: TransitionData): Unit = {
        val sid = tr.target
        if(!directChildrenTransitionsMap.contains(sid)){
            val l = ListBuffer[TransitionData]()
            l += tr
            directChildrenTransitionsMap += (sid -> l)
        }else{
            directChildrenTransitionsMap(sid) += tr
        }
    }

    def removeDirectChildrenTransition(tr: TransitionData): Unit = {
        val sid = tr.target
        if(directChildrenTransitionsMap.contains(sid)){
            if(directChildrenTransitionsMap(sid).size == 1){
                directChildrenTransitionsMap -= sid
            }else{
                directChildrenTransitionsMap(sid) -= tr
            }
        }
    }

    def addNonDirectChildrenTransition(tr: TransitionData): Unit = {
        val sid = tr.target
        if(!nonDirectChildrenTransitionsMap.contains(sid)){
            val l = ListBuffer[TransitionData]()
            l += tr
            nonDirectChildrenTransitionsMap += (sid -> l)
        }else{
            nonDirectChildrenTransitionsMap(sid) += tr
        }
    }

    def removeNonDirectChildrenTransition(tr: TransitionData): Unit = {
        val sid = tr.target
        if(nonDirectChildrenTransitionsMap.contains(sid)){
            if(nonDirectChildrenTransitionsMap(sid).size == 1){
                nonDirectChildrenTransitionsMap -= sid
            }else{
                nonDirectChildrenTransitionsMap(sid) -= tr
            }
        }
    }
}

