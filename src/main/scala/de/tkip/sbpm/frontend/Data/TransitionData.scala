package de.tkip.sbpm.frontend.Data

import org.scalajs.dom

import scala.collection.mutable.ListBuffer

case class RestoreTransitionData() {
    var id: Int = -1
    var transitionType: String = ""
    var priority: Int = -1
    var timeout: Int = -1
    var description: String = ""
    var agentInformation: String = ""
    var activatedStateIDInformation: Int = -1
    var exchangedInformation: ExchangedInformation = ExchangedInformation()
    var inputPoolInformation: InputPoolData = InputPoolData()
    var variableInformation: VariableData = VariableData()

    def copy(data: TransitionData): Unit = {
        this.transitionType = data.transitionType
        this.priority = data.priority
        this.timeout = data.timeout
        this.description = data.label
        this.agentInformation = data.agent
        this.activatedStateIDInformation = data.activatedStateID
        this.exchangedInformation.copy(data.information)
        this.inputPoolInformation.copy(data.inputPoolOperationObject)
        this.variableInformation.copy(data.variableOperation)
    }

}

case class ExchangedInformation() {
    var actionType: String = ""
    var relatedSubjectName: String = ""
    var relatedMessageType: String = ""
//    var relatedSubjectId: Int = 0
    var sendingNum: Int = 1

    def setActionType(a: String): Unit = {
        actionType = a
    }

    def addRelatedSubjectName(sub: String): Unit = {
        relatedSubjectName = sub
    }

    def addRelatedMessageType(msg: String): Unit = {
        relatedMessageType = msg
    }

    def setRelatedSubjectName(sub: String): Unit = {
        relatedSubjectName = sub
    }

    def setRelatedMessageType(msg: String): Unit = {
        relatedMessageType = msg
    }

//    def setRelatedSubjectId(id: Int): Unit = {
//        relatedSubjectId = id
//    }

    def setNum(n: Int) = {
        sendingNum = n
    }

    def resetMessage: Unit = {
        this.actionType = ""
        this.relatedSubjectName = ""
        this.relatedMessageType = ""
//        this.relatedSubjectId = 0
        this.sendingNum = 1
    }

    def copy(data: ExchangedInformation): Unit = {
        this.actionType = data.actionType
        this.relatedSubjectName = data.relatedSubjectName
        this.relatedMessageType = data.relatedMessageType
//        this.relatedSubjectId = data.relatedSubjectId
        this.sendingNum = data.sendingNum
    }

}

case class InputPoolData() {
    var messageType: String = ""
    var subjectName: String = ""

    def setRelatedMessageType(msg: String): Unit = {
        messageType = msg
    }

    def setRelatedSubjectName(subName: String) = {
        subjectName = subName
    }

    def resetInputPoolData: Unit = {
        this.messageType = ""
        this.subjectName = ""
    }

    def copy(ipData: InputPoolData): Unit = {
        this.messageType = ipData.messageType
        this.subjectName = ipData.subjectName
    }
}

case class VariableData() {
    var v1: String = ""
    var v2: String = ""
    var operation: String = ""
    var result: String = ""

    def setV1(v: String): Unit = {
        v1 = v
    }

    def setV2(v: String): Unit = {
        v2 = v
    }

    def setOperation(op: String): Unit = {
        operation = op
    }

    def setResult(r: String): Unit = {
        result = r
    }

    def resetVariableData: Unit = {
        this.v1 = ""
        this.v2 = ""
        this.operation = ""
        this.result = ""
    }

    def copy(vData: VariableData): Unit = {
        this.v1 = vData.v1
        this.v2 = vData.v2
        this.operation = vData.operation
        this.result = vData.result
    }
}


case class TransitionData(id: Int, s: Int, t: Int) {
    val ID = id
    var transitionType = "normal"
    var priority: Int = 0
    var label = ""
    var source = s
    var target = t
    var timeout = 0
    var information: ExchangedInformation = ExchangedInformation()
    var inputPoolOperationObject: InputPoolData = InputPoolData()
    var variableOperation: VariableData = VariableData()
    var agent: String = ""
    var activatedStateID: Int = 0
    var repeatTimes = -2
    var multiTransitions = false

    def init(ttype: String, prio: Int, desc: String, timeo: Int): Unit ={
        transitionType = ttype
        priority = prio
        label = desc
        timeout = timeo
    }

    def resetTransition: Unit = {
        this.transitionType = ""
        this.label = ""
        this.priority = 0
        this.timeout = 0
        this.information.resetMessage
        this.inputPoolOperationObject.resetInputPoolData
        this.variableOperation.resetVariableData
        this.agent = ""
        this.activatedStateID = 0
    }

    def setTransitionType(s: String) = {
        transitionType = s
    }

    def setPriority(p: Int) = {
        priority = p
    }

    def setDescription(s: String) = {
        label = s
    }

    def setTimeout(t: Int) = {
        timeout = t
    }

    def setActivatedStateID(t: Int) = {
        activatedStateID = t
    }

    def setMultiTransitions(t: Boolean): Unit ={
        multiTransitions = t
    }

    def copy(data: RestoreTransitionData): Unit = {
        this.transitionType = data.transitionType
        this.priority = data.priority
        this.timeout = data.timeout
        this.label = data.description
        this.agent = data.agentInformation
        this.activatedStateID = data.activatedStateIDInformation
        this.information.copy(data.exchangedInformation)
        this.inputPoolOperationObject.copy(data.inputPoolInformation)
        this.variableOperation.copy(data.variableInformation)
    }

    override def equals(o: Any) = o match {
        case that: TransitionData => that.id == this.id
        case _ => false
    }
}
