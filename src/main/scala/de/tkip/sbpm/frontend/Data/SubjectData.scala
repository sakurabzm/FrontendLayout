package de.tkip.sbpm.frontend.Data

import de.tkip.sbpm.frontend.graph.GraphObject.Arrow
import de.tkip.sbpm.frontend.graph.{Graph, StateGraph}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set


case class Point() {
    var pointType: Int = 0 // start or end
    var pointPosition: String = "" // up bottom left right
    var pointAvailable: Boolean = true // is available
    var pointX: Int = 0
    var pointY: Int = 0
    var order: Int = 0 // the order of points
}

case class RecordSubjectData() {
    var name: String = ""
    var startSubject: Boolean = false
    var subjectType: String = ""
    var inputPool: Int = -1
    var num: Int = -1
    var description: String = ""
    var isMultiSubject: Boolean = false

    def copy(s: SubjectData): Unit = {
        this.name = s.name
        this.subjectType = s.subjectType
        this.startSubject = s.startSubject
        this.inputPool = s.inputPool
        this.num = s.num
        this.description = s.description
        this.isMultiSubject = s.isMultiSubject
    }
}

class SubjectData(val id: Int, var name: String, var startSubject: Boolean, var subjectType: String, var inputPool: Int, var num: Int, var description: String) {
    var subjectX: Int = 0
    var subjectY: Int = 0
    val stateList: ListBuffer[StateGraph] = ListBuffer()
    val stateMap: Map[Int, StateGraph] = Map()
    // arrowMap == transitionMap
    val arrowMap: Map[Int, Arrow] = Map()
    var isMultiSubject = false
    val callMacroMap: Map[Int, ListBuffer[StateGraph]] = Map()

    val startStateList: ListBuffer[Int] = ListBuffer()
    val exitPoints: Map[String, ListBuffer[Point]] = Map()
    val entryPoint: Map[String, ListBuffer[Point]] = Map()

    def addState(state: StateGraph): Unit ={
        stateList += state
        stateMap += (state.data.id -> state)
    }
    def setSubjectName(n: String): Unit = this.name = n

    def setStartSubject(ss: Boolean) = this.startSubject = ss

    def setInputPool(i: Int) = this.inputPool = i

    def setSubjectType(t: String) = this.subjectType = t

    def setNum(n: Int) = this.num = n

    def setDescription(d: String) = this.description = d


    def setSubjectY(y: Int) = {
        subjectY = y
    }

    def setSubjectX(x: Int): Unit = {
        subjectX = x
    }

    def getAvailablePoint(direction: String): Point = {
        var returnPoint: Point = null
        val num = entryPoint(direction).size
        var flag = true
        if ((direction == "up") || (direction == "bottom")) {
            val temporaryList = entryPoint(direction).reverse // 反向
            for (i <- 0 until num if flag) {
                if (temporaryList(i).pointAvailable) {
                    returnPoint = temporaryList(i)
                    entryPoint(direction)(num - 1 - i).pointAvailable = false
                    flag = false
                }
            }
        } else if (direction == "right") { // 左右箭头只有一个
            returnPoint = entryPoint("left").head
            entryPoint("left").head.pointAvailable = false
        } else {
            returnPoint = entryPoint("right").head
            entryPoint("right").head.pointAvailable = false
        }
        returnPoint
    }

    def copy(data: RecordSubjectData) = {
        this.name = data.name
        this.startSubject = data.startSubject
        this.subjectType = data.subjectType
        this.inputPool = data.inputPool
        this.num = data.num
        this.description = data.description
        this.isMultiSubject = data.isMultiSubject
    }
}
