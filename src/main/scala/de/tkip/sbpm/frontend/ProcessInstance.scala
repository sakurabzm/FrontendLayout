package de.tkip.sbpm.frontend

import japgolly.scalajs.react.Callback
import scala.collection.mutable.{ListBuffer, Map}
import japgolly.scalajs.react.vdom.html_<^.{<, ^, _}
import org.scalajs.dom

/**
  * Created by Wang on 2017/3/22.
  */

case class Subject(id: Int, x: Int, y: Int) {

    val subjectid = id
    val states = ListBuffer[State]()
    val targetLists = ListBuffer[Int]()
    val width = 120
    val height = 240
    var xx = x
    var yy = y
    var colour = "#800"
    var isStartSubject = false

    def subjectConnect(): Unit = {

    }


    def subjectGraph(x: Int, y: Int, color: String): VdomElement = {
        <.div(
            ^.position.absolute,
            ^.width := width.px,
            ^.height := height.px,
            ^.background := "#800",
            ^.left := x.px,
            ^.top := y.px,
            ^.onDoubleClick --> behaviorPage
        )
    }

    def behaviorPage() = Callback {
        println("id: " + subjectid)
        val url = "http://localhost:8080/#subjects/" + subjectid
        dom.window.location.href = url
    }

    def getGraph(): VdomElement = {
        subjectGraph(xx, yy, colour)
    }

    def subjectConnectView(): VdomElement = {
        <.div
    }
}

case class Arrow(sx: Int, sy: Int, tx: Int, ty: Int, deeps: Int) {
    val tip = 16
    val dmultiple = 20
    var color = "#00bff3"

    def leftArrow(x: Int, y: Int, length: Int): VdomElement =
        <.div(
            ^.position.absolute,
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.width := length.px,
                ^.height := 5.px,
                ^.left := x.px,
                ^.top := y.px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(150deg)",
                ^.width := tip.px,
                ^.height := 5.px,
                ^.left := (x + math.cos(math.toRadians(30)) * tip / 2 - tip / 2).px,
                ^.top := (y - math.sin(math.toRadians(30)) * tip / 2).px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(30deg)",
                ^.width := tip.px,
                ^.height := 5.px,
                ^.left := (x + math.cos(math.toRadians(30)) * tip / 2 - tip / 2).px,
                ^.top := (y + math.sin(math.toRadians(30)) * tip / 2).px
            )
        )

    def rightArrow(x: Int, y: Int, length: Int): VdomElement =
        <.div(
            ^.position.absolute,
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.width := length.px,
                ^.height := 5.px,
                ^.left := x.px,
                ^.top := y.px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(30deg)",
                ^.width := tip.px,
                ^.height := 5.px,
                ^.left := (x + length - math.cos(math.toRadians(30)) * tip / 2 - tip / 2).px,
                ^.top := (y - math.sin(math.toRadians(30)) * tip / 2).px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(150deg)",
                ^.width := tip.px,
                ^.height := 5.px,
                ^.left := (x + length - math.cos(math.toRadians(30)) * tip / 2 - tip / 2).px,
                ^.top := (y + math.sin(math.toRadians(30)) * tip / 2).px
            )
        )

    def topArrow(tx: Int, ty: Int, deeps: Int): VdomElement = {
        val length = deeps * dmultiple
        val x = tx - (length + 1) / 2
        val y = ty + (length + 1) / 2
        <.div(
            ^.position.absolute,
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(90deg)",
                ^.width := length.px,
                ^.height := 3.px,
                ^.left := x.px,
                ^.top := y.px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(60deg)",
                ^.width := tip.px,
                ^.height := 3.px,
                ^.left := (x + length / 2 + (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                ^.top := (y - length / 2 + (math.cos(math.toRadians(30)) * tip / 2)).px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(120deg)",
                ^.width := tip.px,
                ^.height := 3.px,
                ^.left := (x + length / 2 - (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                ^.top := (y - length / 2 + (math.cos(math.toRadians(30)) * tip / 2)).px
            )
        )
    }


    def bottomArrow(tx: Int, ty: Int, deeps: Int): VdomElement = {
        val length = deeps * dmultiple
        val x = tx - (length + 1) / 2
        val y = ty - (length + 1) / 2
        <.div(
            ^.position.absolute,
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(90deg)",
                ^.width := length.px,
                ^.height := 3.px,
                ^.left := x.px,
                ^.top := y.px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(120deg)",
                ^.width := tip.px,
                ^.height := 3.px,
                ^.left := (x + length / 2 + (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                ^.top := (y + length / 2 - (math.cos(math.toRadians(30)) * tip / 2)).px
            ),
            <.div(
                ^.position.absolute,
                ^.background := color,
                ^.transform := "rotate(60deg)",
                ^.width := tip.px,
                ^.height := 3.px,
                ^.left := (x + length / 2 - (math.sin(math.toRadians(30)) * tip / 2) - tip / 2).px,
                ^.top := (y + length / 2 - (math.cos(math.toRadians(30)) * tip / 2)).px
            )
        )
    }


    def polyLine(x: Int, y: Int, deeps: Int, isTop: Boolean): VdomElement = {
        val top = if (isTop) (y - (deeps * dmultiple + 1) / 2) else (y + (deeps * dmultiple + 1) / 2)
        dom.console.log("top length...." + x + "  " + top + " deep: " + deeps)
        <.div(
            ^.position.absolute,
            ^.background := color,
            ^.transform := "rotate(90deg)",
            ^.width := (deeps * 20).px,
            ^.height := 3.px,
            ^.left := (x - (deeps * dmultiple + 1) / 2).px,
            ^.top := top.px
        )
    }


    def straightLine(x: Int, y: Int, length: Int, deeps: Int, isTop: Boolean): VdomElement = {
        val top = if (isTop) (y - deeps * dmultiple) else (y + deeps * dmultiple)
        <.div(
            ^.position.absolute,
            ^.background := color,
            ^.width := length.px,
            ^.height := 3.px,
            ^.left := x.px,
            ^.top := top.px
        )
    }


    var arrowLength = Math.abs(tx - sx)

    def showArrow(): VdomElement = {
        dom.console.log("showArrow begin....")
        if (deeps != 0) {
            if (sx > tx) {
                <.div(
                    bottomArrow(tx, ty, deeps),
                    polyLine(sx, sy, deeps, true),
                    straightLine(tx, ty, arrowLength, deeps, true)
                )
            } else {
                <.div(
                    topArrow(tx, ty, deeps),
                    polyLine(sx, sy, deeps, false),
                    straightLine(sx, sy, arrowLength, deeps, false)
                )
            }
        } else {
            if (sx - tx < 0) {
                dom.console.log("Arrow Right....")
                rightArrow(sx, sy, arrowLength)
            } else {
                dom.console.log("Arrow Left....")
                leftArrow(tx, ty, arrowLength)
            }
        }
    }
}

object ProcessInstance {

    var sujectnumber = 0
    val subjects = ListBuffer[Subject]()
    val subjectMap = Map[Int, ListBuffer[State]]()
    val subOutgoingMap = Map[Int, Map[Int, Int]]()
    val subIncomingMap = Map[Int, Map[Int, Int]]()
    val bottomArrowMap = Map[Int, (Int, Int)]()
    val topArrowMap = Map[Int, (Int, Int)]()
    val arrowList = ListBuffer[VdomElement]()
    var startSubjects = ListBuffer[Int]()
    //test
    addSubject()
    addSubject()
    addSubject()
    addSubject()
    addSubject()

    def getStartSubject(): Unit = {
        subjects.map(s => if (s.isStartSubject) startSubjects += s.subjectid)
    }

    def saveSubject(id: Int, stateList: ListBuffer[State]): Unit = {
        subjectMap += (id -> stateList)
        addConnect(id)
    }

    def addSubject(): Unit = {
        val subject = new Subject(sujectnumber, 200 * sujectnumber, 120)
        subjects += subject
        sujectnumber += 1

    }

    def getSubject(sid: Int): Subject = {
        subjects(sid)
    }

    def getSubjects(): ListBuffer[Subject] = {
        subjects.sortBy(_.subjectid)
    }

    def subjectsView(): ListBuffer[VdomElement] = {
        subjects.map(_.getGraph())
    }

    def arrowsView(): ListBuffer[VdomElement] = {
        arrowList.clear()
        connectAll()
        dom.console.info("Arrow List... " + arrowList)
        arrowList
    }

    def arrowTest(): VdomElement = {

        Arrow(30, 30, 50, 30, 2).bottomArrow(30, 20, 1)
        //    <.div(
        //      ^.position.absolute,
        //      ^.width         := 700.px,
        //      ^.height        := 5.px,
        //      ^.left          := 23.px,
        //      ^.top           := 55.px,
        //      ^.background    := "#800"
        //    )
    }

    def loadSubject(id: Int): ListBuffer[State] = {
        if (subjectMap.contains(id)) {
            subjectMap(id)
        } else {
            ListBuffer[State]()
        }
    }

    /*
    确定subject之间的连接，一个出箭头数组，一个入箭头数组。
     */
    def addConnect(sid: Int): Unit = {
        dom.console.info("connect map start...")
        val soMap: Map[Int, Int] = Map()
        val siMap: Map[Int, Int] = Map()
        for (state <- subjectMap(sid)) {
            state.outTransitions.filterNot(t => t.stosMsg.subid == sid).foreach(
                t => {
                    if (soMap.contains(t.stosMsg.subid))
                        soMap(t.stosMsg.subid) += 1
                    else
                        soMap += (t.stosMsg.subid -> 1)
                    if (sid > t.stosMsg.subid) {
                        if (topArrowMap.contains(t.stosMsg.subid))
                            topArrowMap += t.stosMsg.subid -> (topArrowMap(t.stosMsg.subid)._1 + 1, 0)
                        else
                            topArrowMap += t.stosMsg.subid -> (1, 0)
                    } else if (sid < t.stosMsg.subid) {
                        if (bottomArrowMap.contains(t.stosMsg.subid))
                            bottomArrowMap += t.stosMsg.subid -> (bottomArrowMap(t.stosMsg.subid)._1 + 1, 0)
                        else
                            bottomArrowMap += t.stosMsg.subid -> (1, 0)
                    }
                }
            )
        }
        subOutgoingMap += (sid -> soMap)
        dom.console.info("connect map " + subOutgoingMap)

        //    var lastkey : Int = subOutgoingMap.keys.toSeq.sortBy(_).head
        //    val soMap2 : Map[Int, ListBuffer[(Int, Int)]] = Map()
        //    for(key <- subOutgoingMap.keys.toSeq.sortBy(_)){
        //      if(sid > key){
        //        val listb : ListBuffer[(Int, Int)] = new ListBuffer[(Int, Int)]()
        //        for(i <- 1 to soMap(lastkey)){
        //          val sub = subjects.find(s => lastkey == s.sid).get
        //          val xy = ((sub.xx + sub.width), (sub.yy + sub.height/2/(i+1) * i))
        //          listb += xy
        //        }
        //        soMap2 += lastkey -> listb
        //        if(key lastkey){
        //
        //        }
        //      }
        //    }

        //    for(i <- subOutgoingMap.keys){
        //      for(key <- subOutgoingMap(i).keys){
        //        if(key == sid){
        //          siMap += (i -> subOutgoingMap(i)(key))
        //        }
        //      }
        //    }
        //    subIncomingMap += (sid -> siMap)
        //    for(i <- subIncomingMap.keys){
        //      val lessTo = subIncomingMap(i).filterKeys(k => k < i).values.sum
        //      val moreTo = subIncomingMap(i).filterKeys(k => k > i).values.sum
        //      targetArrowMap += (i -> Map(lessKey -> (lessTo, 0)))
        //      targetArrowMap += (i -> Map(moreKey -> (moreTo, 0)))
        //    }
    }

    def connectAll(): Unit = {
        /*
        箭头连接，subOGMapMirror是看箭头指向的subject和指向的数量
         */
        val subjectsMirror = subjects.clone()
        val subOGMapMirror = subOutgoingMap.clone()
        var lastSub = subjectsMirror.head
        subjectsMirror -= subjectsMirror.head
        dom.console.info("connect begin... OGMap" + subOGMapMirror)
        for (sub <- subjectsMirror) {
            dom.console.info("connect subid." + sub.subjectid)
            if (subOGMapMirror.contains(lastSub.subjectid)) {
                val bottomRight = subOGMapMirror(lastSub.subjectid).filterKeys(k => k > lastSub.subjectid).values.sum
                val topLeft = subOGMapMirror(lastSub.subjectid).filterKeys(k => k < lastSub.subjectid).values.sum
                var bottomCount = 0
                var topCount = 0
                dom.console.info("connect id " + bottomRight)
                for (target <- subOGMapMirror(lastSub.subjectid).keys.toSeq.sorted) {

                    val tarSub = subjects.find(s => s.subjectid == target).get
                    for (i <- 1 to subOGMapMirror(lastSub.subjectid)(target)) {
                        if (target == sub.subjectid) {
                            dom.console.info("arrow left " + lastSub.yy + "   " + tarSub.yy)
                            arrowList += Arrow(lastSub.xx + lastSub.width, (lastSub.yy + lastSub.height / 2 / (subOGMapMirror(lastSub.subjectid)(target) + 1) * i),
                                sub.xx, (sub.yy + sub.height / 2 / (subOGMapMirror(lastSub.subjectid)(target) + 1) * i), 0).showArrow()
                        } else if (target > lastSub.subjectid) {
                            bottomArrowMap += target -> (bottomArrowMap(target)._1, bottomArrowMap(target)._2 + 1)
                            bottomCount += 1
                            arrowList += Arrow(lastSub.xx + (lastSub.width / 2) + (lastSub.width / 2 / (bottomRight + 1)) * bottomCount, lastSub.yy + lastSub.height,
                                tarSub.xx + (tarSub.width / 2 / (bottomArrowMap(target)._1 + 1)) * bottomArrowMap(target)._2, tarSub.yy + tarSub.height, bottomCount).showArrow()
                        } else {
                            topArrowMap += target -> (topArrowMap(target)._1, topArrowMap(target)._2 + 1)
                            topCount += 1
                            arrowList += Arrow(lastSub.xx + (lastSub.width / 2 / (topLeft + 1)) * topCount, lastSub.yy,
                                tarSub.xx + tarSub.width - (tarSub.width / 2 / (topArrowMap(target)._1 + 1)) * topArrowMap(target)._2, tarSub.yy, topCount).showArrow()
                        }
                    }
                }
            }
            if (subOGMapMirror.contains(sub.subjectid)) {
                if (subOGMapMirror(sub.subjectid).contains(lastSub.subjectid)) {
                    for (i <- 1 to subOGMapMirror(sub.subjectid)(lastSub.subjectid)) {
                        arrowList += Arrow(sub.xx, (sub.yy + lastSub.height / 2 + lastSub.height / 2 / (subOGMapMirror(sub.subjectid)(lastSub.subjectid) + 1) * i),
                            lastSub.xx + lastSub.width, (lastSub.yy + lastSub.height / 2 + lastSub.height / 2 / (subOGMapMirror(sub.subjectid)(lastSub.subjectid) + 1) * i), 0).showArrow()
                    }
                    subOGMapMirror(sub.subjectid).remove(lastSub.subjectid)
                    dom.console.info("removed OGMap " + subOGMapMirror)
                }
            }
            if (sub.equals(subjectsMirror.last)) {
                if (subOGMapMirror.contains(sub.subjectid)) {
                    val topLeft = subOGMapMirror(sub.subjectid).values.sum
                    var topCount = 0
                    for (target <- subOGMapMirror(sub.subjectid).keys.toSeq.sorted) {
                        val tarSub = subjects.find(s => s.subjectid == target).get
                        for (i <- 1 to subOGMapMirror(sub.subjectid)(target)) {
                            topArrowMap += target -> (topArrowMap(target)._1, topArrowMap(target)._2 + 1)
                            topCount += 1
                            arrowList += Arrow(sub.xx + (sub.width / 2 / (topLeft + 1)) * topCount, sub.yy,
                                tarSub.xx + tarSub.width - (tarSub.width / 2 / (topArrowMap(target)._1 + 1)) * topArrowMap(target)._2, tarSub.yy, topCount).showArrow()
                        }
                    }
                }
            }

            lastSub = sub
        }
    }

    def addOutgoing(id: Int): Map[Int, Int] = {
        val transOutgoingMap: Map[Int, Int] = Map()
        if (subjectMap.contains(id)) {
            val subjects = subjectMap(id)
            for (s <- subjects) {
                s.outTransitions.foreach(t => if (transOutgoingMap.contains(t.targetid)) transOutgoingMap(t.targetid) += 1 else transOutgoingMap += (t.targetid -> 0))
            }
        }
        transOutgoingMap
    }


}
