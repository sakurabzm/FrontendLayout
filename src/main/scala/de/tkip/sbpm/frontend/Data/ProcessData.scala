package de.tkip.sbpm.frontend.Data

import japgolly.scalajs.react.vdom.html_<^.VdomElement

import scala.collection.mutable.{ListBuffer, Map}

class ProcessData(pid: String, pauthor: String, pname: String, pdescriptrion: String) {
    val id = pid
    var authorOfProcess = pauthor
    var nameOfProcess = pname
    var descriptionOfProcess = pdescriptrion
    val subjectMap: Map[String, SubjectData] = Map()
    val subjectList: ListBuffer[SubjectData] = ListBuffer()
    val interactionRelationship: ListBuffer[VdomElement] = ListBuffer()

    def addSubject(data: SubjectData): Unit = {
        subjectMap += (data.id.toString -> data)
        subjectList += data
    }

    //  def addSubjectGraph(id: String, graph: SubjectGraph): Unit = {
    //    subjectGraphList += (id -> graph)
    //  }

    def setAuthor(a: String) = {
        authorOfProcess = a
    }

    def setName(n: String) = {
        nameOfProcess = n
    }

    def setDescription(d: String) = {
        descriptionOfProcess = d
    }
}
