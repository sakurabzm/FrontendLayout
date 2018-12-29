package de.tkip.sbpm.frontend.Data

import scala.collection.mutable.ListBuffer

class StateData(id: Int) {
  val ID = id
  var stateType: String = ""
  var isStartState = false
  var description: String = ""
  val transitionsList: ListBuffer[TransitionData] = ListBuffer()

  def setStateType(s: String): Unit ={
    stateType = s
  }

  def addTransition(tr: TransitionData): Unit ={
    transitionsList += tr
  }

  def removeTransition(tr: TransitionData): Unit ={
    transitionsList -= tr
  }
}
