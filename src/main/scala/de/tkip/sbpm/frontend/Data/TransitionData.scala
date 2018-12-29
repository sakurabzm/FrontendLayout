package de.tkip.sbpm.frontend.Data

class TransitionData(s: StateData, t: StateData) {
  var source = s
  var target = t
  var messageType = ""
}
