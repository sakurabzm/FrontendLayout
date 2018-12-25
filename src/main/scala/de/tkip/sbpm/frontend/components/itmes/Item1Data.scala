package de.tkip.sbpm.frontend.components.itmes

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object Item1Data {
  val component =
    ScalaComponent.builder.static("Process")(
      <.div(
        "This is a undefined page !"
      )
    ).build

  def apply() = component().vdomElement
}
