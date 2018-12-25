package de.tkip.sbpm.frontend.components.itmes

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object ItemsInfo {
  val component = ScalaComponent.builder
    .static("ItemsInfo")(
      <.div(
        " Read me !",
        ^.backgroundColor := "pink",
        ^.width := "100%",
        <.ul(
          <.li(
            "How to create a process"
          ),
          <.li(
            "Condition"
          )
        )
      )
    )
    .build

  def apply() = component().vdomElement
}
