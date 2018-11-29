package de.tkip.sbpm.frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object DocoPage {

  val component = ScalaComponent.static("process")(
    <.p(
      ^.marginTop := "1em",
      ^.fontSize := "110%",
      ^.color := "#292929",
      "process page test ",
      "..."))

}
