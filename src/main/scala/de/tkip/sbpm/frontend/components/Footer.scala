package de.tkip.sbpm.frontend.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object Footer {

    val component = ScalaComponent.builder
            .static("Footer")(
                <.footer(
                    ^.position.relative,
                    ^.width := "100%",
                    ^.textAlign.center,
                    <.div(^.borderTop := "1px solid grey", ^.padding := "0px"),
                    <.p(^.paddingTop := "5px", " footer S-BPM template test")
                )
            )
            .build

    def apply() = component()
}