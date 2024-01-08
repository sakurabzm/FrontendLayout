package de.tkip.sbpm.frontend.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object Footer {

    val component = ScalaComponent.builder
            .static("Footer")(
                <.footer(
                    ^.position.relative,
                    ^.width := "100%",
//                    ^.marginTop := "-60px",
                    ^.height := "40px",
//                    ^.clear := "both",
                    ^.textAlign.center,
                    <.div(^.borderTop := "1px solid grey", ^.padding := "0px"),
                    <.p(^.paddingTop := "2px", " footer1 S-BPM template")
                )
            )
            .build

    def apply() = component()
}