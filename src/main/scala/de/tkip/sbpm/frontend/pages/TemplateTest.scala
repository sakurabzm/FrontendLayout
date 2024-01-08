package de.tkip.sbpm.frontend.pages

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
import scala.util.Random

object TemplateTest {

    object Style extends StyleSheet.Inline {

        import dsl._

        val content = style(
            textAlign.center,
            width :=! "100%",
            fontSize(30.px),
//            minHeight(500.px),
            paddingTop(40.px)
        )

    }


    val component =
        ScalaComponent.builder
                .static("HomePage")(
                    <.div(
                        Style.content,
//                        ^.minHeight := "100vh",
                        ^.height := "auto",
                        ^.height := "100%",
                        ^.flex  := "1",
                        <.div(
//                            ^.minHeight := "80vh",
                            "Welcome to S-BPM"
                        ),
                        <.footer(
                            ^.position.absolute,
                            ^.width := "100%",
//                            ^.marginTop := "-60px",
                            ^.height := "60px",
//                            ^.clear := "both",
                            ^.textAlign.center,
                            <.div(^.borderTop := "1px solid grey", ^.padding := "0px"),
                            <.p(^.paddingTop := "2px", " footer2 S-BPM template")
                        )
                    )
                )
                .build


    def apply() = component()
}
