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
            minHeight(500.px),
            paddingTop(40.px)
        )

    }


    val component =
        ScalaComponent.builder
                .static("HomePage")(
                    <.div(
                        Style.content,
                        <.div(
                            "Welcome to S-BPM"
                        )
                    )
                )
                .build


    def apply() = component()
}
