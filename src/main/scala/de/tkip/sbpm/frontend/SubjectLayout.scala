package de.tkip.sbpm.frontend

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._

object SubjectLayout {

    final case class Props() {
        @inline def render: VdomElement = Component(this)
    }

    //implicit val reusabilityProps: Reusability[Props] =
    //  Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {
        def render(p: Props): VdomElement =
            <.div
    }

    val Component = ScalaComponent.builder[Props]("")
            .renderBackend[Backend]
            //.configure(Reusability.shouldComponentUpdate)
            .build
}