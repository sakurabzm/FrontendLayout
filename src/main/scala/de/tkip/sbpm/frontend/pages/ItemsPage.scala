package de.tkip.sbpm.frontend.pages
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import de.tkip.sbpm.frontend.components.LeftNav
import de.tkip.sbpm.frontend.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object ItemsPage {

  object Style extends StyleSheet.Inline {
    import dsl._
    val container = style(display.flex, minHeight(600.px), borderTop :=! "1px solid rgb(223, 220, 200)") // body of item

    val nav =
      style(width :=! 205.px, borderRight :=! "1px solid rgb(223, 220, 200)", backgroundColor :=! c"#E6E6E6" ) // LeftNav Setting

    val content = style(padding(30.px)) // content of container
  }

  val component = ScalaComponent
    .builder[Props]("ItemsPage")
    .render_P { P =>
      <.div(
        Style.container,
        <.div(Style.nav,
          LeftNav(LeftNav.Props(Item.menu, P.selectedPage, P.ctrl))),
        <.div(Style.content, P.selectedPage.render())
      )
    }
    .build

  case class Props(selectedPage: Item, ctrl: RouterCtl[Item])

  def apply(props: Props) = component(props).vdomElement
}
