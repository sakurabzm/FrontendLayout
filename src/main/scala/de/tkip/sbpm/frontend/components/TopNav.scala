package de.tkip.sbpm.frontend.components

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import de.tkip.sbpm.frontend.models.Menu
import de.tkip.sbpm.frontend.AppRouter.AppPage
//import japgolly.scalajs.react.vdom.all._

object TopNav {

  var message = "ProcessName..."

  object Style extends StyleSheet.Inline {
    import dsl._

    val navMenu = style(
      //position.relative,
      display.flex, // 导航样式
      //width :=! "100%",
      alignItems.center,
      backgroundColor(c"#E6E6E6"),
      margin.`0`,
      listStyle := "none")

    val menuItem = styleF.bool { selected => // 导航中每一项的样式
      styleS(
        padding(20.px),
        fontSize(1.5.em),
        cursor.pointer,
        color(c"rgb(0, 0, 0)"), // fontColor
        mixinIfElse(selected)(backgroundColor(c"#8DB6CD"), fontWeight._500)(
          &.hover(backgroundColor(c"#B0C4DE")))
      )
    }
  }

  case class Props(menus: Vector[Menu],
                   selectedPage: AppPage,
                   ctrl: RouterCtl[AppPage])

  implicit val currentPageReuse = Reusability.by_==[AppPage]
  implicit val propsReuse = Reusability.by((_: Props).selectedPage)

  val component = ScalaComponent
    .builder[Props]("TopNav")
    .render_P { P =>
      <.header(
        <.nav(
          <.ul(
            Style.navMenu,
            <.p(
              "S-BPM",
              ^.fontSize := "1.5em"),
            P.menus.toTagMod { item =>
              <.li(
                ^.key := item.name,
                Style.menuItem(item.route.getClass == P.selectedPage.getClass),
                item.name,
                P.ctrl setOnClick item.route
              )
            }
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build


  def apply(props: Props) = component(props)
}
