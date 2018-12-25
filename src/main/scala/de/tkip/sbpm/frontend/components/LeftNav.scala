package de.tkip.sbpm.frontend.components

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import de.tkip.sbpm.frontend.Item

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._

object LeftNav {

  object Style extends StyleSheet.Inline {

    import dsl._

    val nav =
      style(width :=! 205.px, borderRight :=! "1px solid rgb(223, 220, 200)", backgroundColor :=! c"#E6E6E6")

    val container = style(display.flex,
      flexDirection.column,
      listStyle := "none",
      padding.`0`)

    val menuItem = styleF.bool { selected =>
      styleS(
        lineHeight(50.px),
        padding :=! "0 25px",
        cursor.pointer,
        mixinIfElse(selected)(backgroundColor(c"#8DB6CD"), fontWeight._500)(
          &.hover(backgroundColor(c"#B0C4DE"))
        )
      )
    }
  }

  case class Props(menus: Vector[Item],
                   selectedPage: Item,
                   ctrl: RouterCtl[Item])

  implicit val currentPageReuse = Reusability.by_==[Item]
  implicit val propsReuse = Reusability.by((_: Props).selectedPage)

  val component = ScalaComponent
    .builder[Props]("LeftNav")
    .render_P { P =>
      Style.container
      <.ul(
        Style.container,
        P.menus.toTagMod(
          item =>
            <.li(^.key := item.title,
              Style.menuItem(item == P.selectedPage),
              item.title,
              ^.fontSize := 18.px,
              P.ctrl setOnClick item)
        )

      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def apply(props: Props) = component(props)

}
