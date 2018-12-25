package de.tkip.sbpm.frontend

import de.tkip.sbpm.frontend.components.itmes.{Item1Data, ProcessList, ItemsInfo}
import de.tkip.sbpm.frontend.pages.ItemsPage
import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.vdom.VdomElement

sealed abstract class Item(val title: String,
                           val routerPath: String,
                           val render: () => VdomElement)

object Item {

  case object Info extends Item("Info", "info", () => ItemsInfo())

  case object Item1 extends Item("undefined", "undefined", () => Item1Data())

  case object Process extends Item("Process", "Process", () => ProcessList())

  val menu = Vector(Info, Item1, Process)

  val routes = RouterConfigDsl[Item].buildRule { dsl =>
    import dsl._
    menu
      .map { i =>
        staticRoute(i.routerPath, i) ~> renderR(
          r => ItemsPage(ItemsPage.Props(i, r)))
      }
      .reduce(_ | _)
  }
}