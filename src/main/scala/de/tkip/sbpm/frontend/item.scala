package de.tkip.sbpm.frontend

import de.tkip.sbpm.frontend.pages.{ItemsPage, ListProcesses}
import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.vdom.VdomElement

sealed abstract class Item(val title: String,
                           val routerPath: String,
                           val render: () => VdomElement)

object Item {

    case object Process extends Item("Process", "Process", () => ListProcesses())

    val menu = Vector(Process)

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