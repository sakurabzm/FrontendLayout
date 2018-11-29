package de.tkip.sbpm.frontend

/**
  * Created by Wang on 2017/3/22.
  */

import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.vdom.html_<^.VdomElement


abstract class Subjects(val id: String,
                           val routerPath: String,
                           val render: () => VdomElement)


object Subjects {



//  val subjects: Vector[Subjects]
//  def default: Subjects = subjects.head
//
//  val routes = RouterConfigDsl[Subjects].buildRule { dsl =>
//    import dsl._
//    subjects.map(i =>
//      staticRoute(i.routerPath, i) ~> renderR(r => component("subjects"))
//    ).reduce(_ | _)


//  }


}
