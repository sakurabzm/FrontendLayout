package de.tkip.sbpm.frontend

import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation._
/**
  * Created by Wang on 2017/3/22.
  */
object AppRouter extends JSApp  {


  sealed trait AppPage

  case object Home extends AppPage
  case class SubjectPages(id : Int) extends AppPage
  case class BehaviorPage(subjectid: String) extends AppPage
  case object Process extends AppPage


  val config = RouterConfigDsl[AppPage].buildConfig { dsl =>
    import dsl._


//    val subjectRoutes : Rule = Subjects.routes.prefixPath_/("subject").pmap[AppPage](SubjectPages){ case SubjectPages(id) => id}
    (emptyRule
      | staticRoute(root, Home) ~> render(HomePage.component(""))
      | staticRoute("#test", Process) ~> render(LayoutTest.component())
      | staticRoute("#subjects", Home) ~> render(HomePage.component(""))
      |dynamicRouteCT("#subjects" / int.caseClass[SubjectPages]) ~> dynRender(SubjectViewPage.component(_))
      ).notFound(redirectToPage(Home)(Redirect.Replace))
  }
  val baseUrl = BaseUrl.fromWindowOrigin_/

  def main(): Unit = {

    val container = dom.document.getElementById("root")
    dom.console.info("Router logging is enabled. Enjoy!!")
    val router = Router(baseUrl, config.logToConsole)
//    router() renderIntoDOM(dom.document.body)
    router() renderIntoDOM container
  }

}
