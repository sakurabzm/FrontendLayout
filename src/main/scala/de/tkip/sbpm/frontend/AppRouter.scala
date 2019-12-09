package de.tkip.sbpm.frontend

import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import org.scalajs.dom
import de.tkip.sbpm.frontend.models.Menu
import de.tkip.sbpm.frontend.components.{Footer, TopNav}
import de.tkip.sbpm.frontend.pages.{InternalBehaviorPage, ItemsPage, SubjectsV, TemplateTest}
import de.tkip.sbpm.frontend.css.AppCSS

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation._

/**
  * Created by Wang on 2017/3/22.
  */
object AppRouter extends JSApp {


    sealed trait AppPage

    case object Home extends AppPage

    case class SubjectPages(id: Int) extends AppPage

    //case class BehaviorPage(processid: String, subjectid: String) extends AppPage
    case class BehaviorPage(processid: String, subjectid: String, callMarco: Int = -1) extends AppPage

    case object Process extends AppPage

    case object TemplateTestE extends AppPage

    case class Items(p: Item) extends AppPage

    case class SubjectsVP(processid: String) extends AppPage

    case object InternalBH extends AppPage


    val config = RouterConfigDsl[AppPage].buildConfig { dsl =>
        import dsl._
        val itemRoutes: Rule =
            Item.routes.prefixPath_/("#items").pmap[AppPage](Items) {
                case Items(p) => p
            }
        //    (trimSlashes
        //      | staticRoute(root, Home) ~> render(HomePage())
        //      | itemRoutes)
        //      .notFound(redirectToPage(Home)(Redirect.Replace))
        //      .renderWith(layout)

        //    val subjectRoutes : Rule = Subjects.routes.prefixPath_/("subject").pmap[AppPage](SubjectPages){ case SubjectPages(id) => id}
        (emptyRule
                | staticRoute(root, Home) ~> render(TemplateTest.component())
                | staticRoute("#test", Process) ~> render(LayoutTest.component())
                | staticRoute("#subjects", Home) ~> render(HomePage.component(""))
                | staticRoute("#templatetest", TemplateTestE) ~> render(TemplateTest.component()) // HU
                | dynamicRouteCT("#subjectsView" / string("[a-zA-Z0-9]+").caseClass[SubjectsVP]) ~> dynRender(SubjectsV.component(_)) // HU
                | dynamicRouteCT("#subjectsView" / (string("[a-zA-Z0-9]+") / string("[a-zA-Z0-9]+") / int).caseClass[BehaviorPage]) ~> dynRender(InternalBehaviorPage.component(_)) // HU
                //| dynamicRouteCT("#subjectsView" / (string("[a-zA-Z0-9]+") / string("[a-zA-Z0-9]+")).caseClass[BehaviorPage]) ~> dynRender(InternalBehaviorPage.component(_)) // HU
                // | dynamicRouteCT("#subjects" / int.caseClass[SubjectPages]) ~> dynRender(SubjectViewPage.component(_))
                | itemRoutes
                ).notFound(redirectToPage(Home)(Redirect.Replace))
                .renderWith(layout _)
    }
    val baseUrl = BaseUrl.fromWindowOrigin_/

    val mainMenu = Vector(

        Menu("Home", TemplateTestE),
        Menu("Items", Items(Item.Process))
    )

    def layout(c: RouterCtl[AppPage], r: Resolution[AppPage]) =
        <.div(
            TopNav(TopNav.Props(mainMenu, r.page, c)),
            r.render(),
            Footer()
        )

    def main(): Unit = {
        AppCSS.load
        val container = dom.document.getElementById("root")
        dom.console.info("Router logging is enabled. Enjoy!!")
        val router = Router(baseUrl, config.logToConsole)
        //    router() renderIntoDOM(dom.document.body)
        router() renderIntoDOM container
    }

}
