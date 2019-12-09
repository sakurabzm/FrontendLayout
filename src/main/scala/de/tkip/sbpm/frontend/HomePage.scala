package de.tkip.sbpm.frontend

import org.scalajs.dom.html
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterConfigDsl
import japgolly.scalajs.react.extra.{Reusability, StateSnapshot}
import japgolly.scalajs.react.vdom.html_<^.{<, _}
import de.tkip.sbpm.frontend.AppRouter._

import scala.scalajs.js
import org.scalajs.dom

import js.annotation._
import scala.collection.mutable.ListBuffer

/**
  * Created by Wang on 2017/3/23.
  */


object HomePage {


    val subjects = ListBuffer[Subject]()
    var subjectsnumber = 3

    val component = ScalaComponent.builder[String]("HomePage").render { i =>
        dom.console.info("LOADED 113")
        <.div(
            <.h1(s"SBPM Frondend Test Page"),
            <.br,
            Main()
        )
    }.build

    import org.scalajs.dom.ext.KeyCode

    val OuterX = 1280
    val OuterY = 500
    val InnerX = 80
    val InnerY = 150
    val MoveDist = 24


    val OuterDiv =
        <.div(
            ^.position.relative,
            ^.tabIndex := 0,
            ^.width := OuterX.px,
            ^.height := OuterY.px,
            ^.border := "solid 1px #333",
            ^.background := "#ddd")

    val InnerDiv =
        <.div(
            ^.position.absolute,
            ^.width := InnerX.px,
            ^.height := InnerY.px,
            ^.background := "#800")


    class Backend($: BackendScope[Unit, Int]) {

        def addSubject(e: ReactEventFromInput) = {
            subjectsnumber += 1
            e.preventDefaultCB >>
                    $.modState(_ + subjectsnumber)
        }


        private var outerRef = Ref[html.Element]

        def init: Callback =
            outerRef.foreach(_.focus())

        def subjectsview(): ListBuffer[VdomElement] = {
            for (sub <- ProcessInstance.getSubjects())
                yield sub.getGraph()
        }


        def render(i: Int) = {
            <.div(
                <.button("Add Subject", ^.onClick ==> addSubject),
                <.br,
                OuterDiv.withRef(outerRef)(
                    ProcessInstance.arrowsView().toVdomArray,
                    //          ProcessInstance.arrowTest(),
                    subjectsview().toVdomArray
                )
            )
        }
    }


    val Main = ScalaComponent.builder[Unit]("CallbackOption example")
            .initialState(subjectsnumber)
            .renderBackend[Backend]
            .componentDidMount(_.backend.init)
            .build

    // EXAMPLE:END
}

