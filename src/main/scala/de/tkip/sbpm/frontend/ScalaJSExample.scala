//package de.tkip.sbpm.frontend
//
//
//import scala.scalajs.js.annotation.JSExport
//import org.scalajs.dom
//import org.scalajs.dom.html
//
//import scala.util.Random
//
//case class Point(x: Int, y: Int){
//  def +(p: Point) = Point(x + p.x, y + p.y)
//  def /(d: Int) = Point(x / d, y / d)
//}
//
//@JSExport
//object ScalaJSExample {
//  @JSExport
//  def main(canvas: html.Canvas): Unit = {
//    val ctx = canvas.getContext("2d")
//      .asInstanceOf[dom.CanvasRenderingContext2D]
//
//    ctx.fillStyle = "rgb(0,0,200)"
//    ctx.fillRect(100, 100, 200, 200)
//
//  }
//}
//
////class Example[Builder, Output <: FragT, FragT]
////(val bundle: scalatags.generic.Bundle[Builder, Output, FragT]){
////  val htmlFrag = {
////    import bundle.all._
////    div(
////      h1("Header 1"),
////      h2("Header 2"),
////      h3("Header 3"),
////      h4("Header 4"),
////      h5("Header 5"),
////      h6("Header 6"),
////      hr,
////      p(
////        "This is a paragraph with a whole bunch of ",
////        "text inside. You can have lots and lots of ",
////        "text"
////      ),
////      b("bold"), " ",
////      i("italic"), " ",
////      s("strikethrough"), " ",
////      u("underlined"), " ",
////      em("emphasis"), " ",
////      strong("strong"), " ",
////      sub("sub"), " ",
////      sup("sup"), " ",
////      code("code"), " ",
////      a("a-link"), " ",
////      small("small"), " ",
////      cite("cite"), " ",
////      ins("ins"), " ",
////      del("del"), " ",
////      hr,
////      ol(
////        li("Ordered List Item 1"),
////        li("Ordered List Item 2"),
////        li("Ordered List Item 3")
////      ),
////      ul(
////        li("Unordered List Item 1"),
////        li("Unordered List Item 2"),
////        li("Unordered List Item 3")
////      ),
////      dl(
////        dt("Definition List Term 1"),
////        dd("Definition List Item 1"),
////        dt("Definition List Term 2"),
////        dd("Definition List Item 2")
////      ),
////      hr,
////      pre(
////        """
////          |Preserved formatting area with all the whitespace
////          |kept around and probably some kind of monospace font
////        """.stripMargin
////      ),
////      blockquote(
////        """
////          |Block quote with a bunch of text inside
////          |which really rox
////        """.stripMargin
////      ),
////      hr,
////      table(
////        caption("This is a table caption"),
////        thead(
////          tr(
////            th("Header Content 1"),
////            th("Header Content 2")
////          )
////        ),
////        tbody(
////          tr(
////            td("Body Content 1"),
////            td("Body Content 2")
////          ),
////          tr(
////            td("Body Content A"),
////            td("Body Content B")
////          )
////        ),
////        tfoot(
////          tr(
////            td("Foot Content 1"),
////            td("Foot Content 2")
////          )
////        )
////      ),
////      hr,
////      label("input"), input,
////      br,
////      label("select"),
////      select(
////        option("lol"),
////        option("wtf")
////      ),
////      br,
////      label("textarea"),
////      textarea
////    )
////  }
////  val svgFrag = {
////    import bundle.implicits._
////    import bundle.svgTags._
////    import bundle.svgAttrs._
////    svg(height := "800", width := "500")(
////      polyline(
////        points := "20,20 40,25 60,40 80,120 120,140 200,180",
////        fill := "none",
////        stroke := "black",
////        strokeWidth := "3"
////      ),
////      line(
////        x1 := 175, y1 := 100,
////        x2 := 275, y2 := 0,
////        stroke := "rgb(255,0,0)",
////        strokeWidth := 10
////      ),
////      rect(
////        x := 300, y := 10,
////        rx := 20, ry := 20,
////        width := 100,
////        height := 100,
////        fill := "rgb(0,0,255)",
////        strokeWidth := 3,
////        stroke := "rgb(0,0,0)",
////        fillOpacity := "0.1",
////        strokeOpacity := "0.5"
////      ),
////      circle(
////        cx := 30, cy := 250,
////        r := 10,
////        stroke := "black",
////        strokeWidth := 3,
////        fill := "red"
////      ),
////      ellipse(
////        cx := 150, cy := 250,
////        rx := 100, ry := 50,
////        fill := "yellow",
////        stroke := "purple",
////        strokeWidth := 4
////      ),
////      polygon(
////        points := "300,110 350,290 260,310",
////        fill := "line",
////        stroke := "purple",
////        strokeWidth := 10
////      ),
////      path(
////        d := "M100 300 L25 500 L175 500 Z"
////      ),
////      text(
////        x := 350, y := 250,
////        fill := "red",
////        transform := "rotate(30 20, 40)",
////        "I love SVG!"
////      ),
////      text(x := 350, y := 350, fill := "green")(
////        "Several lines",
////        tspan(x := 350, y := 380, "First line."),
////        tspan(x := 350, y := 410, "Second line.")
////      ),
////      defs(
////        linearGradient(
////          id := "grad1",
////          x1 := "0%",
////          y1 := "0%",
////          x2 := "100%",
////          y2 := "0%",
////          stop(
////            offset := "0%",
////            stopColor := "rgb(255,255,0)"
////          ),
////          stop(
////            offset := "100%",
////            stopColor := "rgb(255,0,0)"
////          )
////        )
////      ),
////      ellipse(
////        cx := 100, cy := 590,
////        rx := 85, ry := 55,
////        fill := "url(#grad1)"
////      )
////    )
////  }
////}
////@JSExport
////object ScalaJSExample {
////  @JSExport
////  def main(t1: String, t2: String, t3: String, t4: String): Unit = {
////    val textExample = new Example(scalatags.Text)
////    val jsDomExample = new Example(scalatags.JsDom)
////    dom.document.getElementById(t1).innerHTML = textExample.htmlFrag.render
////    dom.document.getElementById(t2).innerHTML = textExample.svgFrag.render
////    dom.document.getElementById(t3).appen dChild(jsDomExample.htmlFrag.render)
////    dom.document.getElementById(t4).appendChild(jsDomExample.svgFrag.render)
////  }
////}
//
//
////@JSExport
////object Canvas {
////  @JSExport
////  def main(c: html.Canvas) = {
////    type Ctx2D =
////      dom.CanvasRenderingContext2D
////    val ctx = c.getContext("2d")
////      .asInstanceOf[Ctx2D]
////    val w = 300
////    c.width = w
////    c.height = w
////
////    ctx.strokeStyle = "red"
////    ctx.lineWidth = 3
////    ctx.beginPath()
////    ctx.moveTo(w/3, 0)
////    ctx.lineTo(w/3, w/3)
////    ctx.moveTo(w*2/3, 0)
////    ctx.lineTo(w*2/3, w/3)
////    ctx.moveTo(w, w/2)
////    ctx.arc(w/2, w/2, w/2, 0, 3.14)
////
////    ctx.stroke()
////  }
////}
//
////object TutorialApp extends JSApp {
////  def main(): Unit = {
////    jQuery(setupUI _)
////  }
////
////  def appendPar(targetNode: dom.Node, text: String): Unit = {
////    val parNode = document.createElement("p")
////    val textNode = document.createTextNode(text)
////    parNode.appendChild(textNode)
////    targetNode.appendChild(parNode)
////  }
////
////
////  def addClickedMessage(): Unit = {
////    appendPar(document.body, "You clicked the button!")
////  }
////
////  def setupUI(): Unit = {
////    jQuery("#click-me-button").click(addClickedMessage _)
////    jQuery("body").append("<p>Hello World</p>")
////      jQuery("#focus1").click(addClickedMessage _)
////    jQuery("""<button type="button">Click me!</button>""")
////      .click(addClickedMessage _)
////      .appendTo(jQuery("body"))
////  }
////
////  def clickedCircle(): Unit = {
////
////  }
////}