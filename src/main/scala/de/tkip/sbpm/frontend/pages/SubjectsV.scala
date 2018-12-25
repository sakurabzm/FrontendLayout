package de.tkip.sbpm.frontend.pages

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import de.tkip.sbpm.frontend.components.LeftNav
import de.tkip.sbpm.frontend.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.collection.mutable.ListBuffer

object SubjectsV {

  object Style extends StyleSheet.Inline {

    import dsl._

    val content = style(
      position.relative,
      display.flex,
      width :=! "100%",
      minHeight(600.px),
      backgroundColor :=! "#FA8072",
      borderTop :=! "1px solid rgb(223, 220, 200)",
      fontFamily :=! "Roboto, sans-serif"
    )
    /*
    RightNav
     */
    val rightNav =
      style(
         display.flex,
        //position.absolute,
        width :=! "16%",
        //height :=! "100%",
        borderLeft :=! "1px solid rgb(223, 220, 200)",
        backgroundColor :=! c"#C0C0C0")
    /*
    LeftNav
     */
    val leftNav = style(
      position.relative,
      display.flex,
      margin.`0`,
      width :=! "15%",
      minHeight :=! 600.px,
      //borderLeft :=! "1px solid rgb(223, 220, 200)",
      backgroundColor :=! c"#C0C0C0"
    )
    /*
    buttonStyle
     */
    val buttonStyle = style(
      &.active(backgroundColor :=! c"#6495ED"),
      border :=! "1px solid transparent",
      fontSize :=! 11.px
    )
    /*
    SubjectStyle
     */
    val subjectData = style(
      //display.flex,
      position.relative,
      marginLeft :=! 50.px,
      marginTop :=! "10%",
      minWidth :=! 150.px,
      maxWidth :=! 150.px,
      minHeight :=! 200.px,
      maxHeight :=! 200.px,
      borderRadius :=! 10.px,
      border :=! "solid 2px #696969",
      backgroundColor :=! "#ADD8E6",
      cursor.pointer
    )
  }

  case class Subject(id: Int, name: String, inputPool: Int)


  val eo = Subject(1, "SubjectOne", 100)
  var subjectsList: ListBuffer[Subject] = ListBuffer(eo)


  //case class State(subjects: ListBuffer[Subject])
  case class State(subjects: ListBuffer[Subject], selectedId: String)


  var isSelected: ListBuffer[Subject] = ListBuffer()

  class Backend(val $: BackendScope[Unit, State]) {

    def render(s: State) = {
      <.div(
        Style.content,
        createLeftNav(),
        <.div(
          ^.width := "69%",
          ^.position.relative,
          //^.backgroundColor := "gold",
          ^.overflowX.scroll,
          ^.position.relative,
          ^.display.`inline-flex`,
          s.subjects.toTagMod { itme =>
            createContent(itme, s)
          },
        ),
        createRightNav(s)
      )
    }


    def createLeftNav() = {
      <.div(
        Style.leftNav,
        <.ul(
          "I'm LeftNav!"
        )
      )
    }

    def createContent(s: Subject, state: State) = {
      <.div(
        Style.subjectData,
        s.name,
        //setContent(s, state),
        ^.fontSize := 12.px,
        ^.paddingTop := 50.px,
        ^.textAlign.center,
        ^.wordWrap.`break-word`,
        ^.wordBreak.`break-all`,
        (^.backgroundColor := "#4169E1").when(isSelected.contains(s)),
        ^.onClick --> selected(s)
        //^.onDoubleClick --> jumpLink

      )
    }

    def createRightNav(state: State) = {
      <.div(
        Style.rightNav,
        ^.overflowY.scroll,
        <.ul(
          ^.position.relative,
          //^.backgroundColor := "pink",
          ^.listStyle := "none",
          <.p(
            " Subject Settings",
            ^.textAlign.left,
            ^.marginLeft := 0.px,
            ^.paddingTop := 20.px,
            ^.fontSize := 20.px
          ),
          <.button(
            "Add ",
            Style.buttonStyle,
            ^.borderRadius := "13em/3em",
            ^.width := "22%",
            ^.height := 30.px,
            ^.onClick --> addSubject
          ),
          <.button(
            "Delete",
            Style.buttonStyle,
            ^.borderRadius := "13em/3em",
            ^.marginLeft := 10.px,
            ^.width := "22%",
            ^.height := 30.px,
            ^.onClick --> deleteSubject
          ),
          <.button(
            "Save",
            Style.buttonStyle,
            ^.borderRadius := "13em/3em",
            ^.marginLeft := 10.px,
            ^.width := "22%",
            ^.height := 30.px
          ),
          <.br,
          <.li(
            ^.marginTop := 20.px,
            "Subject ID: ",
            <.br,
            <.br,
            <.label(
              //^.position.absolute,
              ^.display.block,
              ^.width := "72%",
              ^.height := 30.px,
              ^.backgroundColor := "#9370DB",
              state.selectedId
            )
          ),
          <.li(
            ^.marginTop := 20.px,
            "Subject Name: ",
            <.br,
            <.br,
            <.input.text(
              ^.display.`inline-flex`,
              ^.defaultValue := sn(state),
              ^.onChange ==> getSubjectName
            )
          ),
          <.li(
            ^.marginTop := 20.px,
            "InputPool: (Max 200) ",
            <.br,
            <.br,
            <.input.text(
              ^.defaultValue := "100"
            )
          ),
          <.br,
          <.br,
          "Description: ",
          <.textarea(
            ^.display.flex,
            ^.marginTop := 20.px,
            ^.maxWidth := "72%",
            ^.minWidth := "72%",
            ^.maxHeight := 80.px,
            ^.minHeight := 80.px

          ),
          <.br,
          <.br,
          <.br,
          <.button(
            "Confirm",
            Style.buttonStyle,
            ^.borderRadius := "50%",
            ^.width := "30%",
            ^.height := 30.px,
            ^.onClick --> confirmContent(state)
          ),
          <.button(
            "Cancel",
            Style.buttonStyle,
            ^.borderRadius := "50%",
            ^.marginLeft := "20%",
            ^.width := "30%",
            ^.height := 30.px,
            ^.onClick --> Callback.alert("You will lose the current data!")
          ),
          <.br,
          <.br
        )
      )
    }

    var newSubjectName = ""
    def getSubjectName(e: ReactEventFromInput): Callback = {
      newSubjectName = e.target.value
      Callback()
    }

    var id: Int = 1 //subject id should be got from backend.
    def addSubject(): Callback = {
      id += 1
      val newSubject = Subject(id, "Subject", 100)
      subjectsList += newSubject
      $.modState(s => State(subjectsList, ""))
    }


    def deleteSubject(): Callback = {
      subjectsList --= isSelected
      $.modState(s => s)
    }

    def selected(ss: Subject): Callback = {
      if (isSelected.contains(ss)) {
        isSelected -= ss
        $.modState(s => s.copy(subjectsList, ""))
      } else {
        isSelected.clear()
        isSelected += ss
        $.modState(s => s.copy(subjectsList, ss.id.toString))
      }
    }

    def sn(state: State): String ={  // display current subject name
      var currentName: String = ""
     state.subjects.foreach( s => {
       if(s.id.toString.equals(state.selectedId))
         currentName = s.name
       else currentName
     })
      currentName
    }


    def setContent(s: Subject, state: State) = {

    }

    def confirmContent(state: State): Callback = {
      for(i <- state.subjects.indices){
        val sub = state.subjects(i)
        if(sub.id == state.selectedId.toInt){
          val updateS = sub.copy(sub.id, newSubjectName, sub.inputPool)
          state.subjects.update(i,updateS)
        }
      }
      $.modState(s => s)
    }
  }

  val component = ScalaComponent.builder[Unit]("SubjectsPage")
    .initialState(State(subjectsList, ""))
    .renderBackend[Backend]
    .build

  def apply() = component()
}
