package de.tkip.sbpm.frontend.css

import scalacss.Defaults._
import scalacss.internal.mutable.GlobalRegistry
import scalacss.ScalaCssReact._
import de.tkip.sbpm.frontend.components.{LeftNav, TopNav}
import de.tkip.sbpm.frontend.components.itmes.ProcessList
import de.tkip.sbpm.frontend.pages.{InternalBehaviorPage, ItemsPage, SubjectsV, TemplateTest}

object AppCSS {

  def load = {
    GlobalRegistry.register(GlobalStyle,
      TopNav.Style,
      LeftNav.Style,
      ItemsPage.Style,
      TemplateTest.Style,
      SubjectsV.Style,
      InternalBehaviorPage.Style,
      ProcessList.Style)
    GlobalRegistry.onRegistration(_.addToDocument())
  }
}
