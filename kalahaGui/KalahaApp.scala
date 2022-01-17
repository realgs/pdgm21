package kalaha

import kalaha.resource.strings.*
import kalaha.utils.Constants.*

import java.awt.{BorderLayout, Color, Dimension, RadialGradientPaint}
import javax.swing.{JButton, JFrame, JPanel}
import scala.swing.*
import scala.swing.event.{ButtonClicked, EditDone, MouseClicked}
import kalaha.ui.*
import kalaha.models.GradientTypeEnum.*
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import kalaha.models.{Connection, GUIActions, GUIEventsHandler}
import kalaha.models.PlayerEnum.{First, Player, Second}

import scala.io.StdIn.readInt
import scala.util.{Failure, Success, Try}


object KalahaApp extends SimpleSwingApplication {

  def top = new MainKahalaFrame {

    title = app_name
    preferredSize = Dimension(PREFERED_WIDTH, PREFERED_HEIGHT)

    contents = MainMenu.getView()

    listenTo(
      MainMenu.PvP_Btn, MainMenu.PvC_Btn, MainMenu.CvC_Btn, MainMenu.about_Btn
    )

    addListeners

    def addListeners: Unit =
      reactions += {
        case ButtonClicked(MainMenu.PvP_Btn) =>
          mainActor ! GUIActions.MainMenuBtnClicked(MainMenu.PvP_Btn)

          switchToView(First)
        case ButtonClicked(MainMenu.PvC_Btn) =>
          mainActor ! GUIActions.MainMenuBtnClicked(MainMenu.PvC_Btn)

          switchToView(First)
        case ButtonClicked(MainMenu.CvC_Btn) =>
          mainActor ! GUIActions.MainMenuBtnClicked(MainMenu.CvC_Btn)

          switchToView(First)
        case ButtonClicked(MainMenu.about_Btn) =>
          mainActor ! GUIActions.MainMenuBtnClicked(MainMenu.about_Btn)

      }

  }
  
}
