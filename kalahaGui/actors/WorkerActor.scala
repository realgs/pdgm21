package kalaha.actors

import akka.actor.{Actor, ActorRef, Props}
import kalaha.models.PlayerEnum.{First, Second}
import kalaha.actors.{ComputerActor, PlayerActor}
import kalaha.models.GUIActions
import kalaha.models.GuiActorActions.ShowDialogAbout
import kalaha.resource.strings.*
import kalaha.ui.MainMenu

import scala.util.{Failure, Success, Try}

class WorkerActor(private val guiActorFirst: ActorRef) extends Actor {

  val server = context.actorOf(Props[Server], "Server")

  override def receive: Receive = {

    case GUIActions.MainMenuBtnClicked(button) =>
      button match
        case MainMenu.PvP_Btn =>
          startPlayerVsPlayer()

        case MainMenu.PvC_Btn =>
          startPlayerVsComputer()

        case MainMenu.CvC_Btn =>
          startComputerVsComputer()

        case MainMenu.about_Btn =>
          guiActorFirst ! ShowDialogAbout

  }


  private def startPlayerVsPlayer(): Unit =
    val firstPlayer: ActorRef = context.actorOf(Props(new PlayerActor(server, guiActorFirst, First)), "First")
    val secondPlayer: ActorRef = context.actorOf(Props(new PlayerActor(server, guiActorFirst, Second)), "Second")

  private def startComputerVsComputer(): Unit =
    val firstPlayer: ActorRef = context.actorOf(Props(new ComputerActor(server, guiActorFirst, First)), "First")
    val secondPlayer: ActorRef = context.actorOf(Props(new ComputerActor(server, guiActorFirst, Second)), "Second")

  private def startPlayerVsComputer(): Unit =
    val firstPlayer: ActorRef = context.actorOf(Props(new PlayerActor(server, guiActorFirst, First)), "First")
    val secondPlayer: ActorRef = context.actorOf(Props(new ComputerActor(server, guiActorFirst, Second)), "Second")


}
