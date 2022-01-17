package kalaha

import kalaha.resource.strings.*
import kalaha.utils.Constants.*
import java.awt.{BorderLayout, Color, Dimension, RadialGradientPaint}
import javax.swing.{JButton, JFrame, JPanel}
import scala.swing.*
import scala.swing.event.{ButtonClicked, EditDone, MouseClicked}
import kalaha.models.GradientTypeEnum.*
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import kalaha.actors.{ComputerActor, PlayerActor, Server}
import kalaha.models.Connection
import kalaha.models.PlayerEnum.{First, Player, Second}

import scala.io.StdIn.readInt
import scala.util.{Failure, Success, Try}


object KalahaApp{

  val system = ActorSystem("Kahala")
  val server = system.actorOf(Props[Server], "Server")

  def main(args: Array[String]) = {

    println(welcome_info)

    runGame()

  }

  private def runGame(): Unit =
    printStart()

    getSafeUserInput match {
      case Success( userIn ) =>
        userIn match
          case 1 => startPlayerVsPlayer()
          case 2 => startPlayerVsComputer()
          case 3 => startComputerVsComputer()
          case 4 =>
            printAboutInfo()
            runGame()
          case _ => closeGame()
      case Failure(exception) =>
        closeGame()

  }


  private def startPlayerVsPlayer(): Unit =
    val firstPlayer: ActorRef = system.actorOf(Props(new PlayerActor(server, First)), "First")
    val secondPlayer: ActorRef = system.actorOf(Props(new PlayerActor(server, Second)), "Second")

  private def startComputerVsComputer(): Unit =
    val firstPlayer: ActorRef = system.actorOf(Props(new ComputerActor(server, First)), "First")
    val secondPlayer: ActorRef = system.actorOf(Props(new ComputerActor(server, Second)), "Second")

  private def startPlayerVsComputer(): Unit =
    val firstPlayer: ActorRef = system.actorOf(Props(new PlayerActor(server, First)), "First")
    val secondPlayer: ActorRef = system.actorOf(Props(new ComputerActor(server, Second)), "Second")

  private def printStart(): Unit = {
    println(menu_info)
    println(pvp_game_console)
    println(pvc_game_console)
    println(cvc_game_console)
    println(about_console)
    println(close_game_console)
    println(choose_action)
  }

  private def printAboutInfo(): Unit =
    println(about_Info)

  private def getSafeUserInput: Try[Int] = Try {
    readInt()
  }

  private def closeGame(): Unit =
    println(end_info)
    System.exit(0)
}
