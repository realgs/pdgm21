package game

import akka.actor.*
import client.{AI, Human, Player}
import server.ServerManager

import scala.io.StdIn

object Game extends App {

  val system = ActorSystem("kalaha-game-system")

  var firstPlayer = system.actorOf(Props(new AI(1)))
  var secondPlayer = system.actorOf(Props(new AI(2)))

  println("Declare first player mode (1 - Human, 0 - AI):")
  if (StdIn.readInt() == 1)
    firstPlayer = system.actorOf(Props(new Human(1)))

  println("Declare second player mode (1 - Human, 0 - AI):")
  if (StdIn.readInt() == 1)
    secondPlayer = system.actorOf(Props(new Human(2)))

  val serverManager = system.actorOf(Props(new ServerManager(firstPlayer, secondPlayer, new Board)))

  serverManager ! ServerManager.start

}
