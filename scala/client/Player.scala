package client

import akka.actor.*
import client.Player.moveRequest
import game.Board
import server.ServerManager

object Player {

  case class moveRequest(serverBoard: Board)
}

abstract class Player(id: Int) extends Actor {

  var board: Board = _

  override def receive: Receive = {

    case moveRequest(serverBoard) =>
      board = serverBoard
      sender() ! ServerManager.moveTakenFromPlayer(choosePit())

  }

  def choosePit(): Int

}
