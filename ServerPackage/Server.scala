package com.example.ServerPackage

import com.example.GamePackage.Game
import com.example.UserPackage.{Player}
import java.util.concurrent.{TimeoutException}
import javax.swing.JTextPane
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.{DurationInt}

class Server(private val game: Game, private val gamePane: JTextPane) {

  implicit val ec = ExecutionContext.global

  def stopGame(): Unit = {
    gamePane.setText(game.printGameResult())
  }

  def startGame(): Unit = {
    gamePane.setText(game.printGameBoard)
  }

  def processGame(player: Player): Unit = {
     try {
        val result = Future {player.moveRequest(game); true}
        Await.result(result, 2.second)
      }catch{ case _: TimeoutException => ()}
  }

  def moveReceived(player: Player, userField: Int): Unit = {
    game.processPlayerMove(userField)
    game.changeActivePlayer()
    gamePane.setText(game.printGameBoard)
    if (game.checkIfEnd()) stopGame()
    else processGame(player)
  }
}