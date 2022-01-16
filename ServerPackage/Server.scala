package com.example.ServerPackage

import com.example.GamePackage.Game
import com.example.UserPackage.{Computer, Player}

import java.util.{Timer, TimerTask}
import javax.swing.JTextPane
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}

class Server(private val game: Game, private val gamePane: JTextPane) {

  implicit val ec = ExecutionContext.global
  private val timeLimit = 30000
  private var correctMove = false

  private val moveScheduler = new Timer().schedule(
    new TimerTask {
      override def run(): Unit =
        if (!correctMove)
          gamePane.setText(game.printLackOfMove())
    }, timeLimit
  )

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
        correctMove = true
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