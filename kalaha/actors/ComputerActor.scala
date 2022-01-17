package kalaha.actors

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import kalaha.models.Connection.*
import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.models.{Actions, Connection, DecisionTree, GameBoard}
import kalaha.utils.Constants.{DECISION_TREE_DEPTH, PLAYER_HOLES, VALIDATION_TIME_LIMIT}

import scala.concurrent.Await
import scala.io.StdIn.readInt
import scala.util.{Failure, Success, Try}

class ComputerActor(server: ActorRef, player: Player) extends Actor{

  private val decisionTree = new DecisionTree(player)

  server ! Connection.Connect(player.id)

  override def receive: Receive = {

    case Actions.Move(gameBoard: GameBoard) =>
      val nextMove = chooseNextMove(gameBoard)
      player match
        case First =>
          println(s"I am Computer 1 and my move is ${nextMove+1}")
        case Second =>
          println(s"I am Computer 2 and my move is ${nextMove+1}")
      sender ! nextMove

  }

  private def chooseNextMove(gameBoard: GameBoard): Int =
    var move: Int = 0
    var bestScore = Integer.MIN_VALUE

    for(hole <- 0 until PLAYER_HOLES){
      if gameBoard.isMoveValid(hole) then
        val copyBoard = gameBoard.clone()
        copyBoard.changeToSimulationMode()

        copyBoard.moveStones(hole)
        val score = decisionTree.minimax(
          copyBoard, DECISION_TREE_DEPTH, Integer.MIN_VALUE, Integer.MAX_VALUE, copyBoard.getTurn() == player
        )

        if score > bestScore then
          bestScore = score
          move = hole
    }

    move


}
