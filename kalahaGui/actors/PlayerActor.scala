package kalaha.actors

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import kalaha.models.Connection.*
import kalaha.models.GuiActorActions.*
import kalaha.models.PlayerEnum.Player
import kalaha.models.{Actions, Connection}
import kalaha.utils.Constants.{PLAYER_HOLES, TIME_LIMIT, VALIDATION_TIME_LIMIT}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn.readInt
import scala.util.control.Breaks.breakable
import scala.util.{Failure, Success, Try}

class PlayerActor(private val server: ActorRef, private val guiActor: ActorRef, player: Player) extends Actor{

  private val validationTimeout: Timeout = Timeout(VALIDATION_TIME_LIMIT)
  private implicit val timeout: Timeout = Timeout(TIME_LIMIT)

  server ! Connection.Connect(player.id)

  override def receive: Receive = {
    case Actions.Move(gameBoard) =>

      guiActor ! Refresh(gameBoard, player)

      val move = Try(getPlayersMove())

      move match
        case Success(number) =>
          sender ! number
        case Failure(_) =>
          ()

    case Actions.YouWon(myScore, opponentScore) =>
      guiActor ! ShowDialogWon(myScore, opponentScore, this.player.toString)

    case Actions.Draw(score) =>
      guiActor ! ShowDialogDraw(score)
      
  }

  private def getPlayersMove(): Int =
    val move = guiActor ? PrepareForBtnClick(player)

    Try(Await.result(move, TIME_LIMIT).asInstanceOf[Int]) match {
      case Success(value) =>
        if validateMove(value) then
          value
        else getPlayersMove()
      case Failure(_) => throw new Exception("Time limit exception")

    }


  private def validateMove(index: Int): Boolean =
    val isValid = server ? Connection.validateMove(index)
    Await.result(isValid, VALIDATION_TIME_LIMIT).asInstanceOf[Boolean]

}
