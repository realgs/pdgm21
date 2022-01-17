package kalaha.actors

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import kalaha.models.Connection.*
import kalaha.models.PlayerEnum.Player
import kalaha.models.{Actions, Connection}
import kalaha.resource.strings.*
import kalaha.utils.Constants.{PLAYER_HOLES, VALIDATION_TIME_LIMIT}

import scala.concurrent.Await
import scala.io.StdIn.readInt
import scala.util.{Failure, Success, Try}

class PlayerActor(server: ActorRef, player: Player) extends Actor{

  private implicit val validationTimeout: Timeout = Timeout(VALIDATION_TIME_LIMIT)

  server ! Connection.Connect(player.id)

  override def receive: Receive = {
    case Actions.Move(_) =>
      
      var input = -1
      
      while
        if input < 1 || input > PLAYER_HOLES then true
        else !validateMove(input-1)
      do {
        println(choose_hole)

        getSafeUserInput match {
          case Success( userIn ) =>
            if (userIn < 1 || userIn > PLAYER_HOLES) then
              println("eeee jakis blad")
            else
              input = userIn
              println("Dobra dobra jest ok")
          case Failure(exception) =>
            
            println("Unexpected acction occurred, try one more time!")
        }
      }

      sender ! (input - 1)
      
  }


  private def getSafeUserInput: Try[Int] = Try {
    readInt()
  }

  private def validateMove(index: Int): Boolean =
    val isValid = server ? Connection.validateMove(index)
    Await.result(isValid, VALIDATION_TIME_LIMIT).asInstanceOf[Boolean]

}
