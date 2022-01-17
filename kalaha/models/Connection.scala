package kalaha.models
import kalaha.models.PlayerEnum.*

object Connection{

  final case class Connect(playerID: Int)
  case object Disconnect
  final case class Move(pitIndex: Int)
  case object Next
  case object EndGame
  final case class validateMove(pit: Int)

}

