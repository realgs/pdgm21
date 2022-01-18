import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
class Server(val player1: Player, val player2: Player):
  player1.id = 1
  player2.id = 2

  def oppositePlayer(player: Player) = if player.id == 1 then player2 else player1
  def play(housesPerSide: Int, initialSeedAmount: Int) =
    val gameBoard = new KalahaBoard(housesPerSide, initialSeedAmount)
    var currentPlayer = player1
    try
      while !gameBoard.endGame() do
        val moveIndex = Await.result(Future {
          var move = currentPlayer.requestMove(gameBoard.copy())
          while !gameBoard.isMoveLegal(currentPlayer.id, move) do
            currentPlayer.notifyOfIllegalMovement()
            move = currentPlayer.requestMove(gameBoard.copy())

          move
        }, 30.seconds)
        val nextPlayerId = gameBoard.makeAMove(currentPlayer.id, moveIndex)
        currentPlayer = if nextPlayerId == 1 then player1 else player2

      player1.sendResult(gameBoard.scores)
      player2.sendResult(gameBoard.scores)
    catch
      case e: TimeoutException =>
        player1.notifyOfGameInterrupted(oppositePlayer(currentPlayer).id)
        player2.notifyOfGameInterrupted(oppositePlayer(currentPlayer).id)