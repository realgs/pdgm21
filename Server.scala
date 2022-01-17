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
    while !gameBoard.endGame() do
      val moveIndex = Await.result(Future {
        currentPlayer.requestMove(gameBoard)
      }, 30.seconds)
      if !gameBoard.isMoveLegal(currentPlayer.id, moveIndex) then
        currentPlayer.notifyOfIllegalMovement()
      else
        val nextPlayerId = gameBoard.makeAMove(currentPlayer.id, moveIndex)
        currentPlayer = if nextPlayerId == 1 then player1 else player2

    player1.sendResult(gameBoard.scores)
    player2.sendResult(gameBoard.scores)