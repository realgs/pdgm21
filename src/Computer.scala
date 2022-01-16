import scala.util.Random

class Computer extends Client {
	val depth = 5

	override def moveResponse(board: Board, player: Int): Int =
		println(s"MOVE COMPUTER: ${player}")
		//println(board.boardToString())
		val pit = randomMove(board, player)
		println(s"PIT:  ${pit}\n")
		pit

	def randomMove(board: Board, player: Int): Int=
		var k = Random.nextInt(6)
		if board.emptyPit(board.boardPosition(player, k)) then
			 k = randomMove(board, player)
		k

	def decision(board: Board, player: Int): Int=
		5


}
