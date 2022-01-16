import java.util.Scanner

class Player extends Client {
	override def moveResponse(board: Board, player: Int): Int =
		println(s"PLAYER'S: ${player} MOVE")
		println(board.boardToString())
		chooseHole(board, player)

	def chooseHole(board: Board, player: Int): Int =
		val scanner = new Scanner(System.in)
		println("Write number [1-6] to choose pit")
		val playersPit = scanner.nextLine().toInt
		if playersPit < 1 || playersPit > 6 then
			println("Invalid hole number")
			chooseHole(board, player)
		else if board.emptyPit(board.boardPosition(player, playersPit-1)) then
			println("Empty pit")
			chooseHole(board, player)
		else playersPit-1
}
