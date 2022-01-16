import scala.util.Random

class Computer(val depth: Int) extends Client {

	override def moveResponse(board: Board, player: Int): Int =
		println(s"MOVE COMPUTER: ${player}")
		//println(board.boardToString())
		val pit = decision(board, player)
		println(s"PIT:  ${pit+1}\n")
		pit

	def randomMove(board: Board, player: Int): Int=
		var k = Random.nextInt(6)
		if board.emptyPit(board.boardPosition(player, k)) then
			 k = randomMove(board, player)
		k

	def decision(board: Board, player: Int): Int=
		var root = new Node(board.copy(), player, depth, 2,null)
		var sorted: List[Node]=List()
		if player == 0 then
			sorted = root.childNodes.sortBy((n1)=> (-n1.minValSubtree, -n1.maxValSubtree) )
		else
			sorted = root.childNodes.sortBy((n1)=> (n1.minValSubtree, n1.maxValSubtree) )
		sorted(0).pit
}
