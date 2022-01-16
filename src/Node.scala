class Node(board: Board, player: Int, maxDepth: Int, val pit: Int, parentNode: Node) {
	var childNodes: List[Node] = List()
	val nodeAdv = board.playerAStoreAdv()-board.playerBStoreAdv()
	var minValSubtree: Int = nodeAdv
	var maxValSubtree: Int = nodeAdv
	nodeUpdate(nodeAdv)
	makeSubtrees()

	def makeSubtrees():Unit =
		if maxDepth > 0 then
			for (n <- 0 to 5)
				if !board.emptyPit(board.boardPosition(player, n)) then
					var clonedBoard = board.copy()
					var changePlayer = clonedBoard.move(player, n)
					if changePlayer then
						var newPlayer = player+1 %2
						var newNode = new Node(clonedBoard, newPlayer, maxDepth-1, n,this)
						childNodes = newNode :: childNodes
					else
						var newNode = new Node(clonedBoard, player, maxDepth-1, n,this)
						childNodes = newNode :: childNodes

	def nodeUpdate(childValue: Int): Unit=
		minValSubtree = Math.min(minValSubtree, childValue)
		maxValSubtree = Math.max(maxValSubtree, childValue)
		if parentNode != null then
			parentNode.nodeUpdate(childValue)
}
