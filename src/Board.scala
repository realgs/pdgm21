class Board(stones: Int, game: Game):
	private var playerABase: Int = 0
	private var playerBBase: Int = 0
	private var playerAHoles = new Array[Int](6).map(a => stones)
	private var playerBHoles = new Array[Int](6).map(a => stones)


	def playerAPoints =
		playerABase + playerAHoles.foldLeft(0)((points, stonesInHole) => points + stonesInHole)

	def playerBPoints =
		playerBBase + playerBHoles.foldLeft(0)((points, stonesInHole) => points + stonesInHole)

	def pointsDiff(): Int =
		Math.abs(playerAPoints - playerBPoints)

	def playerAAdv(): Int =
		playerBPoints - playerAPoints

	def playerBAdv(): Int =
		playerAPoints - playerBPoints

	def move(player: Int, holeNumber: Int) =
		require(0 <= holeNumber && holeNumber < 6)
		var stonesInHand = if player == 0 then playerAHoles(holeNumber) else playerBHoles(holeNumber)
		val pos = playerAndPosToBoardPos(player, holeNumber)
		setStones(pos, 0)
		addStones(stonesInHand, pos+1)

		def addStones(leftStone: Int, boardPos: Int): Unit =
			leftStone match
				case 1 => handleEndMove(player, boardPos)
				case _ => addStone(boardPos); addStones(leftStone - 1, (boardPos + 1) % 14)

	def handleEndMove(player: Int, boardPos: Int): Unit =
		val endPos = posToPlayersHole(boardPos)
		if endPos._2 == 6 && endPos._1 == player then
			addStone(boardPos)
			game.nextMove(player)
		else if endPos._1 == player && getStonesNumberAtPos(boardPos) == 0 then
			val enemyPlayerHole = 5 - endPos._2
			if player == 0 then
				playerABase += 1
				val position = playerAndPosToBoardPos(enemyPlayer(player), enemyPlayerHole)
				playerABase += getStonesNumberAtPos(position)
				setStones(position, 0)
			else
				playerBBase += 1
				val position = playerAndPosToBoardPos(enemyPlayer(player), enemyPlayerHole)
				playerBBase += getStonesNumberAtPos(position)
				setStones(position, 0)
		else addStone(boardPos)


	def playerAndPosToBoardPos(player: Int, hole: Int): Int =
		player * 6 + hole

	def posToPlayersHole(pos: Int): (Int, Int) =
		(pos / 7, pos % 7)

	def getStonesNumberAtPos(pos: Int): Int =
		val hole = posToPlayersHole(pos)
		if hole._1 == 0 then
			if hole._2 < 6 then playerAHoles(hole._2)
			else playerABase
		else if hole._2 < 6 then playerBHoles(hole._2)
		else playerBBase

	def addStone(pos: Int): Unit =
		val hole = posToPlayersHole(pos)
		if hole._1 == 0 then
			if hole._2 < 6 then playerAHoles(hole._2) += 1
			else playerABase += 1
		else if hole._2 < 6 then playerBHoles(hole._2) += 1
		else playerBBase += 1

	def setStones(pos: Int, stones: Int): Unit =
		val hole = posToPlayersHole(pos)
		if hole._1 == 0 then
			if hole._2 < 6 then playerAHoles(hole._2) = stones
			else playerABase = stones
		else if hole._2 < 6 then playerBHoles(hole._2) = stones
		else playerBBase = stones

	def enemyPlayer(player: Int): Int =
		if player == 0 then 1 else 0

	def boardString(): String=
		val rowB = playerBHoles.foldLeft("")((row, number)=>String.format("| %2d ", number)+row)
		val rowA = playerAHoles.foldLeft("")((row, number)=>row+String.format("| %2d ", number))
		val b = String.format("| %2d |",playerBBase)
		val a = String.format("| %2d |",playerABase)
		"_________________________________________\n"+
		s"|XXXX${rowB}|:pB |\n"+
		s"${b}----|----|----|----|----|----${a}\n"+
		s"| pA:${rowA}|XXXX|\n"+
		"-----------------------------------------\n"