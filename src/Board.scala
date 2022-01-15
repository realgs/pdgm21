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
		addStones(player, stonesInHand, (pos + 1)%14, 0)


	def addStones(player: Int, leftStone: Int, boardPos: Int, przejazd: Int): Unit =
		leftStone match
			case 1 => handleEndMove(player, boardPos, przejazd)
			case _ =>
				if (player==0 && boardPos==13) || (player ==1 && boardPos == 6) then
					addStones(player, leftStone, (boardPos + 1) % 14, przejazd)
				else if (player==0 && boardPos==6) ||( player==1 &&boardPos==13) then
					addStone(boardPos); addStones(player, leftStone - 1, (boardPos + 1) % 14, przejazd+1)
				else
					addStone(boardPos); addStones(player, leftStone - 1, (boardPos + 1) % 14, przejazd)

	def handleEndMove(player: Int, boardPos: Int, przejazd: Int): Unit =
		val endPos = posToPlayersHole(boardPos)
		if endPos._2 == 6 && endPos._1 == player && przejazd==0 then
			addStone(boardPos)
			game.move(player)
		else if endPos._2 ==6 && endPos._1 == enemyPlayer(player) then
			addStones(player, 1, (boardPos+1)%14, przejazd)
		else if endPos._1 == player && getStonesNumberAtPos(boardPos) == 0  && getStonesNumberAtPos(reversePos(boardPos)) != 0 then
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

	def checkGameEnd(player: Int): Unit =
		if playerSiteClear(player) then
			if pointsDiff() == 0 then
				game.gameEnd(0)
			else if playerAPoints > playerBPoints then
				game.gameEnd(1)
			else
				game.gameEnd(2)

	def playerSiteClear(player: Int): Boolean =
		if player == 0 then
			playerAHoles.foldLeft(0)((p, n) => p + n) == 0
		else playerBHoles.foldLeft(0)((p, n) => p + n) == 0

	def playerAndPosToBoardPos(player: Int, hole: Int): Int =
		player * 7 + hole

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

	def reversePos(boardPos: Int): Int =
		12-boardPos

	def anySiteClear(): Boolean =
		playerSiteClear(0) || playerSiteClear(1)

	def boardString(): String =
		val rowB = playerBHoles.foldLeft("")((row, number) => String.format("| %2d ", number) + row)
		val rowA = playerAHoles.foldLeft("")((row, number) => row + String.format("| %2d ", number))
		val b = String.format("| %2d |", playerBBase)
		val a = String.format("| %2d |", playerABase)
		"KALAHA GAME BOARD:\n"+
		"_________________________________________\n" +
			s"|XXXX${rowB}|:pB |\n" +
			s"${b}----|----|----|----|----|----${a}\n" +
			s"| pA:${rowA}|XXXX|\n" +
			"-----------------------------------------\n\n"