class Board(stones: Int):
	var playerAStore: Int = 0
	var playerBStore: Int = 0
	var playerAPits = new Array[Int](6).map(a => stones)
	var playerBPits = new Array[Int](6).map(a => stones)
	val playerA = 0
	val playerB = 1

	def copy(): Board=
		var newBoard = new Board(0)
		newBoard.playerAStore = playerAStore
		newBoard.playerBStore = playerBStore
		newBoard.playerAPits = playerAPits.clone()
		newBoard.playerBPits = playerBPits.clone()
		newBoard


	def playerAPoints =
		playerAStore + playerAPits.foldLeft(0)((points, stonesInHole) => points + stonesInHole)

	def playerBPoints =
		playerBStore + playerBPits.foldLeft(0)((points, stonesInHole) => points + stonesInHole)

	def pointsDiff(): Int =
		Math.abs(playerAPoints - playerBPoints)

	def playerAAdvantagePoints(): Int =
		playerBPoints - playerAPoints

	def playerBAdvantagePoints(): Int =
		playerAPoints - playerBPoints

	def playerAStoreAdv(): Int = playerAStore
	def playerBStoreAdv(): Int = playerBStore

	def move(player: Int, playersPitNumber: Int): Boolean=
		require(0 <= playersPitNumber && playersPitNumber < 6)
		val pos = boardPosition(player, playersPitNumber)
		var stonesInHand = getStones(pos)
		setStones(pos, 0)
		addStones(player, stonesInHand, nextPosition(pos), 0)

	def nextPosition(position: Int): Int=
		(position + 1)%14

	def addStones(player: Int, leftStone: Int, boardPos: Int, przejazd: Int): Boolean =
		leftStone match
			case 1 => handleEndMove(player, boardPos, przejazd)
			case _ =>
				val nextPos = nextPosition(boardPos)
				if enemyPlayerStorePos(player, boardPos) then
					addStones(player, leftStone, nextPos, przejazd)
				else if playerStorePos(player, boardPos) then
					addStone(boardPos); addStones(player, leftStone - 1, nextPos, przejazd+1)
				else
					addStone(boardPos); addStones(player, leftStone - 1, nextPos, przejazd)

	def playerStorePos(player: Int, boardPos: Int): Boolean =
		(player==playerA && boardPos==6) || (player ==playerB && boardPos == 13)

	def enemyPlayerStorePos(player: Int, boardPos: Int): Boolean =
		(player==playerA && boardPos==13) || (player ==playerB && boardPos == 6)

	def handleEndMove(player: Int, boardPos: Int, przejazd: Int): Boolean =
		if playerStorePos(player, boardPos) && przejazd==0 then
			addStone(boardPos)
			true
		else if enemyPlayerStorePos(player, boardPos) then
			handleEndMove(player, nextPosition(boardPos), przejazd)
		else if playerSide(boardPos) == player && emptyPit(boardPos)  && !emptyPit(mirrorPosition(boardPos)) then
			if player == playerA then
				playerAStore += 1
				val mirrorPos = mirrorPosition(boardPos)
				playerAStore += getStones(mirrorPos)
				setStones(mirrorPos, 0)
			else
				playerBStore += 1
				val position = mirrorPosition(boardPos)
				playerBStore += getStones(position)
				setStones(position, 0)
			false
		else
			addStone(boardPos)
			false

	def checkGameEnd(player: Int): Int =
		if anySideClear() then
			if pointsDiff() == 0 then 0
			else if playerAPoints > playerBPoints then 1
			else 2
		else -1

	def emptyPit(pos: Int): Boolean=
		getStones(pos) == 0

	def playerSideClear(player: Int): Boolean =
		if player == 0 then
			playerAPits.foldLeft(0)((p, n) => p + n) == 0
		else playerBPits.foldLeft(0)((p, n) => p + n) == 0

	def boardPosition(player: Int, pitNumber: Int): Int =
		player * 7 + pitNumber

	def playerAndPit(boardPos: Int): (Int, Int) =
		(boardPos / 7, boardPos % 7)

	def playerSide(boardPos: Int): Int =
		boardPos / 7

	def getStones(pos: Int): Int =
		val hole = playerAndPit(pos)
		if hole._1 == 0 then
			if hole._2 < 6 then playerAPits(hole._2)
			else playerAStore
		else if hole._2 < 6 then playerBPits(hole._2)
		else playerBStore

	def addStone(boardPos: Int): Unit =
		val pitPos = playerAndPit(boardPos)
		if pitPos._1 == 0 then
			if pitPos._2 < 6 then playerAPits(pitPos._2) += 1
			else playerAStore += 1
		else if pitPos._2 < 6 then playerBPits(pitPos._2) += 1
		else playerBStore += 1

	def setStones(boardPos: Int, stonesNumber: Int): Unit =
		val pitPos = playerAndPit(boardPos)
		if pitPos._1 == 0 then
			if pitPos._2 < 6 then playerAPits(pitPos._2) = stonesNumber
			else playerAStore = stonesNumber
		else if pitPos._2 < 6 then playerBPits(pitPos._2) = stonesNumber
		else playerBStore = stonesNumber

	def enemyPlayer(player: Int): Int =
		if player == playerA then playerB else playerA

	def mirrorPosition(boardPos: Int): Int =
		12-boardPos

	def anySideClear(): Boolean =
		playerSideClear(0) || playerSideClear(1)

	def boardToString(): String =
		val rowB = playerBPits.foldLeft("")((row, number) => String.format("| %2d ", number) + row)
		val rowA = playerAPits.foldLeft("")((row, number) => row + String.format("| %2d ", number))
		val b = String.format("| %2d |", playerBStore)
		val a = String.format("| %2d |", playerAStore)
		"KALAHA GAME BOARD:\n"+
		"_________________________________________\n" +
			s"|XXXX${rowB}|:pB |\n" +
			s"${b}----|----|----|----|----|----${a}\n" +
			s"| pA:${rowA}|XXXX|\n" +
			"-----------------------------------------"