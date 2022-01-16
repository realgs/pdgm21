import java.util.Scanner
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DAYS, Duration}
import scala.util.Random

class Game:
	var board = new Board(6)

	var playerA :Client = _
	var playerB :Client = _

	val maxResponseTime = 30

	def gameEnd(result: Int): Unit =
		result match
			case 0 => println(s"REMIS\n${board.playerAPoints} : ${board.playerBPoints}")
			case 1 => println(s"PLAYER A WON\n${board.playerAPoints} : ${board.playerBPoints}")
			case 2 => println(s"PLAYER B WON\n${board.playerAPoints} : ${board.playerBPoints}")
			case _ => ()

	def printBoard(): Unit =
		println(board.boardToString())

	def play() =
		var activePlayer = 0
		while !board.anySideClear() do
			var pit = 0
			if activePlayer == 0 then
				try {
					pit = Await.result(Future{playerA.moveResponse(board, activePlayer)}, Duration(maxResponseTime*1000, "millis"))
				}catch {
					case e: java.util.concurrent.TimeoutException =>
						pit = randomMove(board, activePlayer)
						println(s"Time out --- random move: ${pit+1}")
				}
			else
				try {
					pit = Await.result(Future{playerB.moveResponse(board, activePlayer)}, Duration(maxResponseTime*1000, "millis"))
				}catch {
					case e: java.util.concurrent.TimeoutException =>
						pit = randomMove(board, activePlayer)
						println(s"Time out --- random move: ${pit+1}")
				}

			if !board.move(activePlayer, pit) then
				activePlayer = enemyPlayer(activePlayer)

			gameEnd(board.checkGameEnd(activePlayer))

	def enemyPlayer(player: Int): Int =
		if player == 0 then 1 else 0

	def testMove(player: Int, pit: Int): Boolean=
		val k = board.move(player, pit)
		gameEnd(board.checkGameEnd(player))
		k

	def randomMove(board: Board, player: Int): Int=
		var k = Random.nextInt(6)
		if board.emptyPit(board.boardPosition(player, k)) then
			k = randomMove(board, player)
		k

	def start(): Unit =
		println("HELLO")
		println("IT'S KALAHA GAME\n")
		setup()
		play()

	def setup(): Unit =
		val players = setPlayersNumber()
		val computers = 2 - players
		if players == 2 then
			playerA = new Player()
			playerB = new Player()
		else if players == 1 then
			playerA = new Player()
			playerB = new Computer(5)
		else
			playerA = new Computer(8)
			playerB = new Computer(8)

	def setPlayersNumber(): Int =
		val scanner = new Scanner(System.in)
		println("SETUP...")
		println("How many live players would you start with? [0-2] \nComputer numbers will be set automatically")
		var alivePlayers = scanner.nextLine().toInt
		if alivePlayers > 2 || alivePlayers < 0 then
			println("Invalid players number, try again")
			setPlayersNumber()
		else alivePlayers

	def test(): Unit =
		testMove(0, 4)
		testMove(1, 1)
		testMove(0, 1)
		testMove(1, 0)
		testMove(0, 2)
		testMove(1, 5)
		testMove(0, 3)
		testMove(1, 3)
		testMove(0, 0)
		testMove(1, 3)

		testMove(0, 5)
		testMove(1, 2)
		testMove(0, 1)
		testMove(1, 3)
		testMove(0, 2)
		testMove(1, 0)
		testMove(0, 4)
		testMove(1, 1)
		testMove(0, 5)
		testMove(1, 4)

		testMove(0, 1)
		testMove(1, 2)
		testMove(1, 4)
		testMove(0, 0)
		testMove(1, 1)
		testMove(0, 4)
		testMove(0, 2)
		testMove(1, 3)
		testMove(0, 5)
		testMove(1, 0)

		testMove(0, 0)
		testMove(1, 2)
		testMove(0, 3)
		testMove(1, 4)
		testMove(0, 4)
		testMove(0, 1)
		testMove(1, 1)
		testMove(0, 5)
		testMove(0, 2)
		testMove(1, 3)

		testMove(0, 3)
		testMove(1, 5)
		testMove(0, 4)
		testMove(0, 3)
		testMove(1, 0)
		testMove(0, 2)
		testMove(1, 1)
		testMove(0, 1)
		testMove(1, 2)
		testMove(0, 5)
