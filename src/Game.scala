import java.util.Scanner
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class Game:
	var board = new Board(6, this)

	def gameEnd(result: Int): Unit =
		result match
			case 0 => println("REMIS")
			case 1 => println("PLAYER A WON")
			case 2 => println("PLAYER B WON")

	def printBoard(): Unit =
		println(board.boardToString())

	def play() =
		var activePlayer = 0
		while !board.anySideClear() do
			move(activePlayer)
			activePlayer = enemyPlayer(activePlayer)
			board.checkGameEnd(activePlayer)

	def enemyPlayer(player: Int): Int =
		if player == 0 then 1 else 0

	def chooseHole(): Int =
		val scanner = new Scanner(System.in)
		println("Write number [0-5] to choose hole")
		val hole = scanner.nextLine().toInt
		if hole < 0 || hole > 5 then
			println("Invalid hole number")
			chooseHole()
		else hole

	def move(player: Int) =
		printBoard()
		println(s"PLAYER: ${player} MOVE")
		val hole = chooseHole()
		/*if board.getStonesNumberAtPos(board.playerAndPosToBoardPos(player, hole)) ==0 then
			println("Empty hole")
			chooseHole()*/
		board.move(player, hole)

	def start(): Unit =
		println("HELLO")
		println("IT'S KALAHA GAME\n")
		//setup()
		play()

	def setup(): Unit =
		val players = setPlayersNumber()
		val computers = 2 - players
		if players == 2 then
			println("Two players")
		else if players == 1 then
			println("")
		else
			println("")
	//2 computery
	//let's play

	def setPlayersNumber(): Int =
		val scanner = new Scanner(System.in)
		println("How many live players would you start with? [0-2] \n Computer numbers will be set automatically")
		var alivePlayers = scanner.nextLine().toInt
		if alivePlayers > 2 || alivePlayers < 0 then
			println("Invalid players number, try again")
			setPlayersNumber()
		else alivePlayers

	def test(): Unit =
		board.move(0, 4)
		board.move(1, 1)
		board.move(0, 1)
		board.move(1, 0)
		board.move(0, 2)
		board.move(1, 5)
		board.move(0, 3)
		board.move(1, 3)
		board.move(0, 0)
		board.move(1, 3)

		board.move(0, 5)
		board.move(1, 2)
		board.move(0, 1)
		board.move(1, 3)
		board.move(0, 2)
		board.move(1, 0)
		board.move(0, 4)
		board.move(1, 1)
		board.move(0, 5)
		board.move(1, 4)

		board.move(0, 1)
		board.move(1, 2)
		board.move(1, 4)
		board.move(0, 0)
		board.move(1, 1)
		board.move(0, 4)
		board.move(0, 2)
		board.move(1, 3)
		board.move(0, 5)
		board.move(1, 0)

		board.move(0, 0)
		board.move(1, 2)
		board.move(0, 3)
		board.move(1, 4)
		board.move(0, 4)
		board.move(0, 1)
		board.move(1, 1)
		board.move(0, 5)
		board.move(0, 2)
		board.move(1, 3)

		board.move(0, 3)
		board.move(1, 5)
		board.move(0, 4)
		board.move(0, 3)
		board.move(1, 0)
		board.move(0, 2)
		board.move(1, 1)
		board.move(0, 1)
		board.move(1, 2)
		board.move(0, 5)
