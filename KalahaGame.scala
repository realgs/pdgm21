import scala.util.Random

class KalahaGame() {

  var gameBoard = new Board()

  def whoStarts() =
    val random = new Random()
    val number = random.between(1, 3)
    number

  def makeMove(currentPlayer: Int, chosenCup: Int): Int =

    if gameBoard.getStones(chosenCup) == 0 then return -3

    var stones = gameBoard.getStones(chosenCup)
    gameBoard.setStones(chosenCup, 0)
    var cupToDrop = (chosenCup + 1) % 14

    while (stones > 0)
      {
        if playerNotInOppositeBase(currentPlayer, cupToDrop) then
        {
          stones-=1

          if stones == 0 then
            {
              if playerInOwnBase(currentPlayer, cupToDrop) then
                {
                  gameBoard.addStone(cupToDrop)
                  if !gameFinished() then return 1 // extra move
                  else return -1 // end of the game
                }
              else if gameBoard.getStones(cupToDrop) == 0 then
              {
                if playerInOwnHalf(currentPlayer, cupToDrop) then
                  {
                    if gameBoard.getStones(12-cupToDrop) != 0 then
                      {
                        capture(currentPlayer, cupToDrop)
                        gameBoard.setStones(cupToDrop, 0)
                        return 2 // capture
                      }
                  }
              }
            }
        }
        if playerNotInOppositeBase(currentPlayer, cupToDrop) then gameBoard.addStone(cupToDrop)
        cupToDrop = (cupToDrop + 1) % 14
      }
      return 0

  def capture(currentPlayer: Int, index: Int) =
    val capturedCup = 12 - index
    val capturedStones = gameBoard.getStones(capturedCup)
    gameBoard.setStones(capturedCup, 0)

    if currentPlayer == 1 then gameBoard.setStones(6, gameBoard.getStones(6)+capturedStones+1)
    else gameBoard.setStones(13, gameBoard.getStones(13)+capturedStones+1)

  def gameFinished(): Boolean =
    var end = true

    for (i <- 0 to 5)
      if gameBoard.getStones(i) != 0 then end = false

    if end then return end

    for (i <- 7 to 12)
      if gameBoard.getStones(i) != 0 then return false
      else end = true

    end

  def playerNotInOppositeBase(player: Int, cup: Int) = player == 1 && cup != 13 || player == 2 && cup != 6

  def playerInOwnBase(player: Int, cup: Int) = player == 1 && cup == 6 || player == 2 && cup == 13

  def playerInOwnHalf(player: Int, cup: Int) = (player == 1 && cup >= 0 && cup <= 5) || (player == 2 && cup >=7 && cup <= 12)

  def gameStatus(currentPlayer: Player): Int =
    if gameFinished() then -1
    else currentPlayer.getID()

  def gameResults() =
    println("Game over!")
    gameBoard.printBoard()

    val (score1, score2) = score()

    println(s"Score: $score1 : $score2")

    if score1 > score2 then println("Winner: Player 1")
    else if score1 < score2 then println("Winner: Player 2")
    else println("It's a draw!")

  def score() =
    var score1 = gameBoard.getStones(6)
    var score2 = gameBoard.getStones(13)

    for (i <- 0 to 5)
      score1+=gameBoard.getStones(i)

    for (i <- 7 to 12)
      score2+=gameBoard.getStones(i)

    (score1, score2)

  def advantage(playerIndex: Int) =
    if playerIndex == 1 then gameBoard.getStones(6)-gameBoard.getStones(13)
    else gameBoard.getStones(13)-gameBoard.getStones(6)


}
