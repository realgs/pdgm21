class GameState private(private var side: Int, private var gameOver: Boolean, private val position: Array[Int]) {
  private def this(gameState: GameState) =
    this(gameState.side, gameState.gameOver, gameState.position.clone())


  def sideToMove = side
  def pos = position
  def houses = position.length/2 - 1

  def score(player: Int): Int = {
    var score = 0

    for(i <- 0 to houses)
      score = score + position(i + player*position.length/2)

    score
  }

  def updatePos(move: Int): Boolean = {
    var i = move
    val playerEndZone = (side + 1) * position.length/2 - 1
    val opponentEndZone = (2 - side) * position.length/2 - 1

    while (position(move) > 0) {
      i = (i + 1) % position.length
      if i != opponentEndZone then
        position(i) = position(i) + 1
        position(move) = position(move) - 1
    }

    if i != playerEndZone then {
      side = 1 - side
      if position(i) == 1 && (i >= (1 - side) * position.length/2 && i < (2 - side) * position.length/2) then
        position(playerEndZone) = position(playerEndZone) + position(i) + position(position.length - 2 - i)
        position(i) = 0
        position(position.length - 2 - i) = 0
    }

    gameOver = isGameOver

    gameOver
  }

  def isGameOver: Boolean = {
    var c = 0
    for(i <- 0 to houses - 1)
      c = c + position(i + side * position.length/2)

    c == 0
  }

  def winner: Int = {
    val s1 = score(0)
    val s2 = score(1)
    if s1 > s2 then 0
    else if s1 < s2 then 1
    else -1
  }

  def isMoveLegit(move: Int): Boolean = {
    !gameOver &&
      (move >= side * position.length/2 && move < (1 + side) * position.length/2 - 1) &&
      (move != position.length - 1 && move != position.length/2 - 1 && position(move) != 0)
    // game is not over
    // and move is on correct side
    // and it's not an empty house or end zone
  }

  def printRoud: Unit =
    println("player 2 side")
    for(i <- position.length - 1 to position.length/2 by -1)
      print(s"${position(i)} ")

    println()
    print("--")
    for(i <- 0 to position.length/2 - 1)
      print(s"${position(i)} ")
    println()
    println("player 1 side\n")
}

object GameState {
  def apply(houses: Int, seeds: Int): GameState =
    require(houses >= 1 && seeds >= 1)
    new GameState(0, false, (0 to 2*houses + 1).map{ e =>
      if e == houses || e == 2*houses + 1 then 0
      else seeds
    }.toArray)

  def copy(gameState: GameState): GameState =
    new GameState(gameState)
}
