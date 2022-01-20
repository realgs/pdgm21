import scala.annotation.tailrec

class IllegalMoveException(message: String) extends Exception(message)
class KalahaBoard(
                   val housesPerSide: Int,
                   val initialSeedAmount: Int,
                   val player1Houses: Array[House],
                   val player2Houses: Array[House],
                   val player1Store: Store,
                   val player2Store: Store
                 ):

  def this(housesPerSide: Int, initialSeedAmount: Int) =
    this(
      housesPerSide,
      initialSeedAmount,
      Array.tabulate(housesPerSide)(i => House(initialSeedAmount, 1, i)),
      Array.tabulate(housesPerSide)(i => House(initialSeedAmount, 2, i)),
      Store(0, 1),
      Store(0, 2)
    )

  def copy() =
    new KalahaBoard(
      housesPerSide,
      initialSeedAmount,
      player1Houses.clone().map(house => house.copy()),
      player2Houses.clone().map(house => house.copy()),
      player1Store.copy(),
      player2Store.copy()
    )

  @tailrec
  private def nextPitToSow(playerId: Int)(currentPit: Pit): Pit =
    val maxHouseIndex = housesPerSide - 1
    currentPit match
      case Store(_, 1) => player2Houses(0)
      case Store(_, 2) => player1Houses(0)
      case House(_, 1, `maxHouseIndex`) => if playerId == 1 then player1Store else nextPitToSow(playerId)(player1Store)
      case House(_, 1, i) => player1Houses(i + 1)
      case House(_, 2, `maxHouseIndex`) => if playerId == 2 then player2Store else nextPitToSow(playerId)(player2Store)
      case House(_, 2, i) => player2Houses(i + 1)

  def houseByIds(playerId: Int, houseIndex: Int) = (if playerId == 1 then player1Houses else player2Houses)(houseIndex)

  def getOppositeHouse(house: House) =
    val House(_, playerId, index) = house
    (if playerId == 1 then player2Houses else player1Houses)(housesPerSide - index - 1)

  def getOppositePlayerId(playerId: Int) = if playerId == 1 then 2 else 1

  def scores = (player1Store.seeds, player2Store.seeds)

  def isMoveLegal(playerId: Int, houseIndex: Int) =
    if houseIndex < 0 || houseIndex > housesPerSide - 1 then
      false
    else
      val House(seeds, _, _) = houseByIds(playerId, houseIndex)
      seeds > 0

  //returns id of player that will be moving next
  def makeAMove(currentPlayer: Int, chosenMove: Int) =
    if !isMoveLegal(currentPlayer, chosenMove) then throw new IllegalMoveException(s"house $chosenMove is empty")
    val chosenHouse = houseByIds(currentPlayer, chosenMove)
    val seedsToSow = chosenHouse.clear()
    @tailrec
    def sow(currentPit: Pit, seedsLeft: Int): Int =
      currentPit.increment()
      if seedsLeft > 1 then
        sow(nextPitToSow(currentPlayer)(currentPit), seedsLeft - 1)
      else
        currentPit match
          case Store(_, _) => currentPlayer
          case House(1, `currentPlayer`, _) =>
            val oppositeHouse = getOppositeHouse(currentPit.asInstanceOf[House])
            val playersStore = if currentPlayer == 1 then player1Store else player2Store
            playersStore.add(oppositeHouse.clear() + currentPit.clear())
            getOppositePlayerId(currentPlayer)
          case _ => getOppositePlayerId(currentPlayer)


    sow(nextPitToSow(currentPlayer)(chosenHouse), seedsToSow)

  def collectAllSeeds(playerId: Int) =
    val playerHouses = if playerId == 1 then player1Houses else player2Houses
    val allSeeds = (
      for(
        i <- playerHouses
      ) yield i.clear()
    ).sum
    (if playerId == 1 then player1Store else player2Store).add(allSeeds)

  def endGame(): Boolean =
    val canPlayer1Move = !player1Houses.forall(house => house.seeds == 0)
    if !canPlayer1Move then collectAllSeeds(2)
    val canPlayer2Move = !player2Houses.forall(house => house.seeds == 0)
    if !canPlayer2Move then collectAllSeeds(1)
    !canPlayer1Move || !canPlayer2Move

  def printBoard() =
    val stringBuilder = new StringBuilder()
    stringBuilder ++= Console.WHITE
    stringBuilder += ' ' ++= ((housesPerSide - 1) to 0 by -1).mkString("  ") += '\n'
    stringBuilder ++= Console.BLUE
    stringBuilder += '(' ++= player2Houses.map(_.seeds).reverse.mkString(")(") += ')' += '\n'
    stringBuilder += ' '++= player2Store.seeds.toString ++= "\t\t\t\t" ++= Console.GREEN ++= player1Store.seeds.toString += '\n'
    stringBuilder += '(' ++= player1Houses.map(_.seeds).mkString(")(") += ')' += '\n'
    stringBuilder ++= Console.WHITE
    stringBuilder += ' ' ++= (0 until housesPerSide).mkString("  ")
    stringBuilder ++= Console.RESET
    println(stringBuilder.result())