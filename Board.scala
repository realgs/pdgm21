class Board {

  private val board: Array[Int] = Array.fill(14)(4)
  board(13) = 0
  board(6) = 0


  /*

  Codes returned by def move:
  0 => next player moves
  1 => this player has additional move
  2 => wrong move

  */

  def move(house: Int, first: Boolean): Int =
    var code = 0

    if !checkHouse(house, first) then code = 2
    else
      var numberOfStones = board(house)
      board(house) = 0
      var nextHouse = house

      while numberOfStones > 0 do
        nextHouse = (nextHouse + 1) % 14
        if (first && nextHouse == 13) || (!first && nextHouse == 6) then nextHouse = (nextHouse + 1) % 14
        board(nextHouse) = board(nextHouse) + 1
        numberOfStones = numberOfStones - 1


      if lastStoneInBase(first, nextHouse) then code = 1
      else takeOppositeStones(first, nextHouse)

    code


  def lastStoneInBase(first: Boolean, lastHouse: Int): Boolean =
    if first && lastHouse == 6 then true
    else if !first && lastHouse == 13 then true
    else false


  def takeOppositeStones(first: Boolean, lastHouse: Int): Boolean =
    if board(lastHouse) == 1 && board(12 - lastHouse) != 0 then
      if first && lastHouse <= 5 then
        board(6) = board(6) + board(12 - lastHouse) + 1
        board(12 - lastHouse) = 0
        board(lastHouse) = 0
        true
      else if !first && lastHouse >= 7 then
        board(13) = board(13) + board(12 - lastHouse) + 1
        board(12 - lastHouse) = 0
        board(lastHouse) = 0
        true
      else false
    else false



  def checkHouse(house: Int, first: Boolean): Boolean =
    if house < 0 || house > 13 then false
    else if house == 6 || house == 13 then false
    else if board(house) == 0 then false
    else if first then house <= 5
    else house >= 7



  def possibleMoves(first: Boolean): List[Int] =
    var list = List[Int]()
    if first then
      for (i <- 0 to 5)
        if board(i) != 0 then list = i :: list
        else ()
    else
      for (i <- 7 to 12)
        if board(i) != 0 then list = i :: list
        else ()
    list


  def endOfGame(first: Boolean): Unit =
    if first then
      for (i <- 7 to 12)
        board(13) = board(13) + board(i)
        board(i) = 0
    else
      for (i <- 0 to 5)
        board(6) = board(6) + board(i)
        board(i) = 0




  def isEndOfGame(first: Boolean): Boolean =
    if possibleMoves(first) == Nil then true
    else false



  def printWhoWin(): Unit =
    println()
    println("##### Results #####" )
    printf("%n%8s %3s %8s %n", "player1", "vs", "player2")
    printf("%n%4d %8s %4d %n%n", board(6), "", board(13))

    if board(6) > board(13) then println("Player1 won !!!")
    else if board(6) == board(13) then println("Draw !!!")
    else println("Player2 won !!!")


  def countDifference(first: Boolean): Int =
    if first then board(6) - board(13)
    else board(13) - board(6)


  override def clone(): Board =
    val newBoard = new Board()
    for(i <- 0 to 13)
      newBoard.board(i) = board(i)
    newBoard

  def printBoard(): Unit =
    println()
    printf("%20s %n", "Player2")
    printf("%1s%4s %4s %4s %4s %4s %4s %n", " ", "12", "11", "10", "9", "8", "7")
    print(" ")
    for(i <- 0 to 31)
      print("-")
    println()
    printf("%1s%4d %4d %4d %4d %4d %4d%3s%1s %n", "|", board(12), board(11), board(10), board(9), board(8), board(7), " ", "|")
    printf("%1s%4d %19s %4d%3s%1s %n", "|", board(13), "", board(6), " ", "|")
    printf("%1s%4d %4d %4d %4d %4d %4d%3s%1s %n", "|", board(0), board(1), board(2), board(3), board(4), board(5), " ", "|")
    print(" ")
    for(i <- 0 to 31)
      print("-")
    println()
    printf("%1s%4s %4s %4s %4s %4s %4s %n", " ", "0", "1", "2", "3", "4", "5")
    printf("%20s %n%n", "Player1")

}
