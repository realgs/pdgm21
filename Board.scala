class Board {

  var board: Array[Int] = new Array(14)

  for (i <- 0 to 13)
    if i == 6 || i == 13 then board(i) = 0
    else board(i) = 4

  def isStartingPosition(): Boolean =
    var start = true

    for (i <- 0 to 5)
      if getStones(i) != 4 then start = false

    if !start then return start

    for (i <- 7 to 12)
      if getStones(i) != 4 then start = false

    start

  def getStones(index: Int) = board(index)

  def addStone(index: Int) = board(index)+=1

  def setStones(index: Int, amount: Int) = board(index) = amount

  def printBoard() =
    print("\n      ")

    for (i <- 12 to 7 by -1)
      print("(" + board(i) + ")  ")

    println()

    print(board(13) + "                                      " + board(6))

    println()

    print("      ")

    for (i <- 0 to 5)
      print("(" + board(i) + ")  ")

    println()
    println()
}
