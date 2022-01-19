package game

class Board {

  var array = new Array[Int](14)
  var isFirstPlayerTurn: Boolean = true

  def initializeBoard(): Array[Int] =
    for(i <- 0 until 14)
      if ((i + 1) % 7 != 0)
        array(i) = 6
      else
        array(i) = 0
    array

  def displayBoard(): Unit =
    println()
    print("          ")
    for(i <- (12 to 7 by -1))
      print("[" + array(i) + "]")
    println("          ")

    println("[" + array(13) + "]              " +
      "                  [" + array(6) + "]")

    print("          ")
    for(i <- 0 until 6)
      print("[" + array(i) + "]")
    println("          ")
    println()

  def isChosenPitCorrect(pitNumber: Int): Boolean =
    if (isFirstPlayerTurn && pitNumber >= 0 &&
        pitNumber <= 5 &&
        array(pitNumber) != 0)
      return true
    else if (!isFirstPlayerTurn && pitNumber >= 7 &&
        pitNumber <= 12 &&
        array(pitNumber) != 0)
      return true
    return false





}
