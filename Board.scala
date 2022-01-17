package Kalah

class Board (seeds: Int) {
  private val board: Array[Int] = createBoard(seeds)

  def getBoard: Array[Int] = board

  def createBoard(seeds: Int):Array[Int]={
    val arrayBoard = new Array[Int](14)
    for (x <- 0 to 13)
      if x == 0 || x == 7 then
        arrayBoard(x)=0
      else
        arrayBoard(x)=seeds
    arrayBoard
  }
}
