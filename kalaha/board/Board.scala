package board

class Board(private val houseNr: Int, private val stoneNr: Int) {

  private var _player1Base = 0
  private var _player2Base = 0
  private var _player1Houses = new Array[Int](houseNr)
  private var _player2Houses = new Array[Int](houseNr)

  for(i <- 0 until houseNr)
    _player2Houses(i) = stoneNr
    _player1Houses(i) = stoneNr

  def printBoard(): Unit =
    println()
    print("Player2\t\t # \t")
    _player2Houses.reverse.foreach(house => print(house + "\t"))
    println(" #\t\tPlayer1")
    println("\t" + _player2Base + "\t\t # \t\t\t\t\t\t\t # \t\t" + _player1Base)
    print("\t\t\t # \t")
    _player1Houses.foreach(house => print(house + "\t"))
    println(" #")

  def player1Base: Int =
    _player1Base

  def player2Base: Int =
    _player2Base

  def player1Base_=(base: Int): Unit =
    _player1Base = base

  def player2Base_=(base: Int): Unit =
    _player2Base = base

  def player1Houses: Array[Int] =
    _player1Houses
  
  def player1Houses_=(houses: Array[Int]): Unit =
    _player1Houses = houses

  def player2Houses: Array[Int] =
    _player2Houses
  
  def player2Houses_=(houses: Array[Int]): Unit =
    _player2Houses = houses
}
