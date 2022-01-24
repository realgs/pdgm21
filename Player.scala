import scala.io.StdIn.*

class Player(private val id: Int) {

  def makeMove(time: Int): Int ={

    print("\nSelect field number: ")
    val field = readInt()
    field - 1
  }
  
  def getId: Int = id

  def getEnemyMove(move: Int): Unit = ()
}
