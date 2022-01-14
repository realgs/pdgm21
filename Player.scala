import scala.io.StdIn.*

class Player(id: Int) {

  def makeMove(): Int ={

    print("\nSelect field number: ")
    val field = readInt()
    field - 1

  }
  
  def getId: Int = id
}
