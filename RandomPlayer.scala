import scala.io.StdIn.readInt
import scala.util.Random

class RandomPlayer(private val id: Int, private val game: Kalaha) extends Player(id) {

  override def makeMove(time: Int): Int ={

    var isOk = false
    var choice = 0
    while(!isOk){

      choice = Random.nextInt(6)
      if game.getField(id)(choice) != 0 then isOk = true

    }
    choice
  }

  override def getId: Int = id

  override def getEnemyMove(move: Int): Unit = ()
}
