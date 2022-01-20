import scala.util.Random
abstract class Player {
  var name: String = ""
  def getName(): String={
    name
  }

  def move(): Int ={

  }
}

class SimulationPlayer extends Player{
  //override def getName(): String = super.getName()
  override def move(): Int={
    val random = new Random()
    val index = random.between(0,6)
    index
  }
}
