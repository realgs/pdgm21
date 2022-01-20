package kalaha

import scala.util.Random

class RandomAI(var holeMin: Int, var holeMax: Int, var server: Server) extends Player {
  var base: Int = holeMax

  def makeMove(): List[Int] =
    var requests = List[Int]()
    var holeRequest = getRandomHole
    while (checkIfNotEmpty(holeRequest)) {
      holeRequest = getRandomHole
    }
    requests = requests ::: List(holeRequest)
    while (server.requestMove(holeRequest) == 1 && !server.isOver) {
      while (checkIfNotEmpty(holeRequest)) {
        holeRequest = getRandomHole
      }
      requests = requests ::: List(holeRequest)
    }
    requests

  def checkIfNotEmpty(hole: Int): Boolean =
    server.getBoardArray(hole - 1) == 0

  def getRandomHole: Int =
    new Random().nextInt(6) + holeMin + 1
}
