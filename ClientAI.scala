import scala.io.StdIn.readLine

class ClientAI(name:String, isFirst:Boolean) extends Client (name, isFirst) {


  def setIsFirst(value:Boolean): Unit = {
    isFirst = value
  }
  
  def chooseField(kalah: Kalah):Int = {
    var choice = 0
    var kalah1: Kalah = new Kalah()

    var index = 0;
    var index2 = 7
    var max = 0;

    if (name == "player1") {
      while (index < 6) {
        kalah1 = kalah.copyBoard()

        if (kalah1.getBoard()(index).getStones() == 0) {
          index +=1
        } else {
          var (value, user) = chooseTheBestValue(kalah1, index)
          if (user == "player1") {
            choice = index
          } else if (value > max) {
            max = value
            choice = index
          }
          
          index += 1
        }
      }
    } else if (name == "player2") {

      while (index2 < 13) {
        kalah1 = kalah.copyBoard()

        if (kalah1.getBoard()(index2).getStones() == 0) {
          index2 +=1
        } else {
          var (value,user) = chooseTheBestValue(kalah1, index2)
          if (user == "player2") {
            choice = index2
          } else if (value > max) {
            max = value
            choice = index2
          }
          
          index2 += 1
        }
      }
    }
    println("YOU ARE: " + name)
    println("INDEX: "+ choice)
    choice
  }

  def chooseTheBestValue(kalah: Kalah, index: Int): (Int, String) = {
    var kalah1: Kalah = kalah.copyBoard()
    var user = kalah1.makeMove(index, name)

    var score = checkScore(kalah1.getBoard())
    (score, user)
  }

  def checkScore(array:Array[Node]): Int = {
    if name == "player1" then array(6).getStones()
    else array(13).getStones()
  }
  



}
