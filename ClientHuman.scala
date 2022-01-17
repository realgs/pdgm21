import scala.io.StdIn.readLine

class ClientHuman(name: String, isFirst:Boolean) extends Client(name, isFirst){

  def setIsFirst(value:Boolean): Unit = {
    isFirst = value
  }
  

  def chooseField(kalah: Kalah): Int = {
    var choice = -1
    println("YOU ARE: " + name)
    if (name == "player1") {
      println("Choose hole: [0 - 5]")
    } else {
      println("Choose hole  [7 - 12]")
    }

    try {
      choice = readLine.toInt
    } catch {
      case e => NumberFormatException
        println("This is not the number")
    }

    if (name == "player1" && choice > 6 || choice < 0) {
      chooseField(kalah)
      println("WRONG NUMBER. Try again!")
    } else if (name == "player2" && choice < 7 || choice < 0) {
      chooseField(kalah)
    }

    if (choice >= 13 || choice == 6 || kalah.checkIfEmpty(choice)) {
      println("WRONG NUMBER. Try again!")
      chooseField(kalah)
    } else {
      choice
    }
    
    
  }






}
