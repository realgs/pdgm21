class User(val board : Board) extends Player {

  def move() : Int = {
    println("Podaj pole od 0 do 5")

    val number = scala.io.StdIn.readInt()
    if (number >= 0 && number <=5){
      if(board.findActivePlayer()) number
      else number + 7
    } else {
      println("Błędny numer pola")
      move()
    }
  }
}

