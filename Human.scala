class Human extends Player {
  override def makeMove(board: Board): Int = {
    println(s"You are player ${if(board.isMoveFirstPlayer) then "first choos from 1 to 6" else "second choos from 7 to 12"}")
    
    var number = choiceNumber(board)
    
    return number
  }
  
  def choiceNumber(board: Board): Int ={
    println("\nChoose a hold:")
    
    var choice = scala.io.StdIn.readInt()

    if (board.isMoveFirstPlayer) then {
      if (choice < 1 || choice > 6) then {
        println(s"Incorect number: $choice")
        choice = choiceNumber(board)
      }
    } else if (choice < 7 || choice > 12) {
      println(s"Incorect number: $choice")
      choice = choiceNumber(board)

    }

    if(board.board(choice -1) == 0 && board.isMoveFirstPlayer){
      println(s"Incorect number1: $choice == 0")
      choice = choiceNumber(board)
    }

    if(board.board(choice) == 0 && !board.isMoveFirstPlayer){
      println(s"Incorect number1: $choice == 0")
      choice = choiceNumber(board)
    }
    return choice
  }
}

