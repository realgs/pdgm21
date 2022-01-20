import scala.annotation.tailrec

object Interface {

  @tailrec
  def space(i:Int): Unit =
    print(" ")
    if(i>0) then space(i-1)

  def showBoard(board: KalahaSim): Unit =
    print("\u001b[2J\u001b[;H")
    println("### Player 2 ###")
    var len = 2
    @tailrec
    def step2(poz:Int, p1:Boolean): Unit =
      print("  ("+board.at(poz)+")")
      if !p1 then len = len + 4 + board.at(poz).toString().length
      if p1 && poz < 5 then step2(poz+1, p1) else if !p1 && poz > 7 then step2(poz-1, p1)
    space(board.at(13).toString().length+2)
    step2(12, false)
    print("\n["+board.at(13)+"]")
    space(len+1)
    println("["+board.at(6)+"]")
    space(board.at(13).toString().length+2)
    step2(0, true)
    println("\n### Player 1 ###")

  def requestMove(player: Player): Int =
    player.moveOwn()

  def victory(victor:Int): Unit =
    print("\n\n $$$$    ")
    if victor == 0 then println("The match has ended in a draw!    $$$$")
    else println("Player " + victor + " has emerged victorious! Congratulations!    $$$$")
    Thread.sleep(5000)
    

  def missTurn(): Unit =
    print("You missed your turn Player ")
    
  def playerRequest(p1:Boolean): Unit =
    print("\nPlayer " + (if p1 then 1 else 2) + ", please provide from which position you wish to move next: ")  
    
  def readMove(): Int = scala.io.StdIn.readInt()
  
  def computerMove(p1:Boolean, move:Int): Unit = println("Computer player " + (if p1 then 1 else 2) + " moved from position " + move + ".")

  def mainMenu(): Unit =
    System.out.flush()
    println("Welcome to Kalaha!\n1. Simulate bot match\n2. Human vs Computer\n3. Computer vs Human\n4. Human vs Human (WARNING! BETA, still bugged!)\n0. Exit")
    def read(): Int =
      try
        val x = scala.io.StdIn.readInt()
        if x>=0 && x<=4 then x else read()
      catch
        case e: java.lang.NumberFormatException => read()
    val x = read()
    x match
      case 1 =>
        val player1 = AIplayer(true)
        val player2 = AIplayer(false)
        val server = Server(player1, player2)
        server.start()
      case 2 =>
        val player1 = HumanPlayer(true)
        val player2 = AIplayer(false)
        val server = Server(player1, player2)
        server.start()
      case 3 =>
        val player2 = HumanPlayer(false)
        val player1 = AIplayer(true)
        val server = Server(player1, player2)
        server.start()
      case 4 =>
        val player1 = HumanPlayer(true)
        val player2 = HumanPlayer(false)
        val server = Server(player1, player2)
        server.start()
      case 0 => println("Goodbye!")
}
