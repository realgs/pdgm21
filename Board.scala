class Board {
  var Player1Row: Array[Int] = Array(0,0,0,0,0,0)
  var Player1House: Int = 0
  var Player2Row: Array[Int] = Array(0,0,0,0,0,0)
  var Player2House: Int = 0
  val ALL_STONES = 72
  var Player1: Player
  var Player2: Player

  def printRow(values: Array[Int])={
    var i=0;
    while(i<values.size){
      print("["+values(i)+"]")
    }
    println("")
  }

  def Board(Player1: Player, Player2: Player): Unit ={
    this.Player1 = Player1
    this.Player2 = Player2
    var temp : Int = ALL_STONES/12
    Player1Row = Array(temp,temp,temp,temp,temp,temp)
    Player1House = 0
    Player2Row = Array(temp,temp,temp,temp,temp,temp)
    Player2House = 0
    printBoard()
  }

  def printBoard(): Unit ={
    Runtime.getRuntime.exec("cls")
    println(Player1.getName())
    printRow(Player1Row)
    print("("+Player1House+")"+"                  "+"("+Player2House+")")
    printRow(Player2Row)
    println(Player2.getName())
  }

  def endOfTheGame(): Boolean ={
    if(Player1House>ALL_STONES/2 || Player2House>ALL_STONES/2) then true
    else false
  }
}
