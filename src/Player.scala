abstract class Player() extends gameLogic{
  def ReturnMove(): Int
  def Possibilities(startMapState: Array[Array[Int]],player: Int): Unit
  def searchingNeed(): Boolean
}

trait gameLogic{
  def MakeMove(newCurrentMapState: Array[Array[Int]], player: Int, move: Int): (Array[Array[Int]],Int) =
    var currentMapState = Array.fill(2)(Array.fill(7)(4))
    for(i <- 0 to 1){
      for(j <- 0 to 6){
        currentMapState(i)(j) = newCurrentMapState(i)(j)
      }
    }

    if currentMapState(player)(move) == 0 then { println("Błąd!!! Nie można się tak ruszyć!!!"); (currentMapState,player) }
    else
      var leftStones = currentMapState(player)(move)
      currentMapState(player)(move) = 0
      var successor = move
      var line = player
      var enemyPlayer = 0
      if player == 0 then enemyPlayer = 1 else enemyPlayer = 0
      var nextPlayer = enemyPlayer

      while(leftStones > 0){
        if line == 0 then successor = successor - 1
        else if successor == 0 then {successor = 6; line = 0}
        else successor = successor + 1
        successor match
          case 0 => if player != line then successor = 1; line = player
          case 7 => if player != line then {successor = 6; line = player; } else {successor = 0; line = 1}
          case -1 => if line == 0 then {successor = 1; line = 1 }else {successor = 6; line = 0}
          case n =>

        if successor != 0 || line == player then
          currentMapState(line)(successor) = currentMapState(line)(successor) + 1
          leftStones = leftStones -1

        // println("Succesor: " + successor + "   Left Stones: " + leftStones + "  Line: " +line)

        if leftStones == 0 && line == player && currentMapState(line)(successor) == 1 && successor != 0 && currentMapState(enemyPlayer)(successor) != 0then
          currentMapState(player)(0) = currentMapState(player)(0) + currentMapState(player)(successor) + currentMapState(enemyPlayer)(successor)
          currentMapState(player)(successor) = 0
          currentMapState(enemyPlayer)(successor) = 0

        if leftStones == 0 && line == player && successor == 0 then nextPlayer = player
        //PrintArray(currentMapState)
      }

      (currentMapState,nextPlayer)

  def CheckAvailableMoves(currentMapState: Array[Array[Int]], player: Int): List[Int] =
    // @tailrec
    def CheckAvailableMovesRec( pitsNumber: Int): List[Int] =
      if pitsNumber < 7 then
        if currentMapState(player)(pitsNumber) > 0 then pitsNumber::CheckAvailableMovesRec(pitsNumber+1) else CheckAvailableMovesRec(pitsNumber+1)
      else
        List()
    CheckAvailableMovesRec(1)

  def IsOver(CurrentMapState: Array[Array[Int]]): Boolean =
    def pitsCheck(list: List[Int]): Boolean =
      list match
        case List() => true
        case h::t => if h != 0 then false else pitsCheck(t)
    pitsCheck(CurrentMapState(0).toList.tail) || pitsCheck(CurrentMapState(1).toList.tail)

  def GetWinner(CurrentMapState: Array[Array[Int]]): Int ={
    def pitsCounting(list: List[Int]): Int =
      list match
        case List() => 0
        case h::t => h + pitsCounting(t)
    if pitsCounting(CurrentMapState(0).toList) > pitsCounting(CurrentMapState(1).toList)
    then return 1
    else if pitsCounting(CurrentMapState(0).toList) < pitsCounting(CurrentMapState(1).toList)
    then return 2
    else return 0
  }

  def EndingMap(CurrentMapState: Array[Array[Int]]): Unit =
    def pitsCounting(iterator: Int, player: Int): Unit =
      if iterator < 7
      then{ CurrentMapState(player)(0) = CurrentMapState(player)(0) + CurrentMapState(player)(iterator);CurrentMapState(player)(iterator) = 0; pitsCounting(iterator+1,player)}
    pitsCounting(1,0)
    pitsCounting(1,1)

  def PrintArray(currentMapState: Array[Array[Int]]): Unit =
    println(" ")
    println("      Player 1      ")
    println("--------------------")
    print("|  |")
    for(n <- currentMapState(0).slice(1, 7).toList)print(n +" ")
    println("|  |")
    println("| "+currentMapState(0)(0)+"|            | "+currentMapState(1)(0) + "|")
    print("|  |")
    for(n <- currentMapState(1).slice(1, 7).toList)print(n + " ")
    println("|  |")
    println("--------------------")
    println("      Player 2      ")
    println(" ")
}