import scala.concurrent.ExecutionContext

class Server(private var listOfPlayers: List[Human]) extends gameLogic {
  private var engine = new Enginee()
  private var engine2 = new Enginee()

  def Main(): Unit ={
    var stop = false
    var firstPlayer: Human = null
    while(!stop){
      Thread.sleep(100)
      var i = 0
      while (i<listOfPlayers.length){
        if listOfPlayers(i).GetWantPlay() == 1
        then
          if firstPlayer == null
          then firstPlayer = listOfPlayers(i)
          else {println("1  classes: " + firstPlayer.getClass.toString + " " + listOfPlayers(i).getClass.toString);Play(firstPlayer,listOfPlayers(i))}
        else if listOfPlayers(i).GetWantPlay() == 2
        then {println("2  classes: " + firstPlayer.getClass.toString + " " + listOfPlayers(i).getClass.toString);Play(listOfPlayers(i),engine)}
        else if listOfPlayers(i).GetWantPlay() == 3
        then {println("3  classes: " + firstPlayer.getClass.toString + " " + listOfPlayers(i).getClass.toString);Play(engine,engine2)}

        i = i+1
      }
    }
  }

  def Play(player0: Player, player1: Player): Unit =
    var map = Array.fill(2)(Array.fill(7)(4))
    map (0)(0) = 0
    map (1)(0) = 0
    var currentPlayer = 0;
    while(!IsOver(map)){
      val ectx = ExecutionContext.global
      PrintArray(map)
      if currentPlayer == 0
      then
        println("\nPlayer 1 move!")
        ectx.execute(() => player0.Possibilities(map,currentPlayer))
      else
        println("\nPlayer 2 move!")
        ectx.execute(() => player1.Possibilities(map,currentPlayer))

      val start = System.nanoTime()
      var end = System.nanoTime()

      while ((end - start)/1000000000 < 30) {
        Thread.sleep(100)
        end = System.nanoTime()

        if currentPlayer == 0
        then
          if !player0.searchingNeed() then end = start + System.nanoTime() + 1000000000*30
        else
          if !player1.searchingNeed() then end = start + System.nanoTime() + 1000000000*30
      }


      //   println("O BOGOWIE")

      if currentPlayer == 0
      then
        val move = MakeMove(map,currentPlayer,player0.ReturnMove())
        map = move._1
        currentPlayer = move._2
      else
        val move = MakeMove(map,currentPlayer,player1.ReturnMove())
        map = move._1
        currentPlayer = move._2
      Thread.sleep(100)
    }
    PrintArray(map)
    println("-------------------GAMEOVER----------------------")
    if GetWinner(map) == 0
    then println("REMIS")
    else println("Player  " + GetWinner(map) + " have won!!!")
    EndingMap(map)
    PrintArray(map)
    println("-------------------------------------------------")

    if player0.getClass.toString == "class Human" then player0.asInstanceOf[Human].Start()
    if player1.getClass.toString == "class Human" then player0.asInstanceOf[Human].Start()

}