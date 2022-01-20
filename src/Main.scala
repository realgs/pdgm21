
import java.rmi.server.LogStream.log
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.{Executor, ForkJoinPool}
import scala.annotation.tailrec
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    var human = new Human()
    var human2 = new Human()
    var engine = new Enginee()
    var engine2 = new Enginee()

    println(human.getClass)

    var serwer = new Serwer(List(human,human2))

    human.Start()
    human2.Start()

    serwer.Main()




  }

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

class Enginee extends Player {

  private var results = Array.fill(6)(0)
  private var move = Array.fill(6)(0)
  private var parrarelControler = Array.fill(6)(1)
  private var stop = 0
  private var borderOfBadResult = 60000
  private var listOfAvialableMoves: List[Int] = List()
  private var howDeepShouldWeMine = 6*6*6*6*6*6*6

  @Override
  override def Possibilities(startMapState: Array[Array[Int]],player: Int): Unit =
    stop = 0
    val ectx = ExecutionContext.global
    val executor: Executor = new ForkJoinPool(6)
    for(i <- 1 to 6){
      results(i-1) = 0
      move(i-1) = -1
    }
    parrarelControler = Array.fill(6)(1)
    var synchronizedDataBase = new SynchronizedDataBase()

    listOfAvialableMoves = CheckAvailableMoves(startMapState,player)
    ectx.execute(() => mainThreadTask())
    for( i <- listOfAvialableMoves){
     // println("nowy wątek: " + i)
      move(i-1) = i
      executor.execute(() => threadTask(i-1,startMapState,player))
    }

    def threadTask(number: Int, startMapState: Array[Array[Int]],player: Int): Unit =
      //println("nowy wątek start: " + number)
      if synchronizedDataBase.getQueue(number).isEmpty
        then{ val helper = MakeMove(startMapState, player, number+1);
        synchronizedDataBase.getQueue(number).append(new Result(number+1,helper._1,helper._2))}

      var size = 0

      while(parrarelControler(number) == 1){
        if !synchronizedDataBase.getQueue(number).isEmpty
        then
          val currentResult = synchronizedDataBase.getQueue(number).dequeue()
          synchronizedDataBase.setQueueElementsQuantity(number,synchronizedDataBase.getQueueElementsQuantity(number) - 1)
          if currentResult != null then
            if player == 0
            then results(number) = results(number) + currentResult.GetMapState()(0)(0) - currentResult.GetMapState()(1)(0)
            else results(number) = results(number) + currentResult.GetMapState()(1)(0) - currentResult.GetMapState()(0)(0)

            if synchronizedDataBase.getQueueElementsQuantity(number) <  howDeepShouldWeMine
            then
              for( i <- CheckAvailableMoves(currentResult.GetMapState(),currentResult.GetNextPlayer())){
                var helper = MakeMove(currentResult.GetMapState(),currentResult.GetNextPlayer(),i)
                synchronizedDataBase.getQueue(number).append(new Result(i,helper._1,helper._2))
                synchronizedDataBase.setQueueElementsQuantity(number,synchronizedDataBase.getQueueElementsQuantity(number) + 1)
              }

          size = size +1;
          //println(" "+size+":"+number+" ")
          //print(number)


        //   then parrarelControler(number) = 0
        //println("QueueQuantity: "+ synchronizedDataBase.getQueueElementsQuantity(number))
      }
      //println("Thread: " + number + "  end")
//  2015538
    def mainThreadTask(): Unit =
      while(stop == 0){
        //println("Move:")
        for(i <- 0 to 5) {
          //print(move(i) +" " )
        }
        Thread.sleep(100)
          val minimus = GetMinimumAndMaximum(results)
          //println(results(minimus._1) + "  " +results(minimus._2))
          if results(minimus._2) - results(minimus._1) > borderOfBadResult && move(minimus._2) != move(minimus._1) && results(minimus._2) != 0
          then {parrarelControler(minimus._1) = 0;
            synchronizedDataBase.splitQueueBetweenTwo(minimus._2,minimus._1)
            move(minimus._1) = move(minimus._2)
            results(minimus._1) = results(minimus._2)
            synchronizedDataBase.setQueueElementsQuantity(minimus._1,synchronizedDataBase.getQueueElementsQuantity(minimus._2)/2 )
            synchronizedDataBase.setQueueElementsQuantity(minimus._2,synchronizedDataBase.getQueueElementsQuantity(minimus._2)/2 )
            parrarelControler(minimus._1) = 1
            executor.execute(()=>threadTask(minimus._1,startMapState,player))
          }
        if !searchingNeed()
        then {stop = 1
          for(i <- 0 to 5) {
            parrarelControler(i) = 0
          }
        }
      }
      //println("MAIN THREAD END")
      for(i <- 0 to 5) {
        parrarelControler(i) = 0
        synchronizedDataBase.getQueue(i).clear()
      }


  override def searchingNeed(): Boolean =
    var retrunValue = false
    val compareValue = move(0)
    for( i <- 1 to 5) {
      if compareValue != move(i) && move(i) != 0 then retrunValue = true
    }
    retrunValue

  def findBestMove(): Int =
    var resultMove = 0
    var result = Int.MinValue
    for( i <- listOfAvialableMoves){
      var quantityOfMoves = 0
      var valueOfMoves = 0
      for( j <- 0 to 5){
        if move(i-1) == move(j) && move(j) != -1 then{ quantityOfMoves= quantityOfMoves +1; valueOfMoves = valueOfMoves + results(j)}
      }
      //print("  ResultFIndBest: "+i + "  "+ valueOfMoves/quantityOfMoves)
      if valueOfMoves/quantityOfMoves > result && valueOfMoves != 0 then{ result = valueOfMoves/quantityOfMoves; resultMove = i-1}
    }
    for(i <- 0 to 5) {


      parrarelControler(i) = 0
    }
    //println("\nresult move: "+resultMove)
    move(resultMove)

  @Override
  override def ReturnMove(): Int =
    for(i <- 0 to 5) {
      //println("Return  " + i + "  Result: " + results(i) + "  Move: " +move(i) + "  ParrarelControler: " + parrarelControler(i) + "  List: " + listOfAvialableMoves)
      parrarelControler(i) = 0
    }
    stop = 1;
    findBestMove()

}

def GetMinimums(array: Array[Int]): (Int,Int) =
  var result: (Int,Int) = (0,0)
  for(i <- 1 to 5){
    if array(result._1) > array(i)  then result = (i,result._1)
    else if array(result._1) < array(i) && array(result._2) > array(i) then result = (result._1, i)
  }
  result

def GetMinimumAndMaximum(array: Array[Int]): (Int,Int) =
  var result: (Int,Int) = (0,0)
  for(i <- 1 to 5){
    if array(result._1) > array(i)  then result = (i,result._2)
    if array(result._2) < array(i)  then result = (result._1, i)
  }
  result
//private var advantage: Int,private var previousMoves: List[Int],
class Result(private var previousMove: Int,  private var mapState: Array[Array[Int]],  private var nextPlayer: Int) extends gameLogic {

  def GetNextPlayer(): Int = nextPlayer
  def GetMapState(): Array[Array[Int]] = mapState

}

class SynchronizedDataBase() {
  private var queues: Array[mutable.Queue[Result]] = Array.fill(6)(new mutable.Queue[Result]())
  private var access: Array[Boolean] = Array.fill(6)(true)
  private var queueElementsQuantity = Array.fill(6)(0)

  def getQueue( number: Int) = this.synchronized {
    while !access(number) do wait()
    queues(number)
  }

  def getQueueElementsQuantity( number: Int): Int =
    queueElementsQuantity(number)

  def setQueueElementsQuantity( number: Int, quantity: Int): Unit =
    queueElementsQuantity(number) = quantity

  def splitQueueBetweenTwo(toSplit: Int, taker: Int): Unit =
    access(toSplit) = false
//    while( !queues(toSplit).isEmpty)
//      helpQueue.enqueue(queues(toSplit).dequeue())

    var counter = 0
//    !helpQueue.isEmpty
    while (counter < queueElementsQuantity(toSplit)/2 ){
      if !queues(toSplit).isEmpty then
        queues(taker).enqueue(queues(toSplit).dequeue())
        counter += 1
  //    if counter%2 == 0
  //    then queues(toSplit).enqueue(helpQueue.dequeue())
  //    else queues(taker).enqueue(helpQueue.dequeue())

    }
    access(toSplit) = true
}

abstract class Player() extends gameLogic{
   def ReturnMove(): Int
   def Possibilities(startMapState: Array[Array[Int]],player: Int): Unit
   def searchingNeed(): Boolean
}

class Human() extends Player{
  private var result = -1
  private var currentMapState: Array[Array[Int]] = Array.fill(2)(Array.fill(7)(0))
  private var player: Int = 0
  private var wantPlay: Int = 0

  def Start(): Unit ={
    println("Welcome!!!" +
      "\n1. Player vs Player" +
      "\n2. Player vs Computer" +
      "\n3. Computer vs Computer" +
      "\n4. End")
     var line = "";
    while(line == ""){
      line = Console.in.readLine();
      line match
        case "1" => wantPlay = 1
        case "2" => wantPlay = 2
        case "3" => wantPlay = 3
        case "4" => wantPlay = 0
        case _ =>
    }

  }

  def GetWantPlay(): Int = wantPlay

  override def ReturnMove(): Int =
    if result == -1
    then
      CheckAvailableMoves(currentMapState,player).head
    else
      result
  @Override
  override def Possibilities(startMapState: Array[Array[Int]],playerNum: Int): Unit = {
    player = playerNum
    result = -1
    currentMapState = startMapState
    print("Podaj Liczbę: ")
    var line  = Console.in.readLine();
    if line != ""
    then
      while(!CheckAvailableMoves(currentMapState,player).contains(line.charAt(0).asDigit)){
        print("Podaj Liczbę: ")
        line = Console.in.readLine();
        println("")
      }
      result = line.charAt(0).asDigit
  }

  override def searchingNeed(): Boolean =
    if result == -1  then true else false

}

class Serwer(private var listOfPlayers: List[Human]) extends gameLogic {
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
          else Play(firstPlayer,listOfPlayers(i))
        else if listOfPlayers(i).GetWantPlay() == 2
        then Play(listOfPlayers(i),engine)
        else if listOfPlayers(i).GetWantPlay() == 3
        then Play(engine,engine2)

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