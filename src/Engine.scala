import java.util.concurrent.{Executor, ForkJoinPool}
import scala.collection.mutable
import scala.concurrent.ExecutionContext

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

}