import GameState.copy

import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration
import scala.concurrent.*
import scala.io.StdIn.readInt
import ExecutionContext.Implicits.global

abstract class Player (protected val side: Int){
  def getMove(gameState: GameState): Int
}

class User(private val s: Int) extends Player(s){
  override def getMove(gameState: GameState): Int = {
    var move2 = 0
    try {
      val move = Future {input(gameState)}
      move2 = Await.result(move, Duration(30, TimeUnit.SECONDS))
      move2
    } catch {
      TimeoutException => {
        var randomMove = -1
        for(i <- s*gameState.pos.length/2 to (s + 1) * gameState.pos.length/2 - 2)
          if gameState.isMoveLegit(i) then randomMove = i

        println(s"You failed to make a move. We made a move for you: $randomMove")
        randomMove
      }
    }
  }

  private def input(gameState: GameState) = {
    var tmp = true
    var read = -1
    while (tmp) {
      println("enter your move:")
      read = readInt()
      if gameState.isMoveLegit(read) then
        tmp = false
    }
    read
  }
}

class Engine(private val s: Int, private val depth: Int) extends Player(s){
  override def getMove(gameState: GameState): Int = {
    Thread.sleep(5000)
    val k = calculate(depth, gameState)._1
    println(s"move made: $k")
    k
  }

  private def calculate(k: Int, gameState: GameState): (Int, Int) = {
    if gameState.isGameOver then
      if gameState.winner == s then
        (-1, Int.MaxValue)
      else
        (-1, Int.MinValue + 2)
    else {
      var gs = copy(gameState)
      if gameState.sideToMove != s then
        gs.updatePos(bestMove(gs, 1 - s)._1)
        calculate(k, gs)
      else
        if k == 0 then
          bestMove(gameState, s)
        else
          val moves = for(i <- s * (gameState.houses + 1) to (1 + s) * gameState.pos.length/2 - 1)
           yield {
            gs = copy(gameState)
            if gs.isMoveLegit(i) then
              gs.updatePos(i)
              (i, calculate(k - 1, gs)._2)
            else
              (i, Int.MinValue)
           }
          var best = moves.head

          for(x <- moves)
          if x._2 >= best._2 then
            best = x
          best
    }
  }

  private def bestMove(gameState: GameState, si: Int): (Int, Int) = {
    var best = (-1, Int.MinValue + 1)
    val moves = for(i <- si * (gameState.houses + 1) to (1 + si) * gameState.pos.length/2 - 1)
      yield (i, evalMove(copy(gameState), i, si))

    for(x <- moves)
      if best._2 <= x._2 then
        best = x

    best
  }

  private def evalMove(gameState: GameState, move: Int, si: Int): Int = {
    if gameState.isMoveLegit(move) then
      if gameState.updatePos(move) then
        if gameState.winner == si then
          Int.MaxValue
        else
          Int.MinValue + 2
      else
        evalPos(gameState, si)
    else
      Int.MinValue
  }

  private def evalPos(gameState: GameState, si: Int): Int = {
    evalPosHelp(gameState, si) - evalPosHelp(gameState, 1 - si)
  }

  private def evalPosHelp(gameState: GameState, side: Int) = {
    val endZone = gameState.pos.length/2 - 1
    val c = side * gameState.pos.length/2
    var count = gameState.pos(endZone + c)
    for(i <- 0 to endZone - 1)
      if gameState.pos(i + c) > endZone - i then
        count = count + 2*(endZone - i) - gameState.pos(i + c)
      else
        count = count + gameState.pos(i + c)

    count
  }
}