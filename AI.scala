package Player
import Board.Board

import scala.util.Random

class AI(PlayerNum: Int, Name: String) extends Player(PlayerNum , Name) {


  def BestScore(board: Board, PlayerNum: Int): (Int, Int, Int, Int) = {
    var diffrence: Int = 0
    var sum: Int = 0
    var Bestmove: Int = -1
    for (i <- 0 to board.getBoardSize() - 1) {
      var CBoard = board.CloneBoard()
      CBoard.Move(i, PlayerNum)
      var actsum = if PlayerNum == 0 then CBoard.getBoard()(CBoard.getBasePlayer1()) else CBoard.getBoard()(CBoard.getBasePlayer2())
      var actdiffrence = if PlayerNum == 0 then actsum - CBoard.getBoard()(CBoard.getBasePlayer2()) else actsum - CBoard.getBoard()(CBoard.getBasePlayer1())
      if sum <= actsum then
        diffrence = actdiffrence
        sum = actsum
        Bestmove = i
    }
    return (diffrence, sum, PlayerNum, Bestmove)
  }
  
  
  def BestScore2(board: Board): Int = {
    val (possible, hole) = nextmove(board)
    if possible then return hole
    else return bestMove(board)
  }
  
  def nextmove(board: Board): (Boolean,Int) = {
    for(i <- board.getBasePlayer1()-1 to 0 by -1){
      if board.getBoard()(i) + i == board.getBasePlayer1() then return(true , i - 0)
    }
    return (false,-1)
  }
  
    

  def simPlay(board: Board, PlayerNum: Int,idx: Int): Int = {
    var diffrence: Int = 0
    var sum: Int = 0
    var Bestmove: Int = -1
    var CBoard = board.CloneBoard()
    CBoard.Move(idx, PlayerNum)
    var actsum = if PlayerNum == 0 then CBoard.getBoard()(CBoard.getBasePlayer1()) else CBoard.getBoard()(CBoard.getBasePlayer2())
    var actdiffrence = if PlayerNum == 0 then actsum - CBoard.getBoard()(CBoard.getBasePlayer2()) else actsum - CBoard.getBoard()(CBoard.getBasePlayer1())
    diffrence = actdiffrence
    return diffrence
  }

  def bestMove(board: Board): Int = {
    val player = Player(board)
    val ran = new Random
    val diff: Array[Int] = Array.ofDim(player._2 - player._1)
    for(i<- player._1 to player._2 -1) {
      if board.getBoard()(i) == 0 then diff(i - player._1) = Int.MaxValue
      else{
        var CBoard: Board = board.CloneBoard()
        CBoard.Move(i-player._1,Player)
        diff(i - player._1) = enemyScore(CBoard)
      }
    }
    var min = diff.min
    var chosen = List[Int]()
    for(i <- 0 to diff.length-1){
      if diff(i) == min then chosen = i :: chosen
    }
    return chosen(ran.nextInt(chosen.length))
  }

  def enemyScore(board: Board): Int = {
    val player = Player(board)
    val diff: Array[Int] = Array.ofDim(player._4 - player._3)
    for(i <- player._3 to player._4 - 1){
      if board.getBoard()(i) == 0 then diff(i-player._3) = Int.MinValue
      else {
        var CBoard = board.CloneBoard()
        val same = if PlayerNum == 0 then CBoard.Move(i- player._3, 1) else   CBoard.Move(i- player._3, 0)
        diff(i - player._3) = CBoard.getBoard()(player._4) - CBoard.getBoard()(player._2)
        if same == (Player+1)%2 then diff(i-player._3) = diff(i-player._3) +1
      }
    }
    return diff.max
  }

  def Player(board: Board):(Int, Int, Int, Int) = {
    val player1Base: Int = board.getBasePlayer1()
    val player2Base: Int = board.getBasePlayer2()
    if PlayerNum == 0 then return(0,player1Base,player1Base+1,player2Base)
    else return (player1Base+1,player2Base,0,player2Base)
  }


}
