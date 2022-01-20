import scala.annotation.tailrec

class KalahaSim {
  private var board = Array(6, 6, 6, 6, 6, 6, 0, 6, 6, 6, 6, 6, 6, 0)   // 0-5 is P1 row, 6 is P1 base, 7-12 is P2 row, 13 is P2 base

  def move(poz:Int, p1:Boolean): Int =      // -1 means wrong argument, 0 means good argument, 1 means good argument and players gets bonus move
    if poz < 0 || poz == 6 || poz > 12 || board(poz) == 0 then throw Exception("Someone allowed illegal move!") else
      val helper = board(poz)
      board.update(poz, 0)
      @tailrec
      def step(poz:Int, helper:Int): Int =
        if helper > 0 then
          board.update(poz, board(poz)+1)
          step(if poz == 13 then 0 else poz+1, helper-1)
        else
          if (poz == 0 && !p1) || (poz == 7 && p1) then 1 else
            if board(if poz == 0 then 13 else poz-1) == 1 && p1 == (poz<7) then
              board.update(if poz == 0 then 13 else poz-1, board(13-poz)+board(if poz == 0 then 13 else poz-1))
              board.update(13-poz, 0)
            0
      step(poz+1, helper)

  def noMoveLeft(player1:Boolean): Boolean =      //checks if player can move
    val end = if player1 then 5 else 12
    @tailrec
    def step(poz:Int, sum:Int): Int =
      if poz == end then board(poz)+sum else step(poz+1, sum+board(poz))
    0 == step(if player1 then 0 else 7, 0)
    
  def points(player1:Boolean): Int = if player1 then board(6) else board(13)    //check how many points a player has
  def pointDiff(): Int = board(6)-board(13)     //check the difference between points of P1 and P2
  
  def at(poz:Int): Int =
    if poz<0 || poz>13 then throw IllegalArgumentException("Kalaha board is numbered from 0 to 13") else board(poz)
    
  def cheat(p1:Boolean): Unit =             // for testing only
    board = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 )  
    board.update(if p1 then 6 else 13, 99)
  
  
  def cloner():KalahaSim =
    val c = KalahaSim()
    var i = 0
    while(i<=13)
      c.board.update(i, board(i))
      i = i+1
    c  
}
