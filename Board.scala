package Game

class Board {
  private var board : Array[Hole] = new Array[Hole](14)
  private val PLAYER1 = 1
  private val PLAYER2 = 2
  private val BASE1 = 6
  private val BASE2 = 13


  board(BASE1) = new Hole(0, true, PLAYER1)
  board(BASE2) = new Hole(0, true, PLAYER2)

  for(i <- 0 to 5)
  {
    board(i) = new Hole(6,false,PLAYER1)
    board(i).setOpposite(12 - i)
  }

  for(i <- 7 to 12)
  {
    board(i) = new Hole(6,false,PLAYER2)
    board(i).setOpposite(12 - i)
  }

  def movePebbles(number : Int, whichPlayer : Int) : Int =
    val secondPlayer : Int = if(whichPlayer == PLAYER1) then PLAYER2
    else PLAYER1

    val numberHole : Int = if(whichPlayer == PLAYER1) then number -1
    else number + 6

    var pebbles : Int = board(numberHole).getPebbles
    var i = 1
    if(pebbles == 0) then return -1
    else
    {
      board(numberHole).setPebbles(0)
      while(pebbles > 0)
      {
        if(board((numberHole+i)% board.length).getBase == true && board((numberHole+i)% board.length).getPlayer != whichPlayer)
        then
        {
          i += 1
        }
        else {
          board((numberHole + i) % board.length).incrementPebbles
          pebbles -= 1
          i +=1
        }
      }
    }

    val lastIndex : Int = (numberHole + i - 1) % board.length
    var base : Int = 0

    if (lastIndex != BASE1 && lastIndex != BASE2 && board(lastIndex).getPebbles == 1 && board(lastIndex).getPlayer == whichPlayer && board(board(lastIndex).getOpposite).getPebbles != 0) then
    {
      if(whichPlayer == PLAYER1) then base = BASE1
      else base = BASE2
      board(base).setPebbles(board(base).getPebbles + board(lastIndex).getPebbles + board(board(lastIndex).getOpposite).getPebbles)
      board(lastIndex).setPebbles(0)
      board(board(lastIndex).getOpposite).setPebbles(0)
      return secondPlayer
    }
    else if(board(lastIndex).getBase == true && board(lastIndex).getPlayer == whichPlayer)
    {
      return whichPlayer
    }
    else return secondPlayer

  def isGameFinished : Boolean =
    var emptyHole : Int = 0
    for(i <- 0 to 5)
    {
      if(board(i).getPebbles == 0) emptyHole += 1
    }
    if(emptyHole == BASE1) return true

    emptyHole = 0
    for(i <- 7 to 12)
    {
      if(board(i).getPebbles == 0) emptyHole += 1
    }
    if(emptyHole == 6) return true

    return false


  def results : Int =
    for (i <- 0 to 5)
    {
      board(BASE1).setPebbles(board(BASE1).getPebbles + board(i).getPebbles)
      board(i).setPebbles(0)
    }

    for (i <- 7 to 12)
    {
      board(BASE2).setPebbles(board(BASE2).getPebbles + board(i).getPebbles)
      board(i).setPebbles(0)
    }
    val player1 = board(BASE1).getPebbles
    val player2 = board(BASE2).getPebbles
    println("Wyniki: ")
    println(s"${player1} : ${player2}")
    if(player1 > player2) then 1
    else if(player1 < player2) then 2
    else 0

  def showBoard1 : String =
    s"\n    ${board(12).getPebbles} | ${board(11).getPebbles} | ${board(10).getPebbles} | ${board(9).getPebbles} | ${board(8).getPebbles} | ${board(7).getPebbles}\n" +
      s" (${board(13).getPebbles})                     (${board(6).getPebbles})\n" +
      s"    ${board(0).getPebbles} | ${board(1).getPebbles} | ${board(2).getPebbles} | ${board(3).getPebbles} | ${board(4).getPebbles} | ${board(5).getPebbles}\n"

  def showBoard2 : String =
    s"\n    ${board(5).getPebbles} | ${board(4).getPebbles} | ${board(3).getPebbles} | ${board(2).getPebbles} | ${board(1).getPebbles} | ${board(0).getPebbles}\n" +
      s" (${board(6).getPebbles})                     (${board(13).getPebbles})\n" +
      s"    ${board(7).getPebbles} | ${board(8).getPebbles} | ${board(9).getPebbles} | ${board(10).getPebbles} | ${board(11).getPebbles} | ${board(12).getPebbles}\n"

  def emptyHole(index : Int) : Boolean =
    if(board(index).isEmpty) then true
    else false

  def getBoard = board
  def setBoard(board: Array[Hole]): Unit =
    this.board = board

  def copy : Board =
    val copyBoard: Board = new Board
    for (i <- 0 to board.length-1) {
      copyBoard.getBoard(i) = new Hole(board(i).getPebbles, board(i).getBase, board(i).getPlayer, board(i).getOpposite)
    }
    return copyBoard

}
