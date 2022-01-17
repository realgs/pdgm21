package Kalah

import java.util.Scanner

class Game(seeds: Int) {
  private var players:Array[Opponent] = addOpponents
  private val board = new Board(seeds)

  private def addOpponents:Array[Opponent]={
    players = new Array[Opponent](2)

    if consoleChooseGame == 1 then
      players.update(0, new Computer("Computer1", 0))
      players.update(1, new Computer("Computer2", 1))
    else
      players.update(0, new Player("Player", 0))
      players.update(1, new Computer("Computer", 1))

    println("Players created")
    players
  }

  def consoleChooseGame:Int={
    val sc=new Scanner(System.in)
    var chosenGame = 0
    while (chosenGame != 1 && chosenGame != 2) {
      println("Choose game:")
      println("1.Computer vs Computer")
      println("2.Player vs Computer:")
      try {
        chosenGame = Integer.parseInt(sc.next())
      } catch  {
        case e: Exception =>
          println("Try again")
      }
    }
    chosenGame
  }

  def play:Any= {
    var playerIndex = 0
    while (!checkEnd) {
      printBoard
      move(playerIndex)
      playerIndex = (playerIndex + 1) % 2
    }
    printBoard
    endGame()
  }

  def move(playerIndex: Int) : Any={
    Thread.sleep(100)
    var boardIndex = players(playerIndex).move(board)
    if board.getBoard(boardIndex) == 0 then
      println("The house is empty, choose another one.")
      move(playerIndex)
    else
      var seeds = board.getBoard(boardIndex)
      board.getBoard(boardIndex) = 0
      while (seeds != 0) {
        boardIndex= (boardIndex+1) % 14
        board.getBoard(boardIndex)+=1
        seeds-=1
      }
      if playerIndex == 0 && boardIndex == 7 then
        if checkEnd then return 0
        printBoard
        move(playerIndex)
      if playerIndex == 1 && boardIndex == 0 then
        if checkEnd then return 0
        printBoard
        move(playerIndex)
      if selfEmptyHouse(playerIndex, boardIndex) then
        board.getBoard(7 * ((playerIndex+1)%2))+=board.getBoard(boardIndex)
        board.getBoard(boardIndex)=0
        board.getBoard(7 * ((playerIndex+1)%2))+=board.getBoard(14-boardIndex)
        board.getBoard(14-boardIndex)=0
  }

  private def selfEmptyHouse(playerIndex:Int, boardIndex: Int):Boolean={
    board.getBoard(boardIndex) == 1 && boardIndex >= playerIndex * 7 + 1 && boardIndex <= playerIndex * 7 + 6
  }

  def checkEnd:Boolean= {
    for (x <- 1 to 6)
      if board.getBoard(x) != 0 then
        for (y <- 8 to 13)
          if board.getBoard(y) != 0 then return false
        return true
    true
  }

  def endGame()={
    if board.getBoard(0)>board.getBoard(7) then
      println(players(1).toString +" wins!")
    else if board.getBoard(0)<board.getBoard(7) then
      println(players(0).toString +" wins!")
    else
      println("Tie")
  }

  def printBoard={
    var row1="\t\t"
    var row2="\t\t"
    //gracz 2
    for (x <- 0 to 5)
      row1=row1+" ["+(13-x)+"] \t\t"
      row2=row2+" ("+board.getBoard(13-x)+") \t\t"
    row2+=" \t\t\t"
    println(row1)
    println(row2)
    //bazy
    println("\t<"+board.getBoard(0)+">\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t<"+board.getBoard(7)+">\t")
    //gracz 1
    row1="\t\t"
    row2="\t\t"
    for (x <- 1 to 6)
      row1=row1+" ["+x+"] \t\t"
      row2=row2+" ("+board.getBoard(x)+") \t\t"
    row2+=" \t\t\t"
    println(row2)
    println(row1)
  }

}
