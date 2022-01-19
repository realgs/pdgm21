package model.gameModel

import model.GameSpecification
import model.playerModel.Player
import views.MainView

abstract class Game(private val player1: Player, private val player2: Player):

  def drawBoard(isFirstPlayerMove :Boolean = true) =
    MainView.drawSpace()
    if !isFirstPlayerMove then print("*")
    print(s"${player2.name}")

    println()
    print("\t\t")

    for index <- 1 to GameSpecification.INDEXFIRSTSTORE do
      print(s"${player2.arrayOfHomes(GameSpecification.INDEXFIRSTSTORE - index)} \t")

    println()
    print(s"\t${player2.score}")

    for index <- 0 to GameSpecification.INDEXFIRSTSTORE do print("\t")

    print(player1.score)
    println()
    print("\t\t")

    for index <- 0 until GameSpecification.INDEXFIRSTSTORE do
      print(s"${player1.arrayOfHomes(index)} \t")

    println()
    if isFirstPlayerMove then print("*")
    print(s"${player1.name}")

    println("\n")

  def run(isFirstPlayerMove: Boolean = true): Unit =
    if player1.name == player2.name then
      player1.name = player1.name + "1"
      player2.name = player2.name + "2"

    next(isFirstPlayerMove)

  def next(isFirstPlayerMove :Boolean): Unit

  def end(): Unit =
    drawBoard()
    player1.score = player1.countEndingScore()
    player2.score = player2.countEndingScore()
    val winner =
      if player1.score > player2.score then player1.name + " " + player1.score + " " + player2.score
      else if player1.score == player2.score then "Draw"
      else player2.name + " " + player2.score + " " + player1.score
    if winner != "Draw" then println(s"Winner is $winner") else println("Draw")
    scala.io.StdIn.readLine("Press enter to continue")
    MainView.mainMenuView(false)
