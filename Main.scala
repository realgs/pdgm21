package kalahgame

import scala.annotation.tailrec

object Main:

  def chooseComputer(): Player =
    println("\n\t\t\t  Available difficulty levels: ")
    println("\t\t\t\t\t\t  1")
    println("\t\t\t\t\t\t  2")
    println("\t\t\t\t\t\t  3")
    println("\t\t\t\t\t\tRandom")
    print("\t\t\t  Choose difficulty level: ")
    val cmd = scala.io.StdIn.readLine()
    cmd match
      case "1" =>
        return new Computer(2)
      case "2" =>
        return new Computer(4)
      case "3" =>
        return new Computer(11)
      case "Random" =>
        return new RandomComputer()
      case _ =>
        println("\nInvalid choice! Choose Your difficulty, insert: 1, 2, 3 or Random!\n")
        chooseComputer()

  def printMenu(): (Player, Player) =
    println("\n\n^_^_^_^_^_^_^_^_^_Welcome in Kalah game!_^_^_^_^_^_^_^_^_^\n")
    println("\t\t\t\tAvailable game modes: ")
    println("\t\t\t\t-Player versus player: 1")
    println("\t\t\t\t-Player versus computer: 2")
    println("\t\t\t\t-Computer versus computer: 3")
    print("\n\t\t\t\tChoose Your game mode: ")
    val cmd = scala.io.StdIn.readLine()
    cmd match
      case "1" =>
        return (new Human(), new Human())
      case "2" =>
        return (new Human(), chooseComputer())
      case "3" =>
        println("Computer1: ")
        val pc1 = chooseComputer()
        println("Computer2: ")
        val pc2 = chooseComputer()
        return (pc1, pc2)
      case _ =>
        println("\nInvalid choice! Choose Your game mode, insert: 1, 2 or 3!\n")
        printMenu()

  def main(args: Array[String]): Unit =
    var flag = true
    while(flag)
      val players = printMenu()
      val serwer = new Server()
      serwer.runKalah(players._1, players._2)
      println("\tPlay again: hit enter and press 0 || Exit: press any other key")
      if(scala.io.StdIn.readLine() != "0")
        flag = false
    println("\t\t\t\t  Have a nice day!")


end Main