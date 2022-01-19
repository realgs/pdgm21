package controllers

import model.GameSpecification

import scala.annotation.tailrec
import model.playerModel.*
import model.gameModel.*
import model.serverModel.Client
import views.MainView

import java.net.Socket
import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ControllerMainView:

  private def getInput(): Int =
    print(": ")
    try
      scala.io.StdIn.readInt()
    catch
      case e: NumberFormatException =>
        println("Nie ma takiego wyboru!")
        getInput()

  private def getNameOfPlayer(): String =
    println("Podaj imie: ")
    scala.io.StdIn.readLine()

  private def getIsHumanStartin(): Boolean =
    print("Czy chcesz zaczynac\n(Y/N):")
    val choice = scala.io.StdIn.readChar()
    if choice != 'Y' && choice != 'N' then
      println("Nie ma takiego wyboru!")
      getIsHumanStartin()
    choice == 'Y'


  @tailrec
  def mainMenuInput(): Unit =
    val choice = getInput()

    choice match
      case 1 =>
        val player1 = new Computer()
        val player2 = new Computer(isStartingGame = false)
        val game = Simulation(player1, player2)
        game.run()
      case 2 =>
        val isHumanStarting = getIsHumanStartin()
        val human = new HumanPlayer(newName = getNameOfPlayer(), isStartingGame = isHumanStarting)
        val computer = new Computer(isStartingGame = !isHumanStarting)
        val game = SingleGame(human, computer)
        game.run(isHumanStarting)
      case 3 =>
        println("Podaj swoje imie: ")
        val input = Future{scala.io.StdIn.readLine()}
        var name = ""
        try
          name = Await.result(input, Duration(30, "seconds"))
        catch
          case e: TimeoutException => MainView.mainMenuView()

        try
          Thread.sleep(500)
          val client = new Client(new Socket("localhost", GameSpecification.PORT), name)
          client.checkIfStarting()
          client.waitingRoom()
          scala.io.StdIn.readLine("\nPress enter to continue")
          MainView.mainMenuView()
        catch
          case e: java.io.IOException =>
            println("Serwer nie jest dostepny")
            Thread.sleep(1000)
            MainView.mainMenuView()



      case 4 => MainView.optionsView(s"Ilosc kamieni na start ustawiona na: ${GameSpecification.STARTAMOUNTOFROCKS}\nInteligencja komputera ustawiona na: ${GameSpecification.AILEVELNAME()}")
      case 5 => System.exit(0)
      case _ =>
        println("Nie ma takiego wyboru!")
        mainMenuInput()

  @tailrec
  def optionsViewController(): Unit =
    val choice = getInput()

    choice match
      case 1 => MainView.optionsViewAi()
      case 2 => MainView.optionsViewRocks()
      case 3 => MainView.mainMenuView()
      case _ =>
        println("Nie ma takiego wyboru!")
        optionsViewController()

  def optionsAIController(): Unit =
    val choice = getInput()

    choice match
      case 1 => GameSpecification.AILEVEL = 1
      case 2 => GameSpecification.AILEVEL = 4
      case 3 => GameSpecification.AILEVEL = 5
      case 4 => GameSpecification.AILEVEL = 6
      case 5 => GameSpecification.AILEVEL = 8
      case 6 => MainView.optionsView(s"Ilosc kamieni na start ustawiona na: ${GameSpecification.STARTAMOUNTOFROCKS}\nInteligencja komputera ustawiona na: ${GameSpecification.AILEVELNAME()}")
      case _ =>
        println("Nie ma takiego wyboru!")
        optionsAIController()
    MainView.optionsView(s"Inteligencja komputera ustawiona na: ${GameSpecification.AILEVELNAME()}")

  def optionsRocksController(): Unit =
    println("Powrot wpisz -1")
    val choice = getInput()

    if choice == -1 then MainView.optionsView(s"Ilosc kamieni na start ustawiona na: ${GameSpecification.STARTAMOUNTOFROCKS}\nInteligencja komputera ustawiona na: ${GameSpecification.AILEVELNAME()}")

    if choice > 0 then
      GameSpecification.resetAmountOfRocks(choice)
      MainView.optionsView(s"Ilosc kamieni na start ustawiona na: ${GameSpecification.STARTAMOUNTOFROCKS}")
    else
      println("Nie ma takiego wyboru!")
      optionsAIController()
