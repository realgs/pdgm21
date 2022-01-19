package views

import controllers.ControllerMainView

object MainView:
  private val menu: String =
    "1. Symulacja\n2. Single\n3. Multi\n4. Opcje\n5. Zakoncz"

  private val options: String =
    "Opcje:\n1. Poziom inteligencji komputera\n2. Ilosc kamieni na start\n3. Powrot"

  private val optionsAi: String =
    "Poziom inteligencji komputera:\n1. Bardzo latwy(1)\n2. Latwy(4)\n3. Sredni(5)\n4. Trudny(6)\n5. Bardzo trudny(8)\n6. Powrot"

  private val optionsRocks: String =
    "Ilosc kamieni na start:\n"

  def drawSpace() =
    for index <- 0 to 20 do
      println()

  def mainMenuView(isFirst: Boolean = false): Unit =
    if !isFirst then drawSpace()
    println(menu)
    ControllerMainView.mainMenuInput()


  def optionsView(mess: String = ""): Unit =
    drawSpace()
    println(mess)
    println(options)
    ControllerMainView.optionsViewController()

  def optionsViewAi(): Unit =
    drawSpace()
    println(optionsAi)
    ControllerMainView.optionsAIController()

  def optionsViewRocks(): Unit =
    drawSpace()
    println(optionsRocks)
    ControllerMainView.optionsRocksController()

  def run(): Unit =
    println("KALAHA")
    mainMenuView(true)
