import scala.io.StdIn


class Bowl(var value: Int, val numberOfBowl: Int)
{
  def increase():Unit =
    value =  value + 1

  def decrease():Unit =
    value =  value - 1

  override def toString(): String =
    value.toString
}

class Server
{
  
  private var p1Bowl1 = Bowl(6, 1)
  private var p1Bowl2 = Bowl(6, 2)
  private var p1Bowl3 = Bowl(6, 3)
  private var p1Bowl4 = Bowl(6, 4)
  private var p1Bowl5 = Bowl(6, 5)
  private var p1Bowl6 = Bowl(6, 6)
  private var p1Score = Bowl(0, 7)
  private var p1Bowls = List(p1Bowl1, p1Bowl2, p1Bowl3, p1Bowl4, p1Bowl5, p1Bowl6, p1Score)

  private var p2Bowl1 = Bowl(6, 1)
  private var p2Bowl2 = Bowl(6, 2)
  private var p2Bowl3 = Bowl(6, 3)
  private var p2Bowl4 = Bowl(6, 4)
  private var p2Bowl5 = Bowl(6, 5)
  private var p2Bowl6 = Bowl(6, 6)
  private var p2Score = Bowl(0, 7)
  private var p2Bowls = List(p2Bowl1, p2Bowl2, p2Bowl3, p2Bowl4, p2Bowl5, p2Bowl6, p2Score)

  def printBowls(): Unit =
    println("Player one")
    println(p1Bowl6.value + " " + p1Bowl5.value + " " + p1Bowl4.value+ " " + p1Bowl3.value+ " " + p1Bowl2.value+ " " + p1Bowl1.value+ "    Score: " + p1Score.value)
    println()
    println("Player two")
    println(p2Bowl1.value + " " + p2Bowl2.value + " " + p2Bowl3.value+ " " + p2Bowl4.value+ " " + p2Bowl5.value+ " " + p2Bowl6.value+ "    Score: " + p2Score.value)
    println()

  def gameEnded() : Boolean =
    var result = false
    if p1Bowl1.value == 0 && p1Bowl2.value == 0  && p1Bowl3.value == 0  && p1Bowl4.value == 0  && p1Bowl5.value == 0  && p1Bowl6.value == 0 then
      takeLeftStones(1)
      result = true
    if p2Bowl1.value == 0 && p2Bowl2.value == 0  && p2Bowl3.value == 0  && p2Bowl4.value == 0  && p2Bowl5.value == 0  && p2Bowl6.value == 0 then
      takeLeftStones(0)
      result = true
    result

  def takeLeftStones(numberOfPlayer: Int): Unit =
    numberOfPlayer match
      case 0 =>
        while p1Bowls.head.numberOfBowl != 7 do
          p1Score.value = p1Score.value + p1Bowls.head.value
          p1Bowls = p1Bowls.tail
      case 1 =>
        while p2Bowls.head.numberOfBowl != 7 do
          p2Score.value = p2Score.value + p2Bowls.head.value
          p2Bowls = p2Bowls.tail
      case _ => throw new IllegalArgumentException("in makeMove -> Move -> gameEnded -> takeLeftStones")

  def makeMove(player: Int, numberOfBowl: Int): Boolean =
    player match
        case 0 => Move(p1Bowls, p2Bowls, p1Score, numberOfBowl)
        case 1 => Move(p2Bowls, p1Bowls, p2Score, numberOfBowl)
        case _ => false


  private def Move(playerBowls: List[Bowl], oponentBowls: List[Bowl], playerScore: Bowl, numberOfBowls: Integer): Boolean =
    var FreeMove = false
    var list = playerBowls
    var oponentList = oponentBowls
    var i = list.head.value
    var j = numberOfBowls

    while  j >= 0 do
      i = list.head.value
      if j == 0 then
        list.head.value = 0
      list = list.tail
      j = j - 1;


    while i > 0 do
      if !list.isEmpty then
        list.head.increase()
        if list.head.numberOfBowl == 7 && i == 1 then // Ostatni kamyk wpada do naszej bazy
          FreeMove = true
          return FreeMove

        if i == 1 && list.head.value == 1 then // Zbiajnie kamyków u przeciwnika
          list.head.decrease()
          playerScore.increase()
          var BowlNumber = 6 - list.head.numberOfBowl
          oponentList = oponentBowls

          while  BowlNumber >= 0 do
            if BowlNumber == 0 then
              playerScore.value = playerScore.value + oponentList.head.value
              oponentList.head.value = 0

            BowlNumber = BowlNumber - 1
            oponentList = oponentList.tail

        list = list.tail

      else
        if oponentList.head.numberOfBowl != 7 then
          oponentList.head.increase()
          oponentList = oponentList.tail
        else
          list = playerBowls
          oponentList = oponentBowls
          i = i + 1
      i = i - 1
    FreeMove


  def isBowlEmpty(bowlNumber: Int, player: Boolean): Boolean =
    var listOfBowls: List[Bowl] = List()
    if player then
      listOfBowls = p1Bowls
      while !listOfBowls.isEmpty do
        if listOfBowls.head.numberOfBowl == 7 - bowlNumber  then
          if listOfBowls.head.value == 0 then
            return true
        listOfBowls = listOfBowls.tail
    else
      listOfBowls = p2Bowls
      while !listOfBowls.isEmpty do
        if listOfBowls.head.numberOfBowl == bowlNumber  then
          if listOfBowls.head.value == 0 then
            return true
        listOfBowls = listOfBowls.tail
    false

  def playWithBot(): Unit =
    val bot = BotPlayer()
    var freeMovePl1 = false
    var freeMovePl2 = false
    var playerBowl = 0
    var botBowl = 0
    printBowls()

    while !gameEnded() do

      println("Wybierz numer od 1 do 6")
      playerBowl = StdIn.readInt()
      while isBowlEmpty(playerBowl, true) do
        println("Wybierz numer od 1 do 6")
        playerBowl = StdIn.readInt()
      freeMovePl1 = makeMove(0, 6 - playerBowl)
      printBowls()

      while freeMovePl1 do
        println("Wybierz numer od 1 do 6")
        playerBowl = StdIn.readInt()
        while isBowlEmpty(playerBowl, true) do
          println("Wybierz numer od 1 do 6")
          playerBowl = StdIn.readInt()
        freeMovePl1 = makeMove(0, 6 - playerBowl)
        printBowls()

      botBowl = bot.play()
      while isBowlEmpty(botBowl, false) do
        botBowl = bot.play()
      freeMovePl2 = makeMove(1, botBowl - 1)
      printBowls()

      botBowl = bot.play()
      while freeMovePl2  do
        while isBowlEmpty(botBowl, false) do
          botBowl = bot.play()
        freeMovePl2 = makeMove(1, botBowl - 1)

    println("Score: " + p1Score.value + " : " + p2Score.value)
    if p1Score.value > p2Score.value then
    println("Player 1 won!!!")
    else
     if p1Score.value == p2Score.value then
       println("Draw!!!")
     else
       println("Player 2 won!!!")

  def botPlay(bot1: BotPlayer, bot2: BotPlayer): Unit =
    var freeMovePl1 = false
    var freeMovePl2 = false
    var bowl = 0

    printBowls()
    while !gameEnded() do

      bowl = bot1.play()
      while isBowlEmpty(bowl, true) do
        bowl = bot1.play()
      freeMovePl1 = makeMove(0, bowl - 1)
      printBowls()
      while freeMovePl1 do
        bowl = bot1.play()
        while isBowlEmpty(bowl, true) do
          bowl = bot1.play()
        freeMovePl1 = makeMove(0, bowl - 1)
        printBowls()

      bowl = bot2.play()
      while isBowlEmpty(bowl, false) do
        bowl = bot2.play()
      freeMovePl2 = makeMove(1, bowl - 1)
      printBowls()

      while freeMovePl2  do
        bowl = bot2.play()
        while isBowlEmpty(bowl, false) do
          bowl = bot2.play()
        freeMovePl2 = makeMove(1, bowl - 1)
        printBowls()

    println("Score: " + p1Score.value + " : " + p2Score.value)
    if p1Score.value > p2Score.value then
      println("Player 1 won!!!")
    else
      if p1Score.value == p2Score.value then
        println("Draw!!!")
      else
        println("Player 2 won!!!")






}
