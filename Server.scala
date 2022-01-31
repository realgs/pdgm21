import java.io.PrintStream
import java.net.{ServerSocket, SocketException}
import scala.concurrent.duration.Duration
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.io.BufferedSource
import scala.io.StdIn.*

class Server:
  var board = new Board
  var playerA:Client = _
  var playerB:Client = _
  var players:Array[Client] = Array(playerA, playerB)
  var currentPlayer:Int = 0
  val maxWaitTime = Duration(150, TimeUnit.SECONDS)
  var future:Future[Unit] = _
  val socket1 = new ServerSocket(9991)
  val socket2 = new ServerSocket(9992)
  val socket3 = new ServerSocket(9993)
  val socket4 = new ServerSocket(9994)
  val s1 = socket1.accept()
  val s2 = socket2.accept()
  val s3 = socket3.accept()
  val s4 = socket4.accept()


  lazy val in1 = new BufferedSource(s1.getInputStream()).getLines()
  val out1 = new PrintStream(s1.getOutputStream())
  lazy val in2 = new BufferedSource(s2.getInputStream()).getLines()
  val out2 = new PrintStream(s2.getOutputStream())
  lazy val in:Array[Iterator[String]] = Array(in1, in2)
  val out:Array[PrintStream] = Array(out1, out2)
  val out3 = new PrintStream(s3.getOutputStream())
  val out4 = new PrintStream(s4.getOutputStream())


  def gameInit(): Unit =
    playerA = new Player("Player A", 0, in1, out1)
    playerB = new Player("Player B", 1, in2, out2)
    board.boardInit(playerA.name, playerB.name)


  def endGame(noSocket: Boolean = false): Unit =
    board.printBoard()
    if board.getPlayerABase()>board.getPlayerBBase() then
      if !noSocket then
        out1.println("Wygrałeś!")
        out2.println("Przegrałeś!")
      println("Wygrał gracz " + playerA.name)
    else if board.getPlayerABase()<board.getPlayerBBase() then
      if !noSocket then
        out1.println("Przegrałeś!")
        out2.println("Wygrałeś!")
      println("Wygrał gracz " + playerB.name)
    else
      if !noSocket then
        out1.println("Remis!")
        out2.println("Remis!")
      println("Remis")
    socket1.close()
    socket2.close()
    socket3.close()
    socket4.close()
    System.exit(0)




  def start(): Unit =
    gameInit()


    board.printBoard()
    var turn = 0
    while(true)
      try
        if board.checkEnd(currentPlayer) then
          endGame()
        future = Future{
          if currentPlayer ==0 then
            println("Ruch gracza " + playerA.name)
          else
            println("Ruch gracza " + playerB.name)
          out(currentPlayer).println(board.toString(currentPlayer))
          if currentPlayer == 0 then
            out3.println(board.objectToString())
          else
            out4.println(board.objectToString())
          out(currentPlayer).println("Twój ruch\nPodaj otwór: ")
          while !in(currentPlayer).hasNext do
            Thread.sleep(1000)
          val input = in(currentPlayer).next().toInt-1+currentPlayer*7
          val result = board.moveStones(input,currentPlayer)
          if result == 0 then
            out(currentPlayer).println("Niepoprawny ruch")
          else if result == 1 then
            out(currentPlayer).println("Oczekiwanie na przeciwnika")
            currentPlayer =(currentPlayer+1)%2
            out(currentPlayer).println("\n\n\n")
          else
            out(currentPlayer).println("Dodatkowa tura")
          board.printBoard(currentPlayer)
          }
        Await.result(future, maxWaitTime)
        future.failed
      catch
        case e: TimeoutException =>
          out(currentPlayer).println("Przekroczono czas ruchu")
          out(currentPlayer).println("Oczekiwanie na przeciwnika")
          currentPlayer =(currentPlayer+1)%2
        case e: NumberFormatException =>
          out(currentPlayer).println("Zły format ruchu")
        case e: SocketException =>
          println("Klient został zamknięty")
          endGame(true)



