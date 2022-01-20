import scala.annotation.tailrec
import scala.io.StdIn

object Main:
  def main(args: Array[String]): Unit =
    userInterfaceLoop()


  @tailrec
  def userInterfaceLoop(): Unit =
    print("Choose mode (1 - comp vs comp, 2 - comp vs player, 3 - player vs player): ")
    val mode = StdIn.readInt()
    if mode < 1 || mode > 3 then
      println("there is no mode like that")
      userInterfaceLoop()
    else
      val players =
        if mode == 1 then
          (new ComputerPlayer(), new ComputerPlayer())
        else if mode == 2 then
          (new HumanConsolePlayer(), new ComputerPlayer())
        else
          (new HumanConsolePlayer(), new HumanConsolePlayer())

      val server = new Server(players._1, players._2)
      server.play(6, 4)
      print("Do you want to play again? (y/n) : ")
      if StdIn.readBoolean() then
        userInterfaceLoop()
      else
        ()