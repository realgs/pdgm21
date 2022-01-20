import scala.io.StdIn

object Main
{
  def menu(): Unit =
    println("MENU")
    println("1 - Symulacja")
    println("2 - Graj")
    println("0 - Wyjdz")

  def main(args: Array[String]): Unit =
    val server = Server()
    menu()
    var num = StdIn.readInt()
    while num != 0 do
      num match
        case 2 => server.playWithBot()
        case 1 => server.botPlay(BotPlayer(), BotPlayer() )
        case _ =>
      menu()
      num = StdIn.readInt()
}
