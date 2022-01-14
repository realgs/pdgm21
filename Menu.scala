import scala.io.StdIn.*

class Menu() {

  def run(): Unit =
    val game = new Kalaha()
    println("KALAHA")
    println("1.Singleplayer")
    println("2.Multiplayer")
    println("3.Simulation")
    println("4.Exit")
    print("Choice: ")

    val choice = readInt()

    if choice == 1 then
      val p1 = new Player(1)
      val p2 = new AIPlayer(2, game)
      val server = new Server(p1, p2, game)
      server.play()
    if choice == 2 then
      val p1 = new Player(1)
      val p2 = new Player(2)
      val server = new Server(p1, p2, game)
      server.play()
    if choice == 3 then
      val p1 = new AIPlayer(1, game)
      val p2 = new AIPlayer(2, game)
      val server = new Server(p1, p2, game)
      server.play()
    if choice != 4 then run()
}
