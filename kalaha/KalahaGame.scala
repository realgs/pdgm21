import game_system.GameServer

@main
def main(args: String*): Unit =

  val game = new GameServer()
  game.start()
