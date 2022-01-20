import java.awt.Robot
import java.awt.event.KeyEvent

class HumanPlayer (override val Nr1: Boolean) extends Player(Nr1) {
  override def moveOwn(): Int =
    Interface.playerRequest(Nr1)
    try
      val poz = scala.io.StdIn.readInt()
      if poz == 2001 then
        game.cheat(Nr1)
        2001
      else if poz == 777 then
        test()
        moveOwn()
      else if !((poz>=0 && poz<=5 && Nr1) || (poz>=7 && poz<=12 && !Nr1)) || game.at(poz) == 0 then moveOwn() else
        poz
    catch
      case e: java.lang.NumberFormatException => moveOwn()

  override def skipFix(): Unit =
    val dirtyHotFix = Robot()
    Interface.missTurn()
    if !Nr1 then dirtyHotFix.keyPress(KeyEvent.VK_1) else print("1")
    dirtyHotFix.keyPress( if Nr1 then KeyEvent.VK_1 else KeyEvent.VK_2)
    dirtyHotFix.keyPress(KeyEvent.VK_ENTER)
    Thread.sleep(300)
}
