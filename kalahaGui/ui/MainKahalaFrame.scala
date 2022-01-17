package kalaha.ui

import akka.actor.{ActorRef, ActorSystem, Props}
import kalaha.actors.{GUIUpdateActor, WorkerActor}
import kalaha.models.GuiActorActions.SendBtnInfoBack
import kalaha.models.{GUIEventsHandler, GameBoard}
import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.resource.strings.{about, about_Info, about_title, draw_dialog_text, draw_dialog_title, won_dialog_text, won_dialog_title}
import kalaha.ui.MainKahalaFrame.{getSystemActor, systemActor}

import scala.swing.{Dialog, MainFrame}

class MainKahalaFrame extends MainFrame with GUIEventsHandler {

  private var firstBattleView: PlayerBattleView = new PlayerBattleView(First, this)
  private var secondBattleView: PlayerBattleView = new PlayerBattleView(Second, this)

  private var firstPlayerView = firstBattleView.getView()
  private var secondPlayerView = secondBattleView.getView()

  private val guiActor = systemActor.actorOf(Props(new GUIUpdateActor(this)), "workerGui")

  val mainActor = createMainWorker()

  def deleteOldView: Unit =
    peer.getContentPane.remove(0)

  def switchToView(player: Player): Unit =
    deleteOldView
    player match
      case First =>
        contents = firstPlayerView
      case Second =>
        contents = secondPlayerView


  def onPlayerClick(player: Player, buttonIndex: Int): Unit =
    guiActor ! SendBtnInfoBack(player, buttonIndex)

  def refreshButtons(gameBoard: GameBoard, player: Player): Unit =
    firstBattleView.updateButtonsView(gameBoard)
    secondBattleView.updateButtonsView(gameBoard)

  def createMainWorker(): ActorRef =
    systemActor.actorOf(Props(new WorkerActor(guiActor)), "worker")

  def showDialogWon(myScore: Int, opponentScore: Int, who: String): Unit =
    Dialog.showMessage(contents.head, s"$who \n $won_dialog_text $myScore - $opponentScore", title=won_dialog_title)
  
  def showDialogDraw(score: Int): Unit =
    Dialog.showMessage(contents.head, s"$draw_dialog_text $score", title=draw_dialog_title)
  
  override def showDialogAbout(): Unit =
    Dialog.showMessage(contents.head, about_Info, title = about_title)
  
}

object MainKahalaFrame{

  private val systemActor = ActorSystem("Kahala")

  def getSystemActor: ActorSystem =
    systemActor

}
