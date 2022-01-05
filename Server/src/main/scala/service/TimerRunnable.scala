package service

import cask.*
import controller.KalahaController

class TimerRunnable(channel: WsChannelActor, controller: KalahaController) extends Runnable:
    override def run(): Unit =
        while controller.service.gameStarted do
            channel.send(Ws.Text("Time remaining: " + (System.currentTimeMillis - controller.service.makeMoveDeadline)))
            Thread.sleep(5000)