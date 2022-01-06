package service

import cask.*
import controller.KalahaController

class TimerRunnable(channel: WsChannelActor, controller: KalahaController) extends Runnable:
    override def run(): Unit =
        while controller.service.gameStarted do
            val timeRemaining = (controller.service.makeMoveDeadline - System.currentTimeMillis()) / 1000
            if timeRemaining <= 0 then controller.onTimeoutDefeat
            channel.send(Ws.Text(controller.service.turn + "'s time remaining: " + timeRemaining + " seconds"))
            Thread.sleep(5000)