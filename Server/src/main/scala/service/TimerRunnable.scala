package service

import cask.*
import controller.KalahaController

class TimerRunnable(channel: WsChannelActor, controller: KalahaController) extends Runnable:
    override def run(): Unit =
        try
            var start = System.currentTimeMillis()
            while controller.service.gameStarted do
                if controller.gameStatus.status == "waiting for move" then
                    if System.currentTimeMillis() - start >= 1000 then
                        val timeRemaining = (controller.service.makeMoveDeadline - System.currentTimeMillis()) / 1000
                        if timeRemaining <= 0 then controller.onTimeoutDefeat()
                        start = System.currentTimeMillis()
                        channel.send(Ws.Text(s"remaining $timeRemaining"))
                Thread.sleep(10)
        catch
            case e: InterruptedException => println("Interrupt!")