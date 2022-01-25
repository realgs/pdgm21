package kalaha.service

import kalaha.controller.Controller

class TimerRunnable(controller: Controller) extends Runnable:
    override def run(): Unit =
        try
            var start = System.currentTimeMillis()
            while true do
                if controller.service.status == "waiting for move" then
                    if System.currentTimeMillis() - start >= 1000 then
                        val remaining = (controller.service.makeMoveDeadline - System.currentTimeMillis()) / 1000
                        if remaining <= 0 then controller.broadcastTimeoutDefeat
                        start = System.currentTimeMillis()
                        controller.broadcastRemaining(remaining)
                Thread.sleep(10)
        catch
            case e: InterruptedException => println("Thread interrupted!")