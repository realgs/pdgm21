from qtpy.QtCore import QObject, Signal

import logging
import coloredlogs


class SignalHandler(QObject, logging.StreamHandler):
    log = Signal(str)

    def __init__(self):
        super(SignalHandler, self).__init__()

    def emit(self, record: logging.LogRecord) -> None:
        log = self.format(record)
        if isinstance(log, str):
            self.log.emit(log)


class Logger:
    def __init__(self):
        super(Logger, self).__init__()
        self.logger = None
        self.handler = None
        self.formatter = None
        self.signal_handler = None

    def enable(self):
        self.logger = logging.getLogger()
        self.logger.setLevel(logging.NOTSET)

        self.handler = logging.StreamHandler()
        self.handler.setLevel(logging.NOTSET)

        self.formatter = coloredlogs.ColoredFormatter("%(asctime)s "
                                                      "[%(threadName)s] "
                                                      "[%(name)s] "
                                                      "[%(levelname)s] "
                                                      "%(message)s")
        self.handler.setFormatter(self.formatter)
        self.logger.addHandler(self.handler)
        self.logger.info("Logger enabled")
        return self.logger

    def set_level(self, level):
        if self.logger and self.handler:
            self.logger.setLevel(level)
            self.handler.setLevel(level)
        else:
            raise Exception("Logger not enabled!")

    def install_signal_handler(self):
        """Install signal handler."""
        self.signal_handler = SignalHandler()
        self.signal_handler.setLevel(logging.NOTSET)
        self.logger.addHandler(self.signal_handler)
        return self.signal_handler.log
