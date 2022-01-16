from qtpy.QtWidgets import QApplication
from qtpy.QtCore import Qt, qInstallMessageHandler

import os
import sys
import logging
import argparse
import traceback
import qrainbowstyle
from qrainbowstyle.extras import qt_message_handler

from kalaha.argparser import parse_args
from kalaha.logger import Logger
from kalaha.windows import BoardWindow
from kalaha.config import __app_name__, __version__
from kalaha.network.KalahaServer import KalahaServer


if __name__ == '__main__':
    logger = Logger()
    logger.enable()
    qInstallMessageHandler(qt_message_handler)

    def exception_hook(exctype, value, tb):
        logging.critical(''.join(traceback.format_exception(exctype, value, tb)))
        sys.exit(1)
    sys.excepthook = exception_hook

    args = parse_args(sys.argv[1:])
    if not args:
        sys.exit(0)
    logger.set_level(logging.getLevelName(args.log_level))

    if args.mode == "server":
        # to run without display environment variable
        # must be set before QApplication is created
        os.environ["QT_QPA_PLATFORM"] = "offscreen"

    QApplication.setAttribute(Qt.AA_UseHighDpiPixmaps)
    # QApplication.setAttribute(Qt.AA_EnableHighDpiScaling)

    QApplication.setQuitOnLastWindowClosed(True)

    app = QApplication(sys.argv)
    app.setApplicationName(__app_name__)
    app.setApplicationVersion(__version__)
    app.setApplicationDisplayName(__app_name__)

    app.setStyleSheet(qrainbowstyle.load_stylesheet(style="QDarkstyle3"))

    if getattr(sys, 'frozen', False):
        application_path = os.path.dirname(sys.executable)
    elif __file__:
        application_path = os.path.dirname(__file__)
    else:
        application_path = "."
    logging.debug("Application path is {}".format(application_path))
    os.chdir(application_path)

    if args.mode == "server":
        # headless server mode
        server = KalahaServer(args.board_size, args.stones_count, args.turn_timeout)
        server.start(args.host, args.port)
    else:
        window = BoardWindow()
        window.connect_to_server(args.host, args.port)
        window.setup_auto_play(args.auto_play, args.auto_play_delay, args.minimax_depth,
                               args.no_alpha_beta, args.iterative_deepening)
        window.show()

    sys.exit(app.exec_())
