from qtpy.QtWidgets import (QDialogButtonBox, QMenu, QWidgetAction, QAction)
from qtpy.QtCore import Slot, QTimer, QThread
from qrainbowstyle.windows import (FramelessWindow, FramelessWarningMessageBox, FramelessInformationMessageBox,
                                   FramelessCriticalMessageBox)
from qrainbowstyle.widgets import StylePickerHorizontal

import logging

from kalaha.network import KalahaClient
from kalaha.models import Board, AutoPlayer
from kalaha.widgets import BoardWidget, InfoWidget, StatusWidget


class BoardWindow(FramelessWindow):

    def __init__(self, parent=None):
        super(BoardWindow, self).__init__(parent)
        self.logger = logging.getLogger(self.__class__.__name__)

        self.player_number = 0
        self.client = KalahaClient()
        self.auto_player_worker: AutoPlayer = None
        self.auto_player_thread: QThread = None
        self.timeout = 0
        self.board = []

        self.timer = QTimer(self)
        self.timer.timeout.connect(self.on_timer_timeout)

        # connection
        self.client.connected.connect(self.on_client_connected)
        self.client.failed_to_connect.connect(self.on_failed_to_connect)

        # board changes
        self.client.invalid_move.connect(self.on_invalid_move)
        self.client.update_board.connect(self.on_update_board)
        self.client.setup_board.connect(self.on_setup_board)
        self.client.your_move.connect(self.on_your_move)
        self.client.turn_timeout.connect(self.on_turn_timeout)

        # opponent status
        self.client.opponent_connected.connect(self.on_opponent_connected)
        self.client.opponent_not_connected.connect(lambda: self.show_info_dialog("Opponent not connected!"))
        self.client.opponent_disconnected.connect(self.on_opponent_disconnected)

        # game result
        self.client.you_won.connect(lambda: self.on_game_result("You won!"))
        self.client.you_lost.connect(lambda: self.on_game_result("You lost!"))
        self.client.you_tied.connect(lambda: self.on_game_result("Draw!"))

        # windows
        self.message_box = None

        # main widgets
        self.status_widget = StatusWidget(self)
        self.status_widget.set_status("Connecting to server...")
        self.addContentWidget(self.status_widget)

        self.board_widget = BoardWidget(self)
        self.addContentWidget(self.board_widget)

        self.info_widget = InfoWidget(self)
        self.addContentWidget(self.info_widget)

        # other widgets
        self.menu = QMenu(self)
        self.style_picker_action = QWidgetAction(self.menu)
        self.style_picker = StylePickerHorizontal(self)
        self.style_picker_action.setDefaultWidget(self.style_picker)

        self.auto_player_checkbox = QAction("Auto move", self.menu)
        self.auto_player_checkbox.setCheckable(True)
        self.auto_player_checkbox.setChecked(False)
        self.menu.addAction(self.auto_player_checkbox)

        self.menu.addAction(self.style_picker_action)
        self.menu.setTitle("Options")
        self.addMenu(self.menu)

    @Slot(str, int)
    def connect_to_server(self, host: str, port: int):
        self.logger.debug(f"Connecting to server: {host}:{port}")
        self.client.start(host, port)

    @Slot(str, int)
    def on_client_connected(self, host: str, port: int):
        self.logger.info(f"Connected to {host}:{port}")
        self.status_widget.set_status("Waiting for opponent...")
        self.board_widget.pit_clicked.connect(self.on_pit_clicked)

    @Slot(str, int)
    def on_failed_to_connect(self, host: str, port: int):
        self.status_widget.set_status(f"Failed to connect to {host}:{port}")

    @Slot()
    def on_opponent_connected(self):
        self.status_widget.hide()
        self.board_widget.show()
        self.info_widget.show()

    @Slot()
    def on_opponent_disconnected(self):
        self.timer.stop()
        self.auto_player_worker.stop.emit()
        self.client.disconnect_from_server()
        self.logger.debug("Opponent disconnected!")
        if self.message_box:
            self.message_box.close()
        self.board_widget.setEnabled(False)
        self.message_box = FramelessCriticalMessageBox(self)
        self.message_box.setStandardButtons(QDialogButtonBox.Ok)
        self.message_box.button(QDialogButtonBox.Ok).clicked.connect(self.close)
        self.message_box.setText("Opponent disconnected! Click OK to close.")
        self.message_box.show()

    # Board events
    @Slot(Board, list, int)
    def on_setup_board(self, board, allowed_pits, player_number):
        """
        Setup the board and pits
        """
        self.board = board.board
        self.player_number = player_number
        self.info_widget.set_player_number(player_number + 1)
        self.board_widget.setup_board(board, allowed_pits)

    @Slot(Board)
    def on_update_board(self, board: Board):
        """
        Update board widget with new values
        """
        self.board = board.board
        self.board_widget.update_pits(board)

    @Slot(bool, int, str)
    def on_your_move(self, your_move: bool, timeout: int, message: str):
        self.info_widget.set_timeout(timeout)
        self.info_widget.set_player_turn(message)
        if timeout > 0:
            self.timeout = timeout
            self.timer.start(1000)

        if your_move and self.auto_player_checkbox.isChecked():
            self.auto_player_worker.calculate_move.emit(self.board, self.player_number)
        self.show_info_dialog(message)

    @Slot(int)
    def on_pit_clicked(self, pit_index: int):
        if not self.client.is_running():
            return
        if self.auto_player_checkbox.isChecked():
            self.show_info_dialog("Auto play is on!")
            return
        self.client.make_move(pit_index)

    @Slot(str)
    def on_invalid_move(self, message: str):
        self.logger.debug(f"Invalid move: {message}")
        if self.message_box:
            self.message_box.close()
        self.message_box = FramelessWarningMessageBox(self)
        self.message_box.setStandardButtons(QDialogButtonBox.Ok)
        self.message_box.button(QDialogButtonBox.Ok).clicked.connect(self.message_box.close)
        self.message_box.setText(message)
        self.message_box.show()

    @Slot(str)
    def on_game_result(self, message):
        self.timer.stop()
        self.info_widget.set_player_turn(message)
        self.show_info_dialog(message)

    @Slot()
    def on_turn_timeout(self):
        if self.auto_player_worker:
            self.auto_player_worker.stop.emit()

    @Slot(bool, int, int, bool, bool)
    def setup_auto_play(self, auto_play: bool, auto_play_delay: int,
                        minimax_depth: int, no_alpha_beta: bool, iterative_deepening: bool):
        self.info_widget.set_auto_play_options(auto_play, minimax_depth, auto_play_delay,
                                               no_alpha_beta, iterative_deepening)
        self.auto_player_checkbox.setChecked(auto_play)

        self.auto_player_worker = AutoPlayer(minimax_depth, auto_play_delay,
                                             no_alpha_beta, iterative_deepening)
        self.auto_player_thread = QThread()
        self.auto_player_thread.moveToThread(QThread.currentThread())
        self.auto_player_worker.moveToThread(self.auto_player_thread)
        self.auto_player_worker.finished.connect(self.auto_player_thread.quit)
        self.auto_player_worker.make_move.connect(self.client.make_move)

        if auto_play:
            self.auto_player_thread.start()
        else:
            self.info_widget.set_auto_play_options_visible(False)

    @Slot()
    def on_timer_timeout(self):
        self.timeout -= 1
        self.info_widget.set_timeout(self.timeout)
        if self.timeout == 0:
            self.timer.stop()

    # Dialogs to show messages
    @Slot(str)
    def show_info_dialog(self, message: str):
        if self.message_box:
            self.message_box.close()
        self.message_box = FramelessInformationMessageBox(self)
        self.message_box.setStandardButtons(QDialogButtonBox.Ok)
        self.message_box.button(QDialogButtonBox.Ok).clicked.connect(self.message_box.close)
        self.message_box.setText(message)
        self.message_box.show()

    @Slot()
    def show(self):
        """
        Show window with spinner.
        """
        self.info_widget.hide()
        self.board_widget.hide()
        self.status_widget.show()
        super(BoardWindow, self).show()

    @Slot()
    def close(self):
        """
        Close the window and message box if it is open.
        Disconnect from the server and wait for the thread to finish.
        If auto player is running, stop it and wait for the thread to finish.
        """
        self.hide()
        if self.message_box is not None:
            self.message_box.close()
        if self.client.is_running():
            self.client.close()
            self.client.wait()
        if self.auto_player_thread:
            self.auto_player_thread.quit()
            self.auto_player_thread.wait()
        super(BoardWindow, self).close()
