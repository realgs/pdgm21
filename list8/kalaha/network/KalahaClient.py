from qtpy.QtCore import Slot, Signal
from QtPyNetwork.client import QThreadedClient

from kalaha.models import Board

import json
import logging


class KalahaClient(QThreadedClient):
    # board changing / errors signals
    invalid_move = Signal(str)
    your_move = Signal(bool, int, str)
    setup_board = Signal(Board, list, int)
    update_board = Signal(Board)
    turn_timeout = Signal()

    # oponent status
    opponent_connected = Signal()
    opponent_disconnected = Signal()
    opponent_not_connected = Signal()

    # game result
    you_won = Signal()
    you_lost = Signal()
    you_tied = Signal()

    def __init__(self):
        super(KalahaClient, self).__init__()
        self.logger = logging.getLogger(self.__class__.__name__)

    @Slot(bytes)
    def on_message(self, data: bytes):
        message = json.loads(data)
        self.logger.debug("Received message: {}".format(message))
        event = message.get("event")
        if event == "invalid_move":
            self.invalid_move.emit(message.get("error"))
        elif event == "turn_timeout":
            self.turn_timeout.emit()
        elif event == "your_move":
            self.your_move.emit(message.get("value"), message.get("timeout"), message.get("text"))
        elif event == "setup_board":
            self.setup_board.emit(Board.deserialize(message.get("board")), message.get("allowed_pits_range"),
                                  message.get("player_number"))
        elif event == "update_board":
            board = Board.deserialize(message.get("board"))
            self.update_board.emit(board)
        elif event == "you_won":
            self.you_won.emit()
        elif event == "you_lost":
            self.you_lost.emit()
        elif event == "you_tied":
            self.you_tied.emit()
        elif event == "opponent_connected":
            self.opponent_connected.emit()
        elif event == "opponent_disconnected":
            self.opponent_disconnected.emit()
        elif event == "opponent_not_connected":
            self.opponent_not_connected.emit()

    @Slot(dict)
    def write(self, message: dict):
        data = json.dumps(message).encode()
        super().write(data)

    @Slot(int)
    def make_move(self, pit_index: int):
        self.write({"event": "move", "pit_index": pit_index})
