from qtpy.QtCore import Slot
from QtPyNetwork.models import Device


class Player(Device):

    def __init__(self, server, id, ip, port):
        super(Player, self).__init__(server, id, ip, port)
        self.allowed_pits_range = None

    @Slot(dict)
    def write(self, message: dict):
        self.server().write(self, message)

    @Slot(dict, list, int)
    def setup_board(self, board, allowed_pits_range, player_number):
        self.write({"event": "setup_board",
                    "allowed_pits_range": allowed_pits_range,
                    "player_number": player_number,
                    "board": board})

    @Slot()
    def opponent_not_connected(self):
        self.write({"event": "opponent_not_connected"})

    @Slot()
    def opponent_disconnected(self):
        self.write({"event": "opponent_disconnected"})

    @Slot()
    def opponent_connected(self):
        self.write({"event": "opponent_connected"})

    @Slot(bool, str, int)
    def your_move(self, value, timeout, text=None):
        self.write({"event": "your_move", "value": value, "text": text, "timeout": timeout})

    @Slot(str)
    def invalid_move(self, move):
        self.write({"event": "invalid_move", "error": move})

    @Slot(dict)
    def update_board(self, board):
        self.write({"event": "update_board", "board": board})

    # game results
    @Slot()
    def you_won(self):
        self.write({"event": "you_won"})

    @Slot()
    def you_lost(self):
        self.write({"event": "you_lost"})

    @Slot()
    def you_tied(self):
        self.write({"event": "you_tied"})

    def turn_timeout(self):
        self.write({"event": "turn_timeout"})
