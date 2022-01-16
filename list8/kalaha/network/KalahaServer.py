from QtPyNetwork.server import QThreadedServer
from qtpy.QtCore import Signal, Slot, QObject

import json
import logging

from kalaha.models import Player, Board


class KalahaServer(QThreadedServer):

    def __init__(self, board_size, stones_count, timeout, parent=None):
        super(KalahaServer, self).__init__(parent)
        self.logger = logging.getLogger(self.__class__.__name__)
        self.set_device_model(Player)
        self.board = Board(board_size, stones_count, timeout, parent=self)

    @Slot(str, int)
    def start(self, ip: str, port: int):
        logging.info("Starting server on {}:{}".format(ip, port))
        return super(KalahaServer, self).start(ip, port)

    @Slot(Player, str, int)
    def on_connected(self, player: Player, ip: str, port: int):
        if self.board.players_connected():
            self.kick(player)
        else:
            self.board.connect_player(player)

    @Slot(Player)
    def on_disconnected(self, player: Player):
        """Notify second player that the first player has disconnected"""
        self.board.timer.stop()
        for player in self.get_devices():
            player.opponent_disconnected()
        if len(self.get_devices()) == 0:
            self.board.default()
            self.logger.info("No players connected, resetting board")

    @Slot(Player, bytes)
    def on_message(self, player: Player, data: bytes):
        """
        Handle messages from client.
        """
        message = json.loads(data)
        self.logger.debug("Received message: {}".format(message))
        event = message.get("event")
        if event == "move":
            self.board.make_move(player, message.get("pit_index"))

    @Slot(Player, dict)
    def write(self, player: Player, data: dict):
        data = json.dumps(data).encode()
        super().write(player, data)

    @Slot(dict)
    def write_all(self, data: dict):
        for player in self.players:
            self.write(player, data)
