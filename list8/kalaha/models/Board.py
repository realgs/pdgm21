from qtpy.QtCore import QObject, Signal, Slot, QTimer

import random
import logging

from kalaha.models import Player


class Board(QObject):

    def __init__(self, board_size, stones_count, timeout=0, board=None, parent=None):
        super(Board, self).__init__(parent)
        self.logger = logging.getLogger(self.__class__.__name__)

        self.board_size = board_size
        self.stones_count = stones_count
        self.timeout = timeout
        self.game_over = False

        self.player_one_base_index = self.board_size
        self.player_two_base_index = self.board_size * 2 + 1

        if board is None:
            self.default()
        else:
            self.board = board

        self.player_one: Player = None
        self.player_two: Player = None
        self.current_player: Player = None

        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.timer.setInterval(self.timeout * 1000)
        if self.timeout > 0:
            self.timer.timeout.connect(self.on_timer_timeout)

    @Slot()
    def on_timer_timeout(self):
        self.current_player.turn_timeout()
        self.current_player.your_move(False, self.timeout, "Time's up! Your opponent's turn!")
        self.current_player = self.get_next_player()
        self.current_player.your_move(True, self.timeout, "Your turn!")
        self.timer.start()

    @Slot()
    def select_first_player(self) -> Player:
        """
        Selects a random player to start the game.
        """
        if random.randint(0, 1) == 0:
            return self.player_one
        else:
            return self.player_two

    @Slot()
    def get_next_player(self):
        if self.players_connected():
            if self.player_one is self.current_player:
                return self.player_two
            else:
                return self.player_one

    @Slot(Player)
    def connect_player(self, player: Player):
        if not self.players_connected():
            if self.player_one is None:
                self.player_one = player
                self.player_one.allowed_pits_range = range(self.board_size)
            else:
                self.player_two = player
                self.player_two.allowed_pits_range = range(self.board_size + 1, self.board_size * 2 + 1)
        if self.players_connected():
            self.logger.info("Both players connected")
            self.current_player = self.select_first_player()
            self.logger.info(f"Starting game with player {self.current_player.id()}")

            self.player_one.opponent_connected()
            self.player_two.opponent_connected()
            self.player_one.setup_board(self.serialize(), list(self.player_one.allowed_pits_range), 0)
            self.player_two.setup_board(self.serialize(), list(self.player_two.allowed_pits_range), 1)
            self.current_player.your_move(True, self.timeout, "You start!")
            self.get_next_player().your_move(False, self.timeout, "Opponent's turn!")
            self.timer.start()

    @Slot()
    def players_connected(self) -> bool:
        return (self.player_one is not None and
                self.player_one.is_connected() and
                self.player_two is not None and
                self.player_two.is_connected())

    @Slot(Player, int)
    def make_move(self, player: Player, pit_index: int):
        if not self.players_connected():
            if not self.player_one is None:
                self.player_one.opponent_not_connected()
            elif not self.player_two is None:
                self.player_two.opponent_not_connected()
        elif self.game_over:
            player.invalid_move("Game is over!")
        elif self.current_player != player:
            player.invalid_move("It's not your turn!")
        elif pit_index not in player.allowed_pits_range:
            self.logger.debug(f"Pit index {pit_index} not in range {player.allowed_pits_range}")
            player.invalid_move("You can't move stones from this pit")
        elif self.board[pit_index] == 0:
            player.invalid_move("There are no stones in this pit")
        else:
            # move is valid, stop the timer
            self.timer.stop()

            stones = self.board[pit_index]
            self.board[pit_index] = 0
            while stones > 0:
                pit_index += 1
                if pit_index == self.board_size * 2 + 2:
                    pit_index = 0
                self.board[pit_index] += 1
                stones -= 1

            # if last stone was placed in player's base
            if (self.current_player is self.player_one and pit_index == self.player_one_base_index or
                    self.current_player is self.player_two and pit_index == self.player_two_base_index):
                if not self.check_game_over():
                    self.update_boards()
                    self.current_player.your_move(True, self.timeout, "You get another turn!")
                    self.get_next_player().your_move(False, self.timeout, "Your opponent gets another turn!")
                    self.timer.start()
            else:
                # check if last stone was put in the empty pit which is in player's allowed range
                text = "Your opponent's turn!"
                if self.board[pit_index] == 1:
                    opposite_hole_index = self.board_size * 2 - pit_index
                    opposite_hole_stones = self.board[opposite_hole_index]

                    if self.current_player is self.player_one and pit_index in self.player_one.allowed_pits_range:
                        self.board[self.player_one_base_index] += opposite_hole_stones
                        self.board[opposite_hole_index] = 0
                        if opposite_hole_stones > 0:
                            text = f"Great choice! You get additional {opposite_hole_stones} stones!"

                    elif self.current_player is self.player_two and pit_index in self.player_two.allowed_pits_range:
                        self.board[self.player_two_base_index] += opposite_hole_stones
                        self.board[opposite_hole_index] = 0
                        if opposite_hole_stones > 0:
                            text = f"Great choice! You get additional {opposite_hole_stones} stones!"

                if not self.check_game_over():
                    self.update_boards()
                    self.current_player.your_move(False, self.timeout, text)
                    self.current_player = self.get_next_player()
                    self.current_player.your_move(True, self.timeout, "Your turn!")
                    self.timer.start()

    @Slot()
    def update_boards(self):
        board = self.serialize()
        self.player_one.update_board(board)
        self.player_two.update_board(board)

    @Slot()
    def check_game_over(self):
        # handling empty pits
        if all(self.board[pit_index] == 0 for pit_index in self.player_one.allowed_pits_range):
            # if all player one's pits are empty, add all stones from player two's pits to player one's base
            self.board[self.player_two_base_index] += sum(self.board[pit_index]
                                                          for pit_index in self.player_two.allowed_pits_range)
            for i in self.player_two.allowed_pits_range:
                self.board[i] = 0
            self.game_over = True

        elif all(self.board[pit_index] == 0 for pit_index in self.player_two.allowed_pits_range):
            # if all player two's pits are empty, add all stones from player one's pits to player two's base
            self.board[self.player_one_base_index] += sum(self.board[pit_index]
                                                          for pit_index in self.player_one.allowed_pits_range)
            for i in self.player_one.allowed_pits_range:
                self.board[i] = 0
            self.game_over = True

        if self.game_over:
            self.update_boards()
            self.timer.stop()
            if self.board[self.player_one_base_index] > self.board[self.player_two_base_index]:
                self.player_one.you_won()
                self.player_two.you_lost()
            elif self.board[self.player_one_base_index] < self.board[self.player_two_base_index]:
                self.player_one.you_lost()
                self.player_two.you_won()
            else:
                self.player_one.you_tied()
                self.player_two.you_tied()
        return self.game_over

    @Slot()
    def default(self):
        self.board = [self.stones_count] * (self.board_size * 2 + 2)
        # bases should be empty
        self.board[self.player_one_base_index] = 0
        self.board[self.player_two_base_index] = 0

        self.player_one: Player = None
        self.player_two: Player = None
        self.current_player: Player = None
        self.game_over = False

    @Slot()
    def serialize(self) -> dict:
        return {"board_size": self.board_size,
                "stones_count": self.stones_count,
                "board": self.board,
                }

    @staticmethod
    def deserialize(data: dict, parent=None) -> 'Board':
        return Board(data["board_size"],
                     data["stones_count"],
                     board=data["board"],
                     parent=parent)
