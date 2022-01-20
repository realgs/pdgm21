import random
from threading import Timer
import copy


class Player:

    def __init__(self):
        self.godHole = GodHole()
        self.pits = []
        for i in range(0, 6):
            self.pits.append(Pit())

    def __int__(self, pits, god_hole):
        self.pits = pits
        self.godHole = god_hole

    def return_stones(self):
        return self.godHole.return_stones()

    def steal_stones(self, hole):
        self.pits[hole].stones = 0

    def move_pit(self, hole, oponent):
        double_move = False
        a = self.pits[hole - 1].stones
        self.pits[hole - 1].stones = 0
        where_ended = 0
        if a == 0:
            raise ValueError("Can't choose a hole with no stones!")
        for i in range(hole, 6):
            self.pits[i].stones += 1
            a -= 1
            where_ended = i
            if a == 0:
                break
        if a != 0:
            self.godHole.increment_stones()
            a -= 1
            double_move = True
        else:
            if self.pits[where_ended].stones == 1:
                stolen_stones = oponent.pits[5 - where_ended].stones
                # tez nie usuwa kamieni przeciwnika
                oponent.pits[5 - where_ended].stones = 0
                for i in range(0, stolen_stones - 1):
                    self.godHole.increment_stones()
        if double_move == True and a == 0:
            return 666
        else:
            return a

    def move_pit_with(self, hole, stones, oponent):
        a = stones
        where_ended = 0
        if a == 0:
            raise ValueError("Can't choose a hole with no stones!")
        for i in range(hole, 6):
            self.pits[i].stones += 1
            a -= 1
            where_ended = i
            if a == 0:
                break
        if a != 0:
            self.godHole.increment_stones()
            a -= 1
        else:
            if self.pits[where_ended].stones == 1:
                stolen_stones = oponent.pits[5 - where_ended].stones
                # nie usuwa kamieni przeciwnika
                oponent.pits[5 - where_ended].stones = 0
                for i in range(0, stolen_stones - 1):
                    self.godHole.increment_stones()
        return a

    def move_pit_change_player(self, hole, stones_left):
        a = stones_left

        for i in range(hole, 6):
            self.pits[i].stones += 1
            a -= 1
            if a == 0:
                break

        return a

    def is_game_over(self):
        a = 0
        for i in range(0, 6):
            a += self.pits[i].stones
        return a == 0

    def take_all_stones(self):
        for i in range(0, 6):
            a = self.pits[i].stones
            for i in range(0, a):
                self.godHole.increment_stones()
        for i in range(0, 6):
            self.pits[i].stones = 0

    def print_board(self):
        for i in range(0, 6):
            print("   " + str(self.pits[i].stones) + "   ", end=' ')

    def print_board_reverse(self):
        for i in range(0, 6):
            print("   " + str(self.pits[5 - i].stones) + "   ", end=' ')

    def print_hole(self):
        print("God Hole    " + str(self.godHole.return_stones()) + "   ", end=' ')

    def print_hole2(self):
        print("    " + str(self.godHole.return_stones()) + "     " + "God Hole", end=' ')


class Pit:

    def __init__(self):
        self.stones = 4

    def change_stones(self, s):
        self.stones = s

    def get_stones(self):
        return self.stones


class GodHole:

    def __init__(self):
        self.stones = 0

    def return_stones(self):
        return self.stones

    def change_stones(self, s):
        self.stones = s

    def increment_stones(self):
        self.stones += 1


class ArtificialPlayer:

    def choose_hole_tree2(self, player, oponent):
        def minmax(player, oponent, depth, maximizing_player, moves):
            copy_moves = moves
            move = None
            if depth == 0 or player.is_game_over() or oponent.is_game_over():
                # what to return?
                return [moves[0], player.godHole.return_stones() - oponent.godHole.return_stones()]

            if maximizing_player:
                max_eval = -10000
                for i in range(0, 6):
                    try:
                        another_copy = copy.copy(copy_moves)
                        another_copy.append(i)
                        player_copy = copy.deepcopy(player)
                        oponent_copy = copy.deepcopy(oponent)
                        server_copy = Server(player_copy, oponent_copy)
                        double_move = server_copy.move(i, 1)
                        if double_move:
                            move, max_eval = minmax(player_copy, oponent_copy, depth - 1, True, another_copy)

                        else:
                            move, max_eval = minmax(player_copy, oponent_copy, depth - 1, False, another_copy)
                    except:
                        max_eval = max_eval
                return move, max_eval
            else:
                min_eval = 10000
                for i in range(0, 6):
                    try:
                        another_copy = copy_moves
                        another_copy.append(i)
                        player_copy = copy.deepcopy(player)
                        oponent_copy = copy.deepcopy(oponent)
                        server_copy = Server(player_copy, oponent_copy)
                        double_move = server_copy.move(i, 2)
                        if double_move:
                            move, min_eval = minmax(player_copy, oponent_copy, depth - 1, False, another_copy)
                        else:
                            move, min_eval = minmax(player_copy, oponent_copy, depth - 1, True, another_copy)

                    except:
                        min_eval = min_eval
            return move, min_eval

        return minmax(player, oponent, 5, True, [])

    @staticmethod
    def choose_hole_tree(player, oponent):
        for i in range(0, 6):
            if player.pits[i].get_stones() == 6 - i:
                return i + 1
            elif player.pits[i].get_stones() == 13:
                return i + 1
        new_player = player
        new_oponent = oponent
        best_move = [-100, 0]  # dif between players
        for i in range(1, 7):
            worst_move = -100
            for j in range(1, 7):
                artificial_game = Server(player, oponent)
                artificial_game.move(i, new_player)
                artificial_game.move(j, new_oponent)
                if new_player.godHole.return_stones() - new_oponent.godHole.return_stones() < worst_move:
                    worst_move = new_player.godHole.return_stones() - new_oponent.godHole.return_stones()
                if j == 6:
                    if worst_move > best_move[0]:
                        best_move[0] = i

    @staticmethod
    def choose_hole(player, oponent):
        for i in range(0, 6):
            if player.pits[i].get_stones() == 6 - i:
                if player.pits[i].stones != 0:
                    return i + 1
            elif player.pits[i].get_stones() == 13:
                if player.pits[i].stones != 0:
                    return i + 1
        for i in range(0, 6):
            for j in range(0, 6):
                if player.pits[i].get_stones() == j and oponent.pits[5 - i].get_stones() > 0:
                    if player.pits[i].stones != 0:
                        return i + 1
        for i in range(0, 6):
            if player.pits[i].get_stones() > 3:
                for j in range(0, 6):
                    if oponent.pits[j].get_stones() == 5 - j:
                        if player.pits[i].stones != 0:
                            return i + 1
        while True:
            random_choice = random.randrange(6) + 1
            if player.pits[random_choice - 1].stones != 0:
                return random_choice


class Server:

    def __init__(self, player=None, oponent=None):
        if (player is None):
            self.p1 = Player()
            self.p2 = Player()
            # I will use boolean as a turn meaning that true is player's 1 turn
            # false player's 2 turn
            self.stones_left = 0
            self.winner = None
        else:
            self.p1 = player
            self.p2 = oponent
            # I will use boolean as a turn meaning that true is player's 1 turn
            # false player's 2 turn
            self.stones_left = 0
            self.winner = None

    def move(self, hole, player):
        double_move = False
        if player != 1 and player != 2:
            raise ValueError("You can choose either player 1 or 2, but other value has ben input as a player")
        if player == 1:
            double_move = self.move_player_helper(hole, self.p1, self.p2)

        else:
            double_move = self.move_player_helper(hole, self.p2, self.p1)

        if self.p1.is_game_over() or self.p2.is_game_over():
            self.p1.take_all_stones()
            self.p2.take_all_stones()
            if self.p1.godHole.return_stones() > self.p2.godHole.return_stones():
                self.winner = self.p2
            else:
                self.winner = self.p1
        return double_move

    def is_over(self, player):
        if player == 1:
            self.p1.take_all_stones()
        else:
            self.p2.take_all_stones()
        if self.p1.return_stones() > self.p2.return_stones():
            self.winner = "Player 1"
        if self.p1.return_stones() < self.p2.return_stones():
            self.winner = "Player 2"
        if self.p1.return_stones() == self.p2.return_stones():
            self.winner = "No one, it's a draw!"

    def print_board(self):
        print("Player 2", end='\n \n')
        self.p2.print_hole()
        self.p2.print_board_reverse()
        print("\n")
        print("                ", end=' ')
        self.p1.print_board()
        self.p1.print_hole2()
        print("\n \nPlayer 1", end='')

    def play_pvp(self):
        while self.winner is None:
            madeMove = False
            while (madeMove is False):
                try:
                    self.play_turn(1)
                    madeMove = True
                except:
                    print("You chose a hole with no stones, choose a different one")
            madeMove = False
            while (madeMove is False):
                try:
                    self.play_turn(2)
                    madeMove = True
                except:
                    print("You chose a hole with no stones, choose a different one")

    def play_pvc(self):
        artificial_int = ArtificialPlayer()
        while self.winner is None:
            made_move = False
            while made_move is False:
                try:
                    self.play_turn(1)
                    made_move = True
                except:
                    print("You chose a hole with no stones, choose a different one")

            double_move = True
            while double_move:
                over = self.break_iteration_if_over()

                print('psa')
                if over:
                    break
                else:
                    move = artificial_int.choose_hole(self.p2, self.p1)
                    double_move = self.move(move, 2)
                    self.print_board()
                    print()

    def play_pve(self):
        artificial_int = ArtificialPlayer()
        while self.winner is None:
            made_move = False
            while made_move is False:
                try:
                    self.play_turn(1)
                    made_move = True
                except:
                    print("You chose a hole with no stones, choose a different one")

            double_move = True
            while double_move:
                over = self.break_iteration_if_over()

                if over:
                    break
                else:
                    move = artificial_int.choose_hole_tree2(self.p2, self.p1)[0]
                    double_move = self.move(move, 2)
                    self.print_board()
                    print()

    def break_iteration_if_over(self):
        a = self.p1.is_game_over()
        if a:
            if a == 1:
                player_win = "Player1"
            else:
                player_win = "Player2"
            print("Koniec gry")
        return a

    def play_turn(self, player):
        double_move = True
        while double_move:
            over = self.break_iteration_if_over()
            if over:
                break
            print("\n")
            timeout = 500
            if player == 1:
                t = Timer(timeout, print, ['30 seconds passed, Player 2 wins!'])
                t.start()
                move = int(input("Player1 choose hole from 1 to 6:")) % timeout
                t.cancel()
                double_move = self.move(move, 1)
            else:
                t = Timer(timeout, print, ['30 seconds passed, Player 1 wins!'])
                t.start()
                move = int(input("Player2 choose hole from 1 to 6:")) % timeout
                t.cancel()
                double_move = self.move(move, 2)
            self.print_board()
            print()

    def move_player_helper(self, hole, player, oponent):
        double_move = False
        self.stones_left = player.move_pit(hole, oponent)
        if self.stones_left == 666:
            double_move = True
        if 0 < self.stones_left < 600:
            self.stones_left = oponent.move_pit_change_player(0, self.stones_left)
            if self.stones_left > 0:
                self.stones_left = player.move_pit_with(0, self.stones_left, oponent)
                if self.stones_left == 666:
                    double_move = True

        return double_move
