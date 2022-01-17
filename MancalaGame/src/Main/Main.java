package Main;


import Mancala.Heuristics.MancalaHeuristic;
import Mancala.Board.MancalaBoard;
import MyScanner.MyScanner;

import java.io.*;

public class Main {

    public static final int MAX_DEPTH = 15;
    private static final int nMaxStages = 150;



    public static void main(String[] args) throws IOException {
        int heuristics[] = {0, 0};
        for (int i = 0; i <= 1; ++i) {
            heuristics[i] = MyScanner.readInt(0, 4, "Select a heuristic 0 or 1 for player " + i + " (O for Human): ");
        }
        int depth = MAX_DEPTH;
        if (heuristics[0] != 0 || heuristics[1] != 0) depth = MyScanner.readInt(1, 15, "Enter Depth to search @ each node: ");
        playLoop(selectStrategy(heuristics[0]), selectStrategy(heuristics[1]), depth);

    }



    ///=================================================== IO Methods ======================================================////
    private static MancalaHeuristic selectStrategy(int n) {
        return MancalaHeuristic.intToStrategy(n);
    }

    public static int playLoop(MancalaHeuristic s0, MancalaHeuristic s1, int MAX_DEPTH) {
        int bins = 6;
        int stones = 6;

        MancalaBoard.printBoardConfiguration();
        return play(bins, stones, s0, s1, MAX_DEPTH);
    }

    private static int play(int bins, int stones, MancalaHeuristic heuristic1, MancalaHeuristic heuristic2, int MAX_DEPTH) {
        MancalaBoard board = new MancalaBoard(bins, stones, heuristic1, heuristic2, MAX_DEPTH);

        System.out.println(board);
        int round = 0;
        while (!board.isGameOver() && round < nMaxStages) {
            System.out.println("------------" + round + "--------------");
            int currentPlayer = board.currentPlayer();
            System.out.println("Player " + currentPlayer + "'s move.");
            int bin = board.move();
            if (bin <= 0) break;
            System.out.println("Player " + currentPlayer + " selects "
                    + board.stonesMoved() + " stones from bin " + bin);
            System.out.println(board);
            System.out.println("\n\n\n--------------------------\n\n\n");
            round++;
        }
        System.out.println("Final board configuration:\n");
        System.out.println(board);
        if (board.getBin(0, 0) == board.getBin(1, 0)) {
            System.out.println("The game ends in a tie!");
            return -1;
        } else if (board.getBin(0, 0) > board.getBin(1, 0)) {
            System.out.println("Player0 wins!");
            return 0;
        } else {
            System.out.println("Player1 wins!");
            return 1;
        }
    }


}
