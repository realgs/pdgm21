package Mancala.Heuristics;


import Algorithms.Minimax.Minimax;
import Mancala.Board.MancalaBoard;

public abstract class MancalaHeuristic {

    public int selectMove(MancalaBoard board, int depth) {
        int bin = 0;
        try {
            bin = Minimax.minimax(board, depth) + 1; // index starts from 0 but bin from 1
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        return bin;
    }


    public static MancalaHeuristic intToStrategy(int i) {
        if (i == 1) {
            return new Heuristic1();
        }
        return new UserHeuristic();
    }


    public abstract int getHeuristicValue(MancalaBoard board);
}
