package Algorithms.Minimax;

import java.util.ArrayList;

public class Minimax {

    private static final double INF = 10000000;

    private static class Solution {
        MinimaxProblem instance;
        double result;

        Solution(MinimaxProblem instance, double result) {
            this.instance = instance;
            this.result = result;
        }

        static Solution max(Solution sol1, Solution sol2) {
            if (sol2 == null || (sol1 != null && sol1.result >= sol2.result)) return sol1;
            else return sol2;
        }

        static Solution min(Solution sol1, Solution sol2) {
            if (sol2 == null || (sol1 != null && sol1.result <= sol2.result)) return sol1;
            else return sol2;
        }
    }

    public static int minimax(MinimaxProblem root, int maxDepth) {
        Solution solution = alphabeta(root, -INF, INF, true, maxDepth);
        ArrayList<MinimaxProblem> list = root.children();
        for (int i = 0; i < list.size(); ++i) {
            if (list.get(i) != null && list.get(i).equals(solution.instance)) return i;
        }
        return -1;
    }


    private static Solution alphabeta(MinimaxProblem state, double alpha, double beta, boolean isMaximizing, int maxdepth) {
        if (state.isLeaf() || maxdepth == 0) {
            return new Solution(state, state.heuristicValue());
        }
        if (isMaximizing) {
            Solution maxSolution = new Solution(null, -INF);
            for (MinimaxProblem s : state.children()) {
                if (s == null) continue;
                Solution solution = new Solution(s, alphabeta(s, alpha, beta, s.isMaximizing(), maxdepth - 1).result);
                maxSolution = Solution.max(maxSolution, solution);
                alpha = Math.max(alpha, maxSolution.result);
                if (alpha >= beta) break; //beta-cut-off
            }
            return maxSolution;

        } else {
            Solution minSolution = new Solution(null, INF);

            for (MinimaxProblem s : state.children()) {
                if (s == null) continue;
                Solution solution = new Solution(s, alphabeta(s, alpha, beta, s.isMaximizing(), maxdepth - 1).result);
                minSolution = Solution.min(minSolution, solution);
                beta = Math.min(beta, minSolution.result);
                if (alpha >= beta) break; //alpha-cut-off
            }
            return minSolution;
        }
    }


}
