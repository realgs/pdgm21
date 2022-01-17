package Algorithms.Minimax;

import java.util.ArrayList;

/// any minimax problem has to implement this interface properly to use Minimax algorithm class
public interface MinimaxProblem {
	
	double heuristicValue();
	
	ArrayList<MinimaxProblem> children();
	
	boolean isLeaf();
	
	boolean isMaximizing();
	
	boolean equals(MinimaxProblem o);
}
