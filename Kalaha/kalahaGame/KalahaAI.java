package kalahaGame;

import java.util.LinkedList;
import java.util.Queue;

public class KalahaAI {
	int player;
	Queue<Integer> moves;
	
	public KalahaAI (int Player) {
		player = Player;
		moves = new LinkedList<Integer>();
		
		if (player > 1)
			player = 1;
		else if (player < 0)
			player = 0;
	}
	
	public int move(int[] board) {
		if (moves.isEmpty())
			makeSimulation(board);
		
		return (moves.poll() + 1);
	}
	
	private void makeSimulation(int[] board) {
		Node root = new Node(board);
		root.simulate(player);
		
		moves = root.findBestResult();
	}
	
	private class Node {
		int[] board;
		Node[] simulations;
		Queue<Integer> movesQueue;
		
		Node (int[] Board) {
			board = Board;
			simulations = new Node[6];
			movesQueue = new LinkedList<Integer>();
		}
		
		void simulate(int playerNumber) {
			for (int i = 0; i < simulations.length; i++) {
				int offset = 0;
				
				if (playerNumber == 1)
					offset = 7;
				
				if (board[i + offset] != 0) {
					int[] simulatedBoard = board.clone();
					boolean anotherTurn = executeMove(i, simulatedBoard, playerNumber);
					
					simulations[i] = new Node(simulatedBoard);
					simulations[i].movesQueue.addAll(movesQueue);
					
					if (playerNumber == player) {
						simulations[i].movesQueue.add(i);
					
					if (anotherTurn) 
						simulations[i].simulate(playerNumber);
					else 
						simulations[i].simulate(playerNumber == 0 ? 1 : 0);
					}
				}	
			}
		}
		
		int getResults() {
			if (player == 0)
				return board[6] - board[13];
			else
				return board[13] - board[6];
		}
		
		boolean executeMove(int move, int[] boardToSimulate, int playerNumber) {
			if (playerNumber == 1)
				move += 7;
			
			int steps = boardToSimulate[move];
			boardToSimulate[move] = 0;
			int index = move;
			
			for (int i = 0; i < steps; i++) {
				if (++index > 13)
					index = 0;
				
				boardToSimulate[index]++;
			}
			
			if (index != 6 && index != 13 && boardToSimulate[index] == 1 && boardToSimulate[12 - index] != 0) {
				if (index < 6 && playerNumber == 0) {
					boardToSimulate[6] += boardToSimulate[12 - index];
					boardToSimulate[6]++;
					boardToSimulate[12 - index] = 0;
					boardToSimulate[index] = 0;
				}
				else if (index > 6 && playerNumber == 1){
					boardToSimulate[13] += boardToSimulate[12 - index];
					boardToSimulate[13]++;
					boardToSimulate[12 - index] = 0;
					boardToSimulate[index] = 0;
				}
			} else if ((index == 6 && playerNumber == 0) || (index == 13 && playerNumber == 1)) {
					return true;
			}
			
			return false;
		}
		
		boolean areSimulationsNull() {
			for (int i = 0; i < simulations.length; i++)
				if (simulations[i] != null)
					return false;
			
			return true;
		}
		
		Queue<Integer> findBestResult() {
			//Integer bestResult = Integer.MIN_VALUE;
			BestResult bestResult = new BestResult();
			
			return find(this, bestResult);
		}
		
		Queue<Integer> find(Node n, BestResult best) {
			if (n.areSimulationsNull()) {
				if (n.getResults() > best.result) {
					best.result = n.getResults();
					return n.movesQueue;
				}
				else 
					return null;
			}
			else {
				Queue<Integer> bestMoves = null;
				
				for (int i = 0; i < n.simulations.length; i++) {
					if (n.simulations[i] != null) {
						Queue<Integer> possibleMoves = find(n.simulations[i], best);
						
						if (possibleMoves != null)
							bestMoves = possibleMoves;
					}	
				}
				
				return bestMoves;
			}
		}
		
		private class BestResult {
			int result;
			
			public BestResult() {
				result = Integer.MIN_VALUE;
			}
		}
	}	

}
