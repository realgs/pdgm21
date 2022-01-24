package kalahaServer;

import java.io.*;
import java.net.*;
import java.util.ArrayList;

import kalahaGame.KalahaGame;

public class KalahaServer {
	
	ArrayList<GameInstance> gamesList;
	ArrayList<ClientService> clients;
		
	public static void main(String[] args) {
		KalahaServer server = new KalahaServer();
		server.work();
	}
	
	public void work() {
		gamesList = new ArrayList<GameInstance>();
		clients = new ArrayList<ClientService>(); 
		
		try (ServerSocket serverSocket = new ServerSocket(5000)){
			while(true) {
				Socket socket = serverSocket.accept();
				
				clients.add(new ClientService(socket, clients.size()));
				new Thread(clients.get(clients.size() - 1)).start();
				
				System.out.println("Connected.");
			}
		} 
		catch(IOException ex) {
			ex.printStackTrace();
		}
	}
	
	/*void playGame(int gameIndex) throws IOException {
		GameInstance game = gamesList.get(gameIndex);
		int whatPlayer = 0;
		String command = "";
		ClientService player1 = clients.get(game.player1);
		ClientService player2 = clients.get(game.player2);
		
		while(!"GameEnd".equalsIgnoreCase(command)) {
			if ((whatPlayer % 2) == 0) {
				while(!"TurnEnd".equalsIgnoreCase(command)) {
					command = player1.reader.readLine();
					System.out.println(command);
					player2.writer.println(command);
				}
				
				player2.writer.println("YourTurn");
			}
			else {
				//player2.writer.println("Player 2\nHole number? ");
				command = player2.reader.readLine();
				while(!"TurnEnd".equalsIgnoreCase(command)) {
					command = player2.reader.readLine();
					System.out.println(command);
					player1.writer.println(command);
				}
				
				player1.writer.println("YourTurn");
			}
			
			whatPlayer++;
		}
		
		player2.gameEnded = true;
	}*/
	
	private String printGames() {
		String result = "";
		
		for (int i = 0; i < gamesList.size(); i++) {
			if (gamesList.get(i).player2 == -1) {
				result = result.concat((i + 1) + ". " + gamesList.get(i).stonesNumber + " stones in a hole.");
			}
		}
		
		return result;
	}
	
	private class ClientService implements Runnable {
		Socket clientSocket;
		InputStreamReader strReader;
		BufferedReader reader;
		PrintWriter writer;
		int index;
		int gameIndex;
		
		ClientService(Socket socket, int Index) {
			index = Index;
			gameIndex = -1;
			
			try {
				clientSocket = socket;
				strReader = new InputStreamReader(clientSocket.getInputStream());
				reader = new BufferedReader(strReader);
				writer = new PrintWriter(clientSocket.getOutputStream());
			} 
			catch(Exception ex) {
				ex.printStackTrace();
			}
		}
		
		public void run() {
			try {
				String command = "";
				while(!"stop".equalsIgnoreCase(command)) {
					switch (command = reader.readLine()) {
					case "createGame":
						createGame();
						command = "stop";
						break;
						
					case "joinGame":
						joinGame();
						command = "stop";
						break;
						
					default:
						writer.println("Wrong command!");
						writer.flush();
						break;
					}
				}
				
				System.out.println("Connection ended.");
				
				clientSocket.close();
				writer.close();
				reader.close();
			} 
			catch (IOException e1) {
				e1.printStackTrace();
			}
		}
		
		void createGame() throws IOException {
			writer.println("How many stones in a hole? ");
			writer.flush();
			
			int stones = Integer.parseInt(reader.readLine());
			
			gamesList.add(new GameInstance(stones, index));
			
			System.out.println("Created game");
			
			gameIndex = gamesList.size() - 1;
			
			while (!gamesList.get(gameIndex).gameStarted) {}
			
			writer.println("Connected with another player! Game starts!");
			writer.flush();
			
			System.out.println("Game begins");
			playGame();
		}
		
		void joinGame() throws IOException {
			writer.println("Which game you want to join?");
			writer.println(printGames());
			writer.flush();
			
			int choice = Integer.parseInt(reader.readLine()) - 1;
			
			if (choice >= 0 && choice < gamesList.size() && gamesList.get(choice).player2 == -1) {
				gameIndex = choice;
				gamesList.get(choice).setPlayer(index);
				
				writer.println(gamesList.get(choice).stonesNumber);
				writer.flush();
			}
			else {
				writer.println("Wrong number!");
				writer.flush();
				return;
			}
			
			System.out.println("Game joined");
			playGame();
		}
		
		void playGame() throws IOException {
			GameInstance gameBuffer = gamesList.get(gameIndex);
			KalahaGame game = new KalahaGame(gameBuffer.stonesNumber);
			
			String command = "";
			
			if (index == gameBuffer.player2) {
				writer.println(gameBuffer.take(index));
				writer.flush();
			}
			
			while (!game.ifEndOfGame()) {
				int correctMove = -1;
				
				while (correctMove == -1 || correctMove == 1) {
					writer.println("Hole number? ");
					writer.flush();	
					
					command = reader.readLine();
					System.out.println("Player" + index + "move: " + command);
					
					gameBuffer.put(index, command);
					correctMove = game.makeMove(Integer.parseInt(command));
					
					if (correctMove == 1) 
						writer.println("You have another turn!");
					else if (correctMove == 0)
						writer.println("EndTurn");
					else 
						writer.println("Wrong move!");

					writer.flush();	
						
				}
					
				command = gameBuffer.take(index);
				writer.println(command);
				game.makeMove(Integer.parseInt(command));
				writer.println("YourTurn");
				writer.flush();
				command = "";	
			}
			
		}
		
	}
	
	private class GameInstance {
		int stonesNumber;
		int player1;
		int player2;
		volatile int turn;
		volatile boolean gameStarted;
		ArrayList<String> buffer;
		
		public GameInstance(int StonesNumber, int Player1) {
			stonesNumber = StonesNumber;
			player1 = Player1;
			player2 = -1;
			gameStarted = false;
			turn = player1;
			buffer = new ArrayList<String>();
		}
		
		public synchronized void put(int index, String s) {
	        while (index != turn) {
	        	try {
		        	  wait(); 
		         } 
		        catch (InterruptedException e) {
		        	System.out.println(e);
		        }
	        }
	        
	        buffer.add(s);
	        notify();
	    }

	    public synchronized String take(int index) {
	        while (index == turn || buffer.isEmpty()) {
	        	try {
		        	  wait(); 
		         } 
		        catch (InterruptedException e) {
		        	System.out.println(e);
		        }
	        }
	        
	        String s = buffer.get(0);
	        buffer.clear();
	        turn = player1 == turn ? player2 : player1;
	        notify();
	        
	        return s;
	    }
		
		synchronized void setPlayer(int index) {
			player2 = index;
			gameStarted = true;
		}
	}
	
}

/*
 * void playGame() throws IOException {
			GameInstance gameBuffer = gamesList.get(gameIndex);
			KalahaGame game = new KalahaGame(gameBuffer.stonesNumber);
			
			String command = "";
			
			if (index == gameBuffer.player2) {
				writer.println(gameBuffer.take(index));
				writer.flush();
			}
			
			while(!"GameEnd".equalsIgnoreCase(command)) {
				while(!"TurnEnd".equalsIgnoreCase(command)) {
					command = reader.readLine();
					System.out.println("Player" + index + "move: " + command);
					gameBuffer.put(index, command);
					command = reader.readLine();
				}
					
				writer.println(gameBuffer.take(index));
				writer.println("YourTurn");
				writer.flush();
				command = "";	
			}
			
		}
 */
