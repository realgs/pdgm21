package kalahaClient;

import kalahaGame.*;
import java.util.Scanner;
import java.io.*;
import java.net.*;

public class KalahaClient {
	
	Socket socket;
	InputStreamReader strReader;
	BufferedReader reader;
	PrintWriter writer;
	Scanner scanner;
	KalahaGame game;
	int player;
	
	public static void main(String[] args) {
		new KalahaClient().menu();
	}
	
	private void printMenu() {
		System.out.println("------------------");
		System.out.println("|     KALAHA     |");
		System.out.println("------------------");
		System.out.println("");
		System.out.println("------------------");
		System.out.println("|1. Singleplayer |");
		System.out.println("------------------");
		System.out.println("");
		System.out.println("------------------");
		System.out.println("|2. Multiplayer  |");
		System.out.println("------------------");
		System.out.println("");
		System.out.println("------------------");
		System.out.println("|3. Simulation   |");
		System.out.println("------------------");
		System.out.println("");
		System.out.println("------------------");
		System.out.println("|4. Exit         |");
		System.out.println("------------------");
		System.out.println("");
	}
	
	private void menu() {
		int choice = 0;
		scanner = new Scanner(System.in);	
		
		while (choice != 4) {
			printMenu();
			
			System.out.print("What option do you want to use? ");
			choice = scanner.nextInt();
			
			switch (choice) {
			case 1:
				singleplayer();
				break;
				
			case 2:
				multiplayer();
				break;
				
			case 3:
				simulation();
				break;
				
			case 4:
				System.out.println("The end.");
				break;
				
			default:
				System.out.println("You pick the wrong number, fool!");
			}
			
			System.out.println();
			System.out.println();
			System.out.println();
		}
		
		if (socket != null && reader != null && writer != null) {
			try {
				socket.close();
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
	}
	
	private void singleplayer() {
		player = 0;
		System.out.print("How many stones in a hole? ");
		
		game = new KalahaGame(scanner.nextInt());
		KalahaAI opponent = new KalahaAI(1);
		int whatPlayer = 0;
		
		while (!game.ifEndOfGame()) {
			int correctMove = -1;
		
			if (whatPlayer % 2 == 0) {
				while (correctMove == -1 || correctMove == 1) {
					System.out.println(game.getBoardToPrint());
					System.out.println("Player " + ((whatPlayer % 2) + 1));
					System.out.print("Hole number? ");
					
					correctMove = game.makeMove(scanner.nextInt());
					
					if (correctMove == -1)
						System.out.println("Wrong move!");
					else if (correctMove == 1)
						System.out.println("You have another turn!");
				}
			}
			else {
				while (correctMove == -1 || correctMove == 1)
					correctMove = game.makeMove(opponent.move(game.getBoardCopy()));	
			}	
				
			whatPlayer++;
		}
		
		System.out.println(game.getBoardToPrint());
		System.out.println(game.getResults());
	}
	
	private void multiplayer() {
		try {
			connectWithServer();
		
			int choice = 0;
		
			while (choice < 1 || choice > 3) {
				System.out.println("1. Create game.");
				System.out.println("2. Join game.");
				System.out.println("3. Quit multiplayer.");
				System.out.print("What option do you want to use? ");
				choice = scanner.nextInt();
				
				
					switch (choice) {
					case 1:
						createMultiGame(); 
						break;
						
					case 2:
						joinMultiGame();
						break;
						
					case 3:
						quitMulti();
						break;
						
					default:
						System.out.println("You pick the wrong number, fool!");
					}
				}
			}
			catch (IOException e) {
				System.out.println("No connection!");
			}
	}
	
	private void simulation() {
		System.out.print("How many stones in a hole? ");
		
		game = new KalahaGame(scanner.nextInt());
		KalahaAI opponent1 = new KalahaAI(0);
		KalahaAI opponent2 = new KalahaAI(1);
		int whatPlayer = 0;
		
		while (!game.ifEndOfGame()) {
			int correctMove = 1;
		
			if (whatPlayer % 2 == 0) {
				while (correctMove == 1) {
					System.out.println(game.getBoardToPrint());
					System.out.println("Player " + ((whatPlayer % 2) + 1));
					correctMove = game.makeMove(opponent1.move(game.getBoardCopy()));	
				}
			}
			else {
				while (correctMove == 1) {
					System.out.println(game.getBoardToPrint());
					System.out.println("Player " + ((whatPlayer % 2) + 1));
					correctMove = game.makeMove(opponent2.move(game.getBoardCopy()));	
				}
					
			}	
				
			whatPlayer++;
		}
		
		System.out.println(game.getBoardToPrint());
		System.out.println(game.getResults());
	}
	
	private void connectWithServer() throws IOException {
		try {
			socket = new Socket("127.0.0.1", 5000);
			strReader = new InputStreamReader(socket.getInputStream());
			reader = new BufferedReader(strReader);
			writer = new PrintWriter(socket.getOutputStream());
		}
		catch(IOException ex) {
			
		}
	}
	
	private void createMultiGame() throws IOException {
		player = 0;
		
		writer.println("createGame");
		writer.flush();
		
		System.out.println(reader.readLine());
		
		int stones = scanner.nextInt();
		writer.println(stones);
		writer.flush();
		
		String str = "";
		
		while (!(str = reader.readLine()).equalsIgnoreCase("Connected with another player! Game starts!")) {}
		
		System.out.println(str);
		
		playMultiGame(stones);
	}
	
	private void joinMultiGame() throws IOException {
		player = 1;
		
		writer.println("joinGame");
		writer.flush();
		
		System.out.println(reader.readLine());
		System.out.println(reader.readLine());
		
		writer.println(scanner.next());
		writer.flush();
		
		String response = reader.readLine();
		
		if (response.equalsIgnoreCase("Wrong number!"))
			System.out.println(response);
		else
			playMultiGame(Integer.parseInt(response));
	}
	
	private void quitMulti() throws IOException {
		writer.println("stop");
		writer.flush();
	}
	
	private void playMultiGame(int stones) throws IOException {
		game = new KalahaGame(stones);
		String command = "";
		
		System.out.println("Game started!");
		
		if (player == 1) {
			while (!(command = reader.readLine()).equalsIgnoreCase("YourTurn")) 
			game.makeMove(Integer.parseInt(command));
		}
		
		while (!game.ifEndOfGame()) {
			while (!"EndTurn".equalsIgnoreCase(command)) {
				System.out.println(game.getBoardToPrint());
				System.out.println("Player " + (player + 1));

				command = reader.readLine();
				System.out.println(command);
				
				int move = scanner.nextInt();
				writer.println(move);
				writer.flush();
				
				command = reader.readLine();
				
				if (!"Wrong move!".equalsIgnoreCase(command))
					game.makeMove(move);
				
				if (!"EndTurn".equalsIgnoreCase(command)) 
					System.out.println(command);
			}
			
			while (!(command = reader.readLine()).equalsIgnoreCase("YourTurn")) 
				game.makeMove(Integer.parseInt(command));
			
		}	
		
		System.out.println(game.getBoardToPrint());
		System.out.println(game.getResults());	
	}
}

