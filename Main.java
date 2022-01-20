import java.util.Scanner;

public class Main {
    public static void main(String[] args){

        Scanner scanner = new Scanner(System.in);

        System.out.println("1.User vs User");
        System.out.println("2.User vs Computer");
        System.out.println("3.Computer vs Computer");
        System.out.print("Choose number of gameplay option:");
        Server server = new Server(new Board());
        int number = scanner.nextInt();
        switch (number) {
            case 1: {
                server.run(new User(), new User());
                break;
            }
            case 2: {
                server.run(new User(), new Computer());
            }
            case 3: {
                server.run(new Computer(), new Computer());
            }
        }
    }
}

