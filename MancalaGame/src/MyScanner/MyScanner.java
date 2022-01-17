package MyScanner;

import java.util.Scanner;

public class MyScanner {
    static Scanner scanner;

    public MyScanner() {
        scanner = new Scanner(System.in);
    }

    public static int readInt(int start, int end, String str) {
        if (scanner == null) scanner = new Scanner(System.in);
        System.out.println(str);
        while (true) {
            int returnValue = scanner.nextInt();
            if (returnValue >= start && returnValue <= end)
                return returnValue;
            System.out.println("Please insert valid input");
        }
    }

    public static int readInt(String s) {
        if (scanner == null) scanner = new Scanner(System.in);
        System.out.println(s);
        int returnValue = scanner.nextInt();
        return returnValue;
    }

    public static boolean readBoolean(String str) {
        if (scanner == null) scanner = new Scanner(System.in);
        System.out.println(str);
        return scanner.nextBoolean();
    }

    public static int readInt(int start, int end) {
        if (scanner == null) scanner = new Scanner(System.in);
        while (true) {
            int returnValue = scanner.nextInt();
            if (returnValue >= start && returnValue <= end)
                return returnValue;
            System.out.println("Please insert valid input");
        }
    }
}
