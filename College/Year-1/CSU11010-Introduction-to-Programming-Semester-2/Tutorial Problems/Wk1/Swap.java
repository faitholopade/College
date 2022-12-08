//Write a Java class Swap which swaps around two numbers to be inputted by the user without using a third variab
import java.util.Scanner;

public class Swap {

	public static void main(String[] args) {
		
		int number1 = 0, number2 = 0;
		
		System.out.print("Enter a whole number ");
		Scanner inputScanner = new Scanner(System.in);
		if(inputScanner.hasNextInt()) {
			 number1 = inputScanner.nextInt();
		}
		System.out.print("Enter another whole number ");
		if(inputScanner.hasNextInt()) {
			number2 = inputScanner.nextInt();
		}
		System.out.println("number 1: " + number1 + " number 2: " + number2);
		
		System.out.println("Swapping");
		
		number1 = number1 + number2;
		number2 = number1 - number2;
		number1 = number1 - number2;
		
		System.out.println("After swapping: "+"number 1: " + number1 + " number 2: " + number2);
		
		
	}

}
