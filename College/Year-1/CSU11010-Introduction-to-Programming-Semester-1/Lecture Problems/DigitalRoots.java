import java.util.Scanner;

//Write a program to compute the digital root of a
//user supplied whole number. The digital root is a
//single digits which is computed by adding the
//individual digits of the number together, and
//repeating this until a single digit remains.
//e.g. Number = 12345 … 1+2+3+4+5 = 15
//15 … 1+5 = 6
//The digital root of 12345 is 6.
public class DigitalRoots {

	public static void main(String[] args) {
		System.out.print(
				"Enter an integer to compute its digital root:");
				Scanner input = new Scanner( System.in );
				int number = input.nextInt();
				int currentValue = number;
				while (Math.abs(currentValue) >= 10)
				{
				int remainingNumber = currentValue;
				int sumOfDigits = 0;
				while (remainingNumber != 0)
				{
				sumOfDigits += remainingNumber%10;
				remainingNumber = remainingNumber/10;
				}
				currentValue = sumOfDigits;
				}
				int digitalRoot = currentValue;
				System.out.printf( "The digital root of %d is %d", number,
				digitalRoot );

	}

}
