import java.util.Scanner;

//Write a program to calculate factorial of some
//number (i.e. number!).
//For example 4! = 1*2*3*4 = 24
public class Factorial {
public static void main(String[] args) {
System.out.print("Enter a positive whole number: ");
Scanner scanner = new Scanner( System.in );
int number = scanner.nextInt();
int factorial = 1;
for (int index=2; (index<=number);
index = index + 1)
{
factorial = factorial * index;
}
System.out.println("The factorial of " +
number + " is " + factorial );
}
}
