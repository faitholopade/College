/*Write a program to compute a particular term in
the Fibonacci sequence.
Fibonacci( 1 ) = 0
Fibonacci( 2 ) = 1
Fibonacci( n ) = Fibonacci( n-1 ) +
Fibonacci( n-2 )
where n > 2
*/ 

import java.util.Scanner;

public class Fibonacci {
public static final int FIBONACCI_NUMBER_1 = 0;
public static final int FIBONACCI_NUMBER_2 = 1;
public static void main(String[] args) {
Scanner input = new Scanner( System.in );
System.out.print(
"Enter the index of the required Fibonacci number:");
int index = input.nextInt();
input.close();
int fibonacciNumber = ComputeFibonacciNumber( index );
System.out.println("Fibonacci_number( " + index + " ) is " +
fibonacciNumber );
}
public static int ComputeFibonacciNumber( int index )
{
int result;
switch (index)
{
case 1:
result = FIBONACCI_NUMBER_1;
break;
case 2:
result = FIBONACCI_NUMBER_2;
break;
default:
result = ComputeFibonacciNumber( index-1 ) +
ComputeFibonacciNumber( index-2 );
break;
}
return result;
}
}
