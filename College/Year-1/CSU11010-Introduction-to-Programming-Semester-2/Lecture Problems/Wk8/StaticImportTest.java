/*
 * Create a class that calculates the Square Root, Ceiling  of a given value and the value of PI to a specified number  of decimal places.​

​

Make use of the static java.lang.Math library and call the  in-built functions via their unqualified names.​
 */
import static java.lang.Math.*;

public class StaticImportTest
{ 
	public static void main(String[] args)
	{
		System.out.printf("sqrt(900.0) = %.1f%n", sqrt(900.0));
		System.out.printf("ceil(-9.8) = %.1f%n", ceil(-9.8));
		System.out.printf("PI = %f%n", PI);
	}
}