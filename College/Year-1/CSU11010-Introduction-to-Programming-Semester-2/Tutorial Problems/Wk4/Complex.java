/*
 * A complex number is of the form x + i y where x and y are real numbers and i is the imaginary unit equal to the square root of -1. Write a Java class Complex for dealing with complex number. Your class must have the following features:
Instance Variables:
x for the real part of type double
y for imaginary part of type double
Constructor:
public Complex (): A default constructor, it should initialize the number to (0, 0)
public Complex (double realPart, double imaginaryPart): A constructor with parameters, it creates the complex object by setting the two fields to the passed values.
Instance Methods:
public static Complex add (Complex a, Complex b): This method will find the sum of the current complex number and the passed complex number.  The methods returns a new Complex number which is the sum of the two – (x1+y1 i)+(x2+y2 i)=(x1+x2)+i (y1+y2)
public static Complex multiply (Complex a, Complex b): This method will find the product of the current complex number and the passed complex number.  The methods returns a new Complex number which is the product of the two –
(x1+y1 i)  (x2+y2 i)=(x1 x2 -y1 y2)+i (x1 y2+y1 x2)
public static Complex divide (Complex a, Complex b): The methods returns a new complex number -
   (x1+y1 i)/(x2+y2 i) = ((x1 x2 + y1 y2)  + i (y1 x2 - x1 y2))/(x22+y22)
public String toString(): Allows the complex number to be easily printed out to the screen
Write a separate class ComplexDemo with a main() method and test the Complex class methods
 */
/**
 * 
 */

/**
 * @author faith
 *
 */
public class Complex
{
	private double x;
	private double y;

	Complex()
	{
		x = 0;
		y = 0;
	}

	Complex(double realPart, double imaginaryPart)
	{
		x = realPart;
		y = imaginaryPart;
	}

	public static Complex add(Complex a, Complex b)
	{
		return new Complex(a.x+b.x, a.y+b.y);
	}

	public static Complex multiply(Complex a, Complex b)
	{
		return new Complex((a.x*b.x - a.y*b.y), (a.x*b.y + a.y*b.x));
	}

	public static Complex divide(Complex a, Complex b)
	{
		return new Complex(((a.x*b.x + a.y*b.y)/(Math.pow(b.x, 2) + Math.pow(b.y, 2))),
				(((a.y*b.x) - (a.x*b.y))/(Math.pow(b.x, 2) + Math.pow(b.y, 2))));
	}

	public String toString()
	{
		return "{" + x + "," + y + "}";
	}
}