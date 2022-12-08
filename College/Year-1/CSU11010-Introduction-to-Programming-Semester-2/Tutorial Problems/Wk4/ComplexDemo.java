public class ComplexDemo
{
	public static void main(String[] args)
	{
		Complex num1 = new Complex(4, 7);
		Complex num2 = new Complex(1, -3);

		Complex num3 = Complex.add(num1, num2);
		System.out.println(num3.toString());

		Complex num4 = Complex.multiply(num1, num2);
		System.out.println(num4.toString());

		Complex num5 = Complex.divide(num1, num2);
		System.out.println(num5.toString());
	}
}
