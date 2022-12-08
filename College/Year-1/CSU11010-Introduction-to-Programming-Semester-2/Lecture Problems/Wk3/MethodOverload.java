
public class MethodOverload {

	public static int square(int num)
	{
		System.out.println("In Int Method");
		return num*num;
	}

	public static double square(double num)
	{
		System.out.println("In Double Method");
		return num*num;
	}
	public static void main(String[] args) {
		System.out.println("Integer Square: " +square(5));
		System.out.print("Double Square: " +square(6.25));


	}

}
