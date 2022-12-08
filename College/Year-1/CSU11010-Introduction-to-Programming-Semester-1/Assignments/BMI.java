import java.util.Scanner;

public class BMI {
	// weight(kg)/ height(m)^2

	public static void main(String[] args) {
		
		Scanner input = new Scanner (System.in);
		
		
		
		System.out.println ("What is your weight in kg?");
		double weight = input.nextDouble();
		
		System.out.println ("What is your height in metres?");
		double height = input.nextDouble();
		
		System.out.println(weight);
		System.out.println(height);
		
	
		double BMI = weight / Math.pow(height, 2);
		System.out.println ("Your BMI is: " + BMI);
		
		
		
		
		 

	}

}
