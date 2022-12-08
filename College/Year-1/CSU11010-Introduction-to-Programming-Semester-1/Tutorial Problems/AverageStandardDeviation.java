import java.util.Scanner;

public class AverageStandardDeviation {

	public static void main(String[] args) {
		
	
		
		System.out.println ("Enter three numbers");
		 
		Scanner inputScanner = new Scanner (System.in);
		
		double number1 = inputScanner.nextDouble ();
		double number2 = inputScanner.nextDouble ();
		double number3 = inputScanner.nextDouble ();
		
		inputScanner.close ();
	
		double average = (number1 + number2 + number3) / 3.0;
		
		double SD = (number1-average)*(number1-average) +
		(number2-average)*(number2-average) +
		(number3-average)*(number3-average) / 3.0;
		
		double standardDeviation = Math.sqrt( SD );
		
		System.out.println ("For numbers " + number1 +
	    ", " + number2 + ", " + number3 + ", the average is " +
		average + " and the standard deviation is " + standardDeviation );
		
		
		
		
		
		
		

	}

}
