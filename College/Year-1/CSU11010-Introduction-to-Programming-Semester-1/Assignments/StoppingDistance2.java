/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable and constant names?
       Mark out of 15: 15
       Comment: Yes I used easy-to-understand meaningful variable and constant names
   2. Did I format the variable and constant names properly?
       Mark out of 5:  5
       Comment: Yes I formated	the variable and constant names properly
   3. Did I indent the code appropriately?
       Mark out of 10:  10
       Comment: Yes I indented	the code appropriately
      Total Mark out of  30 (Add all the previous marks):  30
*/
import java.util.Scanner;

public class StoppingDistance2 {

	public static void main(String[] args) {
		
		Scanner inputScanner = new Scanner (System.in);
		
	
		System.out.print("Enter vehicle speed in km/hr> ");
			double vehicleSpeed = inputScanner.nextDouble();
		  
		System.out.print("Enter age in years> ");
			double driverAge = inputScanner.nextInt();
		  
			double reactionTime;
		  
		 if (driverAge < 50){ 
			   reactionTime = 2.0;
		 }
		 else if (driverAge < 60) {
			  reactionTime = 2.5;
		 }
		 else if (driverAge >= 60) {
			  reactionTime = 3.0;
		 }
		 else {
			 reactionTime = 0.0;
		 }
		 
		System.out.print("Is the road wet (Yes/No)? ");
		    String roadWetAnswer = inputScanner.next();
		boolean roadWet = roadWetAnswer.equalsIgnoreCase("Yes");
		
			double coefficientFriction;
		
		
		if (! roadWet) {
			coefficientFriction = 0.8;
		}
		else {
			coefficientFriction = 0.4;
			 
		}
		
		
			final double CONVERSION_KMPERHR_TO_MTRSPERSEC = 3.6;
			final double TWO_HUNDRED_AND_FIFTY = 250;
		
			double reactionDistance = ((vehicleSpeed / CONVERSION_KMPERHR_TO_MTRSPERSEC) * (reactionTime));
			double rD = Math.round(reactionDistance);
		
		
			double brakingDistance = ((vehicleSpeed * vehicleSpeed) / (TWO_HUNDRED_AND_FIFTY * coefficientFriction));
		
		
			double stoppingDistance = reactionDistance + brakingDistance;
			double sD = Math.round(stoppingDistance);
		
		
	      
		 System.out.print("As the road is " + (roadWet?"wet,": "dry,") + " the coefficient of friction "
			+ "is " + coefficientFriction +  " \n");
	
	
		 System.out.printf(
				("As the driver is %.0f" + " years old, his/her reaction time is "
			+ reactionTime + "s" + 	" \n") , driverAge);
	
			
		 System.out.printf(
				("At a speed of %.1f"  + "km/hr," +
		    " the reaction distance is %.0f" + "m," + " the braking "
		   	+ "distance is %.0f" + "m," +  " and so the stopping"
			+ " distance is %.0f"  + "m") ,vehicleSpeed,rD,brakingDistance,sD);		
		
		}
		

	}


