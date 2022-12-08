/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful and properly formatted variable names and constants?
       Mark out of 5:   5
       Comment: Yes I used easy-to-understand meaningful and properly formatted variable names and constants
    2. Did I indent the code appropriately?
       Mark out of 5:   5
       Comment: Yes I indented the code appropriately
   3. Did I make use of the functions appropriately within main and the other functions?
       Mark out of 5:   5
       Comment: Yes I made use of the functions appropriately within main and the other functions
   4. Have I avoided inappropriate duplication of code?
       Mark out of 5:   5
       Comment: I have avoided inappropriate duplication of code
     Total Mark out of  20 (Add all the previous marks):  20
 */
import java.util.Scanner;

public class StoppingDistance3 {

	public static double COEFFICIENT_OF_FRICTION_FOR_DRY_ROAD = 0.8;
	public static double COEFFICIENT_OF_FRICTION_FOR_WET_ROAD = 0.4;
	public static double COEFFICIENT_OF_FRICTION_FOR_ICY_ROAD_OR_ICY_AND_WET_ROAD = 0.1;
	public static double TWO_HUNDRED_AND_FIFTY = 250;
	public static double VALUE_TO_CONVERT_KM_PER_HOUR_TO_MTRS_PER_SECOND = 3.6;
	public static double CONSTANT_REACTION_TIME_VALUE = 1.0;
	public static double VALUE_FOR_REACTION_TIME_SUBTRACTION = 24;
	public static double VALUE_FOR_REACTION_TIME_MULTIPLICATION = 0.05;

	public static void main(String[] args) {

		Scanner inputScanner = new Scanner (System.in);

		boolean vehicleSpeedError = true;
		double vehicleSpeed = 0.0;
		while (vehicleSpeedError)
		{
			System.out.print("Enter vehicle speed in km/hr> ");
			if (inputScanner.hasNextDouble())
			{
				vehicleSpeed = inputScanner.nextDouble();
				vehicleSpeedError = false;
			}
			else 
			{
				System.out.println("Error: Enter a positive number for the speed of the vehicle.");
				inputScanner.next();
			}

		}

		boolean driverAgeError = true;
		int driverAge = 0;
		while (driverAgeError)
		{
			System.out.print("Enter age in years> ");
			if (inputScanner.hasNextInt())
			{
				driverAge = inputScanner.nextInt();
				driverAgeError = false;
			}
			else 
			{
				System.out.println("Error: Enter a positive whole number for the age of the driver.");
				inputScanner.next();
			}
		}

		boolean roadWetError = true;
		boolean roadIsWet = false;
		while (roadWetError)
		{
			System.out.print("Is the road wet (Yes/No)? ");
			String userResponse = inputScanner.next();
			if (userResponse.equalsIgnoreCase("YES") || userResponse.equalsIgnoreCase("NO"))
			{
				roadWetError = false;

				if (userResponse.equalsIgnoreCase("YES"))
				{
					roadIsWet = true;
				}

			}
			else
			{
				System.out.println("Error: You must enter either Yes or No.");
				inputScanner.next();
			}
		}

		boolean roadIcyError = true;
		boolean roadIsIcy = false;
		while (roadIcyError)
		{
			System.out.print
			("Is the road icy (Yes/No)? ");
			String userResponse = inputScanner.next();
			if (userResponse.equalsIgnoreCase("YES") || userResponse.equalsIgnoreCase("NO"))
			{
				roadIcyError = false;

				if (userResponse.equalsIgnoreCase("YES"))
				{
					roadIsIcy = true;
				}

			}
			else
			{
				System.out.println("Error: You must enter either Yes or No.");
				inputScanner.next();
			}
		}

		System.out.print("As the road is ");

		int countWordsAdded=0;
		if (roadIsIcy) 
		{
			System.out.print("icy");
			countWordsAdded++;
		}
		if (roadIsWet) 
		{
			if (countWordsAdded!=0)
			{	
																																																																																									
				System.out.print(" and ");
			}	System.out.print("wet");
		}
		if (!roadIsWet) 
		{
			if (countWordsAdded!=0)
			{
				System.out.print(" and ");
			}	System.out.print("dry");
		}

		System.out.println(", the coefficient of friction is "+computeCoefficientOfFriction(roadIsIcy,roadIsWet));

		System.out.printf(
				("At a speed of " + vehicleSpeed + "km/hr, a driver who is "+ driverAge + 
						" years old, the reaction distance is " + Math.round(computeReactionDistance(vehicleSpeed,driverAge))
						+ "m, the braking distance is " + Math.round(computeBrakingDistance(vehicleSpeed,roadIsIcy,roadIsWet)) 
						+ "m, and so the stopping distance is " + computeStoppingDistance(vehicleSpeed,driverAge,roadIsIcy,roadIsWet) 
						+ "m"));
	}


	public static double computeCoefficientOfFriction(boolean roadIsIcy, boolean roadIsWet)
	{
		double coefficientOfFriction = 0.0;

		if (!roadIsIcy && !roadIsWet)
		{
			coefficientOfFriction = COEFFICIENT_OF_FRICTION_FOR_DRY_ROAD;
		}
		else if (!roadIsIcy && roadIsWet)
		{
			coefficientOfFriction = COEFFICIENT_OF_FRICTION_FOR_WET_ROAD ;
		}
		else
		{
			coefficientOfFriction = COEFFICIENT_OF_FRICTION_FOR_ICY_ROAD_OR_ICY_AND_WET_ROAD;
		}
		return coefficientOfFriction;
	}

	public static double computeBrakingDistance(double vehicleSpeed, boolean roadIsIcy, boolean roadIsWet)
	{
		double brakingDistance = 0.0;
		brakingDistance = ((vehicleSpeed * vehicleSpeed ) / (TWO_HUNDRED_AND_FIFTY * 
				computeCoefficientOfFriction(roadIsIcy,roadIsWet)));
		return brakingDistance;
	}
	public static double computeReactionDistance(double vehicleSpeed, int driverAge)
	{
		double reactionDistance = 0.0;
		double reactionTimeInSeconds = 0.0;
		double speedInMetresPerSecond = 0.0;

		reactionTimeInSeconds = (CONSTANT_REACTION_TIME_VALUE + Math.abs(driverAge - VALUE_FOR_REACTION_TIME_SUBTRACTION)
		* VALUE_FOR_REACTION_TIME_MULTIPLICATION);

		speedInMetresPerSecond = vehicleSpeed / VALUE_TO_CONVERT_KM_PER_HOUR_TO_MTRS_PER_SECOND;

		reactionDistance = ((speedInMetresPerSecond) * (reactionTimeInSeconds));

		return reactionDistance;
	}
	public static int computeStoppingDistance(double vehicleSpeed, int driverAge, boolean roadIsIcy, boolean roadIsWet)
	{
		int stoppingDistance = 0;
		stoppingDistance = ((int)(Math.round(computeReactionDistance(vehicleSpeed, driverAge))) + 
				(int)(Math.round(computeBrakingDistance(vehicleSpeed,roadIsIcy,roadIsWet))));
		return stoppingDistance;
	}


}


