
		
		//Write a program which takes in hours worked,
		//minutes worked and an hourly pay rate, and
		//then determines the amount of pay due...
import java.util.Scanner;
public class Wages {
public static final double MINUTES_PER_HOUR = 60.0;
public static void main(String[] args) {
System.out.print("Enter hourly rate (Euros.Cents):");
Scanner input = new Scanner( System.in );
double hourlyRate = input.nextDouble();
System.out.print("Enter time worked (hours:minutes):");
input.useDelimiter("(\\p{javaWhitespace}|\\:)+");
int hoursWorked = input.nextInt();
int minutesWorked = input.nextInt();
input.close();
double totalHoursWorked = ((double) hoursWorked) +
(((double) minutesWorked)/((double) MINUTES_PER_HOUR));
double totalWages = hourlyRate * totalHoursWorked;
System.out.println("Amount earned: €" + totalWages);
}
}
