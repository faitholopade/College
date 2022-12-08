import java.util.Scanner;

/*
 * Write a program to determine is an entered date (day/month/year) is valid.
 * As part of the program write and use the following routines:
 *  - validDate() which takes a year, month and day and returns whether or not
 *  the date is valid
 *  - daysInMonth() which takes a month (between 1 and 12) and year, and returns
 *    the number of days in the month.
 *  - isLeapYear() which takes a year and return whether or not it is a leap year.
 */
public class ValidDate {

	public static final int DAYS_IN_APRIL_JUNE_SEPT_NOV = 30;
	public static final int DAYS_IN_FEBRUARY_NORMALLY = 28;
	public static final int DAYS_IN_FEBRUARY_IN_LEAP_YEAR = 29;
	public static final int DAYS_IN_MOST_MONTHS = 31;
	public static final int NUMBER_OF_MONTHS = 12;

	public static void main(String[] args) {

		try
		{
			System.out.print("Enter date (day/month/year): ");
			Scanner input = new Scanner( System.in );
			input.useDelimiter("(\\p{javaWhitespace}|\\/)+");
			int day = input.nextInt();
			int month = input.nextInt();
			int year = input.nextInt();
			input.close();

			if (validDate( day, month, year))
			{
				System.out.println("" + day + "/" + month + "/" + year + " is a valid date.");
			}
			else
			{
				System.out.println("" + day + "/" + month + "/" + year + " is not a valid date.");
			}
		}
		catch (java.util.NoSuchElementException exception)
		{
			System.out.println("Error: No number entered.");
		}
	}
	
	public static boolean validDate( int day, int month, int year)
	{
		return ((year >= 0) && (month >= 1) && (month <= NUMBER_OF_MONTHS) &&
				(day >= 1) && (day <= daysInMonth( month, year )));
	}
	
	public static int daysInMonth( int month, int year )
	{
		int numberOfDaysInMonth = DAYS_IN_MOST_MONTHS;
		switch (month)
		{
		case 2:
			numberOfDaysInMonth = isLeapYear( year ) ? DAYS_IN_FEBRUARY_IN_LEAP_YEAR :
				                                       DAYS_IN_FEBRUARY_NORMALLY;
			break;
		case 4:
		case 6:
		case 9:
		case 11:
			numberOfDaysInMonth = DAYS_IN_APRIL_JUNE_SEPT_NOV;
			break;
		default:
			numberOfDaysInMonth = DAYS_IN_MOST_MONTHS;
			break;
		}
		return numberOfDaysInMonth;
	}
	
	public static boolean isLeapYear( int year )
	{
		return (((year%4 == 0) && (year%100 != 0)) || (year%400 == 0));
	}
}