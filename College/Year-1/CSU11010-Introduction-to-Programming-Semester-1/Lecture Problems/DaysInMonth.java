
public class DaysInMonth {

	private static final double UNKNOWN = 0;
	private static final double DAYS_IN_FEBRUARY_IN_LEAP_YEAR = (Double) null;
	private static final double DAYS_IN_FEBRUARY_NORMALLY = (Double) null;
	private static final double DAYS_IN_APRIL_JUNE_SEPT_NOV = 0;
	private static final double DAYS_IN_MOST_MONTHS = 0;

	public static void main(String[] args) {
		int daysInMonth = (int) UNKNOWN;
		int month = 0;
		switch (month)
		{
		case 2:
			int year = 0;
			boolean isLeapYear = (((year%4 == 0) && (year%100 != 0)) ||
		(year%400 == 0));
		daysInMonth = (int) (isLeapYear ? DAYS_IN_FEBRUARY_IN_LEAP_YEAR :
		DAYS_IN_FEBRUARY_NORMALLY);
		break;
		case 4:
		case 6:
		case 9:
		case 11:
		daysInMonth = (int) DAYS_IN_APRIL_JUNE_SEPT_NOV;
		break;
		default:
		daysInMonth = (int) DAYS_IN_MOST_MONTHS;
		break;
		}
	}

}
