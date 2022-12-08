// Faith: added class to convert string date to seconds offset from timeZero which is 1950 January 1st at 00:00 29/03
static class Time {

  public static final int timeZero = 1950;

  // Convert the date string found in file to a time in seconds
  static long parseDate(String date) {
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy, MMMM, d", Locale.ENGLISH);
    LocalDate newDate = LocalDate.parse(date, formatter);
    System.out.println(date);
    String[] dateArray = date.split("-");
    long year = Long.valueOf(dateArray[0]);
    long month = Long.valueOf(dateArray[1]);
    long day = Long.valueOf(dateArray[2]);

    long dayInSeconds = 24 * 60 * 60 * day;
    long monthInSeconds = getSecondsInXMonths(month);
    long yearInSeconds = getSecondsInXYears(year);

    return dayInSeconds + monthInSeconds + yearInSeconds;
  }

  // Returns the number of seconds in a given number of months
  private static long getSecondsInXMonths(long month) {
    long result = 0;
    for (int m = 1; m <= month; m++) {
      if (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) {
        result += 60 * 60 * 24 * 31;
      } else if (m == 2) {
        result += 60 * 60 * 24 * 28;
      } else {
        result += 60 * 60 * 24 * 30;
      }
    }
    return result;
  }

  // Returns the number of seconds in a given number of years
  private static long getSecondsInXYears(long year) {
    long result = 0;
    // Making time 0 be 1950
    for (int y = timeZero; y <= year; y++) {
      result += getSecondsInXMonths(12);
      if (isLeapYear(y)) {
        // Add one extra day worth of seconds for a leap year
        result += 60 * 60 * 24;
      }
    }
    return result;
  }

  private static boolean isLeapYear(long year) {
    if (year % 4 == 0) {
      if ( year % 100 == 0) {
        if ( year % 400 == 0)
          return true;
        else {
          return false;
        }
      } else {
        return true;
      }
    } else {
      return false;
    }
  }
}
