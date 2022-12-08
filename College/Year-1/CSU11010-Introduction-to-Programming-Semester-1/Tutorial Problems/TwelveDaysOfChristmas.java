public class TwelveDaysOfChristmas {
	
	final static String[] NTH_DAY_OF_CHRISTMAS = {"first", "second","third","fourth","fifth","sixth",
											      "seventh","eighth","ninth","tenth","eleventh","twelfth"};
	
	public static void main(String[] args) 
	{
		boolean finished = false;
		int verse = 1;
		while (!finished)
		{
			System.out.print("On the "+ NTH_DAY_OF_CHRISTMAS [verse-1]+" day of Christmas \nmy true love sent to me:");
			switch (verse)
			{
	
			case 12:
					System.out.print("\n12 Drummers Drumming,");
					finished = true;
			case 11:
					System.out.print("\n11 Pipers Piping,");
			case 10:
					System.out.print("\n10 Lords a Leaping, ");
			case 9:
					System.out.print("\n9 Ladies Dancing,");
			case 8:
					System.out.print("\n8 Maids a Milking,");
			case 7:
					System.out.print("\n7 Swans a Swimming,");
			case 6:
					System.out.print("\n6 Geese a Laying,");
			case 5:
					System.out.print("\n5 Golden Rings,");
			case 4:
					System.out.print("\n4 Calling Birds,");
			case 3:
					System.out.print("\n3 French Hens,");
			case 2:
					System.out.print("\n2 Turtle Doves\nand ");
			case 1:
					if (verse==1) System.out.print("\n");
					System.out.print("a Partridge in a Pear Tree.\n");	
			default:
					verse++;
					break;
			}	
		}
	}
}