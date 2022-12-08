//Write	a	program	to	determine	if	someone	is	an	
//adult	and	if	so	tell	them	(some	of)	their	rights
import java.util.Scanner;	
public class IsAdult	{	
		public static void	main(String[]args)	{	
				System.out.print("Enter	your age? ");	
				Scanner	input = new	Scanner(	System.in	);	
				int	number = input.nextInt();	
				boolean	isAdult	= (number>= 18);	
				if (isAdult)	
				{	
					System.out.println("You	may	now	legally	drink!");	
				}	
		}
}	