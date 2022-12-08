// Write	a	program	to	determine	if	a	number	is	odd	or	
//even.

import java.util.Scanner;	
public class OddOrEven	{	
		public static void	main(String[]	args)	{	
				System.out.print("Enter	your	number?	");	
				Scanner	input	=	new	Scanner(	System.in	);	
				int	number	=	input.nextInt();	
				boolean	numberIsOdd	=	((number%2)==1);	
				if	(numberIsOdd)	
				{	
						System.out.println("The	number	"	+	number	+	"	is	odd.");	
				}	
				else
				{	
						System.out.println("The	number	"	+	number	+	"	is	even.");	
				}	
		}
}