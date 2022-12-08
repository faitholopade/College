//Write	a	program	to	determine	if	someone	is	
//allowed	to	drive	and	if	they	are	allowed	to	drink	
//alcohol.

import java.util.Scanner;	
public class	AgeLimits	{	
	final static int	MINIMUM_LEGAL_DRIVING_AGE	=	17;	
	final static int	MINIMUM_LEGAL_DRINKING_AGE	=	18;	
	public static void	main(String[]	args)	{	
		System.out.print("Enter	your	age?	");	
		Scanner	input	=	new	Scanner(	System.in	);	
		int	age	=	input.nextInt();	
		if	(age	>=	MINIMUM_LEGAL_DRIVING_AGE	)	
		{	
			System.out.println(	
					"You	may	now	legally	drive	(once	you	get	a	licence).");	
		}	
		if	(age	>=	MINIMUM_LEGAL_DRINKING_AGE	)	
		{	
			System.out.println(	
					"You	may	now	legally	drink,	but	be	responsible!");	
		}	
	}
}	