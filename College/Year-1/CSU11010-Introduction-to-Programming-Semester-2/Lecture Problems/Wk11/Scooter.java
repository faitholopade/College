// Since the object reference ‘b’ refers to the subclass object
// and the subclass method run() overrides the parent class method,
// the subclass method is invoked at runtime
public class Scooter extends Bike{

	@Override
	void run()
	{
		System.out.println("Running safely at 60km");
	}

	public static void main(String[] args) {
		Bike b = new Scooter(); //upcasting 
		b.run();

	}

}
