
public class Sphere extends Shape{
	private static String name = "Sphere";
	private double radius;
	
	public Sphere(int radius) {
		super(name);
		this.radius = radius;
	}

	public double area() {
		
		return 4 * 3.14 * radius;
	}

}
