public class Cylinder extends Shape{
	
	private static String name = "Rectangle";
	private double radius;
	private double height;
	
	public Cylinder (double radius, double height) {
		
		super(name);
		this.radius = radius;
		this.height = height;
		
	}
	
	public double area() {
		
		return 3.14 * (radius * radius) * height;
	}

}
