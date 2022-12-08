
public class Rectangle extends Shape{
	private static String name = "Rectangle";
	public double length;
	private double width;
	
	
	public Rectangle(double length, double width){
		
		super(name);
		this.length = length;
		this.width = width;
		
	}
	
	public double area() {
		
		return length * width;
	}
}
