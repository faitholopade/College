
public class Paint {
private double coverage;
	
	public Paint (double coverage) {
		this.coverage = coverage; 
	}
	
	
	
	public double amount(Shape shape) {
		
		if("Rectangle".equalsIgnoreCase(shape.toString())) {
			System.out.println("Computing amount for " + shape + " of length " + " and width ");
			System.out.println("Amount required is " + shape.area() / coverage);
			return shape.area() / coverage;
		}
		
		if("Sphere".equals(shape.toString())) {
			System.out.println("Computing amount for " + shape + " of radius ");
			System.out.println("Amount required is " + shape.area() / coverage);
			return shape.area() / coverage;
		}
		
		if("Cylinder".equals(shape.toString())) {
			System.out.println("Computing amount for " + shape + " of radius " + " and height ");
			System.out.println("Amount required is " + shape.area() / coverage);
			return shape.area() / coverage;
		}
		
		return -1;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
