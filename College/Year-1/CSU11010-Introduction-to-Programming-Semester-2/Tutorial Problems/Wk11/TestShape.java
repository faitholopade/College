
public class TestShape {

	public static void main(String[] args) {
		Shape s1 = new Square("red", 5);  // Upcast
        System.out.println("Area of Square is " + s1.getArea());
        System.out.println("Perimeter of Square  is " + s1.getPerimeter());

	Shape s2 = new Triangle("blue", 4, 5);  // Upcast
        System.out.println("Area of Triangle is " + s2.getArea());
        System.out.println("Perimeter of Triangle  is " + s2.getPerimeter());

	}

}
