/*
 * class Rectangle with a default constructor  and print out the length and breadth of the  rectangle.​
 */
public class Rectangle {
	private static int length;
	private static int breadth;

	Rectangle(){
		length = 10;
		breadth = 10;
		
	}
	
	/*
	 * class Rectangle with a parameterized  constructor and print out the length and breadth  of the rectangle.​
	 */
	
	Rectangle(int len, int bre){
		length = len;
		breadth = bre;	
	}
	public static void main(String[] args) {
		Rectangle r1 = new Rectangle();
		System.out.println("length" + r1.length+ "breadth" +r1.breadth);
		
		Rectangle r2 = new Rectangle(20,30);
		
	}

}
