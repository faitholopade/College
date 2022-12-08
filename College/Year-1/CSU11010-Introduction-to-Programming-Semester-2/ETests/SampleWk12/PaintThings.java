
public class PaintThings {

	public static void main(String[] args) {
		Paint paint = new Paint(1);
		Rectangle deck = new Rectangle(20, 35); 
		Sphere bigBall = new Sphere(15);
		Cylinder tank = new Cylinder(10, 30);
		
		paint.amount(deck);
		paint.amount(bigBall);
		paint.amount(tank);

	}

}
