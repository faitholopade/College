
public class Square extends Rectangle{
	private int length;
	   
    public Square(String color, int length)
    {
       super(color, length, length);
       this.length = length;
    }
   
    @Override
    public double getPerimeter()
    {
       return 4*length;
    }
}
