
public class Triangle implements Shape{
	String color;
    private int base;
    private int height;
   
    public Triangle(String color, int base, int height)
    {
	this.color = color;
        this.base = base;
        this.height = height;
    }
   
    public double getArea()
    {
        return 0.5*base*height;
    }

    public double getPerimeter()
    {
        return base*3;
    }
}
