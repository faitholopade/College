
public abstract class Rectangle implements Shape {
	private String color;
    private int length;
    private int width;
   
    public Rectangle(String color, int length, int width)
    {
	this.color = color;
        this.length = length;
        this.width = width;
    }
   
    public abstract double getPerimeter();

    @Override
    public double getArea()
    {
        return length*width;
    }
}
