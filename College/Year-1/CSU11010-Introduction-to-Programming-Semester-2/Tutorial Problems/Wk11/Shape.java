/*
 * Implement the above inheritance hierarchy. Our program uses many kinds of shapes, such as triangle, rectangle etc. Design an interface Shape, which defines the public behaviors of all the shapes. We would like all the shapes to have methods getArea() and getPerimeter(), which return the area and perimeter of that particular shape respectively.
Derive subclasses, such as Triangle and Rectangle from the superclass Shape. The Triangle subclass implemens the getArea() and getPerimeter() methods inherited from the superclass. Assume the Triangle in this case is an equilateral triangle i.e., all sides are of the triangle are equal. Note that the Rectangle class only implements the getArea() method.
Derive a subclass Square from Rectangle which implements the Shape’s getPerimeter() method.
Use upcasting to assign class Shape a subclass reference to the Triangle and Square in order to call the appropriate getArea() and getPerimeter() methods.
 */
public interface Shape {
	public double getArea();
    public double getPerimeter();
}
