class Alien
{
  int x;
  int y;
  PImage invader;
  int count = 0;
  int alienHeight;
  int direction = 1;

  Alien (int xpos, int ypos, PImage alienImage) {
    // constructor
    x = xpos;
    y = ypos;
    invader = alienImage;
    alienHeight = invader.height;
  }



  void draw() {
    image(invader, x, y, 30, 25);
  }

  void move() {
    //if (x<0 || (x+invader.width >= 499)) {
    //  y++;
    //} else
    //{
    //  x++;
    //}
    if (x< 0 || (x+invader.width >= 499))
    {
      if (count< alienHeight) // ensuring invader does not move down past its own height
      {

        y++;
        count++;
        println("a", count);
        println("b", alienHeight);
      } else
      {
        direction*= -1; // inverting direction
        x+= direction;
        count = 0;
      }
    } else
    {
      x+= direction;
    }
  }
}
