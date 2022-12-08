final int A_FORWARD = 0;
final int A_BACKWARD = 1;
final int A_DOWN = 2;


class Alien {

  int x;
  int y;
  int direction = 1;
  int appearance;
  PImage invader;
  int alienHeight;
  int count = 0;
  PImage exploded;

  /* Constructor is passed the x and y position where the alien is to
   be created, plus the image to be used to draw the alien */

  Alien(int xpos, int ypos, PImage alienImage) {

    x = xpos;
    y = ypos;
    invader = alienImage;
    alienHeight = invader.height;


    /* set up the new alien object */
  }

  void move() {
    println("width:", invader.width);

    if (x< 0 || (x+invader.width >= width))
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

  void draw() {
    image(invader, x, y, 30, 25);
  }
}
