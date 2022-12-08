class Alien {
  int x;
  int y;
  int direction  = 1;
  PImage explode = loadImage("exploding.gif");
  int count = 0; // height of alien
  int alienHeight;
  int exploded = 0;
  boolean choose; // picking which alien to be blown up
  float dy;
  float dx;
  int sinAngle;
  PImage invader;

  Alien(int xpos, int ypos, PImage alienImage) {
    x = xpos;
    y = ypos;
    invader = alienImage;
    alienHeight = invader.height;
    choose = false;
  }
  Alien(int xpos, int ypos, PImage image, boolean choose) {
    x = xpos;
    y = ypos;
    dx = random(1, 1.5);
    dy = random(1, 1.25);
    invader = image;
    alienHeight = invader.height;
    this.choose = choose;
  }
  void move() {

    if (choose) {
      if (x<0 || (x+invader.width >= width)) {
        dx*=-1; // inverting direction of movement
        dy*=-1;
        x+=dx;
        y+=dy+sin(sinAngle);
        println("a:", dx, ", ", dy);
      } else if (y<0 || (y+invader.height>=height)) {
        dx*=-1;
        dy*=-1;
        x+=dx;
        y+=dy;
        println("b:", dx, ", ", dy);
      } else {
        x += dx;// x moves normally
        y += dy+sin(sinAngle); // y cause sin wave
      }
      sinAngle += 360/ 30;
      if (sinAngle>=360) sinAngle=0; // sin wave repeats every 360
    } else {
      // normal movement
      if (x<0 || (x+invader.width >= width)) {
        if (count<alienHeight) {
          y++;
          count++;
        } else {
          direction*=-1;
          x+=direction;
          count = 0;
        };
      } else {
        x += direction;
      }
    }
  }
  void draw() {
    if (exploded>=1 && exploded<10) {
      image(explode, x, y, 30, 25);
      exploded++;
    } else if (exploded==0) {
      image(invader, x, y, 30, 25);
    }
  }
  void explode() {
    if (exploded==0) {
      exploded = 1;
      //image(explode, x, y, 30, 25);
    }
  }
}
