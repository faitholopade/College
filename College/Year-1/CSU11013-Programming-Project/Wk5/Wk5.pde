class Alien {
  int x;
  int y;
  int direction  = 1;
  PImage explode = loadImage("exploding.gif");
  int count = 0; // height of alien
  int alienHeight;
  int exploded = 0;
  PImage invader;


  Alien(int xpos, int ypos, PImage alienImage) {
    x = xpos;
    y = ypos;
    invader = alienImage;
    alienHeight = invader.height;
  }

  void move() {
    // normal movement
    if (x<0 || (x+invader.width >= width)) {
      if (count<alienHeight) {
        y+=3;
        count++;
      } else {
        direction*=-1;
        x+=direction;
        count = 0;
      }
    } else {
      x += direction;
    }
  }

  void draw() {
    if (exploded>=1 && exploded<10) {
      image(explode, x, y, 30, 25);
    } else {
      image(invader, x, y, 30, 25);
    }
  }
  void explode() {
    exploded = 1;
    image(explode, x, y, 30, 25);
    exploded++;
  }
}
