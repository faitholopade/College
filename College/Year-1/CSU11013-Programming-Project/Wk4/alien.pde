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
      };
    } else {
      x += direction;
    }
  }

  void draw() {
    if (exploded>=1 && exploded<10) {
      image(explode, x, y, 30, 25);
      if (int(random(0, 1))==int(random(0, 1))) {
        powerUp1=true;
        fill(#6308FA);
        rect(x, y, 50, 50);
        y+=20;
      }
      if (int(random(0, 3))==int(random(0, 3))) {
        powerUp2=true;
        fill(#FAD608);
        rect(x, y, 50, 50);
        y+=20;
      }
      if (int(random(0, 5))==int(random(0, 5))) {
        powerUp3=true;
        fill(#08FACF);
        rect(x, y, 50, 50);
        y+=20;
      }
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
