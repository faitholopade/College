class Ball {
  float x;
  float y;
  float dx;
  float dy;
  int radius;
  color ballColour = color(255);

  Ball() {
    x = random(SCREENX/4, SCREENX/2);
    y = random(SCREENY/4, SCREENY/4);
    dx = random(1, 2);
    dy = random(1, 2);
    radius = 10;
  }

  void move() {
    x = x + dx;
    y = y + dy;
  }

  void draw() {
    fill(ballColour);
    ellipse(int(x), int(y), radius, radius);
  }

  void collide(Player tp) {
    // wall Collision
    if (y-radius<=0) {
      dy=-dy;
    } else if (y+radius>=SCREENY) {
      dy=-dy;
    }
    if (x+radius >= tp.xpos && // paddle collison
      x<=tp.xpos+PADDLEWIDTH+MARGIN && y>tp.ypos
      && y<tp.ypos+PADDLEHEIGHT) {
      println("collided!");
      dx=-dx;
    }
  }

  int score() {
    if (x-radius>=SCREENX) {
      return int(-1);
    } else if (x+radius<=0) {
      return int(1);
    } else return int(0);
  }

  void gameReset() {
    x = random(SCREENX/4, SCREENX/2);
    y = random(SCREENY/4, SCREENY/2);
    dx = dx + 0.005;
    dy = dy + 0.005;
  }
}
