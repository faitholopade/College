class Bullet {
  float xpos;
  float ypos;
  int bulletHeight;
  int bulletWidth;
  Bullet(int x) {
    bulletHeight = 25;
    bulletWidth = 5;
    xpos = x-(bulletWidth/2);
    ypos = SCREENY - (MARGIN+(1.5*PADDLEHEIGHT));
  }
  void move() {
    ypos-=3;
  }
  void draw() {
    noStroke();
    fill(#05D0F2);
    rect(xpos, ypos, bulletWidth, bulletHeight);
  }
  void collide(Alien alien) {
    boolean touchedX = false;
    boolean touchedY = false;
    if (xpos>=alien.x && xpos<=alien.x+invader.width) {
      touchedX = true;
    }
    if (ypos>=alien.y && ypos<=alien.y+invader.height) {
      touchedY = true;
    }
    if (touchedX&&touchedY) {
      alien.explode();
    }
  }
}
