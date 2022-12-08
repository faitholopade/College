class Bomb {
  float xbomb;
  float ybomb;
  int width;
  int height;
  Bomb(float xpos, float ypos) {
    xbomb = xpos;
    ybomb = ypos;
    width = 30;
    height = 30;
  }

  void move() {
    ybomb++;
  }
  void draw() {
    stroke(0);
    fill(255, 255, 0);
    rect(xbomb, ybomb, width, height);
  }
  boolean offScreen() {
    if (ybomb>SCREENY) {
      return true;
    } else return false;
  }
  boolean collide(Player player) {
    boolean collidedX = false;
    if (xbomb>=player.xpos && xbomb<=player.xpos+PADDLEWIDTH) {
      collidedX = true;
    }
    boolean collidedY = false;
    if (ybomb>player.ypos-PADDLEHEIGHT*0.5 && ybomb<=player.ypos+PADDLEHEIGHT) {
      collidedY = true;
    }

    if (collidedX&&collidedY&&!offScreen()) {
      //collided
      return true;
    } else return false;
  }
}
