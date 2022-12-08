class Player {
  float xpos;
  float ypos;
  Player(float screen_y) {
    xpos = SCREENX/2;
    ypos = screen_y;
  }
  void move(float x) {
    if (x>SCREENX-PADDLEWIDTH) xpos = SCREENX-PADDLEWIDTH;
    else if (x<0) xpos = 0;
    else xpos = x;
  }
  void draw() {
    fill(0);
    rect(xpos, ypos, PADDLEWIDTH, PADDLEHEIGHT);
    rect(xpos+((PADDLEWIDTH/2)-(PADDLEWIDTH/8)), ypos-(PADDLEHEIGHT/2), PADDLEWIDTH/4, PADDLEHEIGHT/2);
  }
}
