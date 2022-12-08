class Shield {
  int x;
  int y;

  Shield(int xpos, int ypos) {
    x = xpos;
    y = ypos;
  }
  //void move(float x) {
  //  if (x>SCREENX-PADDLEWIDTH) xpos = SCREENX-PADDLEWIDTH;
  //  else if (x<0) xpos = 0;
  //  else xpos = x;
  //}
  void draw() {
    fill(#07F5E7);
    rect(x, y, 100, 100);
  }
}
