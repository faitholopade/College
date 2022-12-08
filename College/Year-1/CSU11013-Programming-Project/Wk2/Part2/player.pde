class Player {
  float xpos;
  float ypos;
  int score;
  color paddlecolor = color(255);
  boolean player;
  float dy;

  Player(int screen_x) { // constructor function
    xpos=screen_x;
    ypos=SCREENY/2;
  }
  void move(float y) {
    if (y>SCREENY-PADDLEHEIGHT) ypos = SCREENY-PADDLEHEIGHT;
    else ypos=y;
  }
  void computerMove() {
    dy = random(1, 2);
    if (theBall.x < SCREENX && theBall.x >0) {
      if (theBall.y > (ypos + PADDLEHEIGHT)) {
        ypos+=dy;
      } else if (theBall.y < ypos) ypos-=dy;
    }
    if ((xpos + PADDLEWIDTH) >= SCREENX) xpos = (SCREENX-PADDLEWIDTH);
  }
  void draw() {
    fill(paddlecolor);
    rect(xpos, ypos, PADDLEWIDTH, PADDLEHEIGHT);
  }
  void score() {
    score++;
  }
  void reset() {
    score=0;
  }
}
