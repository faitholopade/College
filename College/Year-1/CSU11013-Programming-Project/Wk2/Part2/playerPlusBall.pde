Ball  theBall;
Player thePlayer;
Player theComputer;

boolean cont;

void settings() {
  size(SCREENX, SCREENY);
}

void setup() {
  thePlayer = new Player(0+MARGIN);
  theComputer = new Player(SCREENX - MARGIN - PADDLEWIDTH);
  theBall = new Ball();
  ellipseMode(RADIUS);
  cont = true;
  thePlayer.score = 0;
  theComputer.score = 0;
  PFont myFont = loadFont("ArialNarrow-18.vlw");
  textFont(myFont);
}

void draw() {
  if (cont) {
    background(0);
    thePlayer.move(mouseY-(PADDLEHEIGHT/2));
    theComputer.computerMove();

    thePlayer.draw();
    theComputer.draw();
    theBall.move();
    if (theBall.x<SCREENY/2) {
      theBall.collide(thePlayer);
    } else theBall.collide(theComputer);
    int scored = theBall.score();
    if (scored!=0) {
      reset(scored);
    }
    thePlayer.draw();
    theComputer.draw();
    theBall.draw();

    text(thePlayer.score, SCREENX/2-100, 50);
    text(theComputer.score, SCREENX/2+100, 50);
    text("Current Speed:\n"+ theBall.dy, PADDLEWIDTH+MARGIN+10, 50);
  } else {
    reset(0);
  }
}

void reset(int scorer) {
  cont = false;
  theBall.gameReset();
  // Scoring logic
  if (theComputer.score>=3) {
    text("Game Over\nYou Lose", SCREENX/2, SCREENY/2);
    fullReset();
  } else if (thePlayer.score>=3) {
    text("Game Over\nYou Win", SCREENX/2, SCREENY/2);
    fullReset();
  } else {
    if (scorer == -1) {
      thePlayer.score();
    } else if (scorer == 1) {
      theComputer.score();
    }
  }
}
void fullReset() {
  theComputer.score = 0;
  thePlayer.score = 0;
}
void mousePressed() {
  cont = true;
}
