Player thePlayer;
Ball theBall;

void settings(){
  size(SCREENX, SCREENY);
}

void setup(){
  ellipseMode(RADIUS);
  thePlayer = new Player(SCREENY-MARGIN-PADDLEHEIGHT);
  theBall = new Ball();
}

void draw(){
  background(0);
  thePlayer.move(mouseX);
  theBall.update();
  theBall.collide(thePlayer);
  theBall.draw();
  thePlayer.draw();
}
