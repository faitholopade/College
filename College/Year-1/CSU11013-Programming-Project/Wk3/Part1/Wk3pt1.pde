Alien theAlien;

void setup() {

  size(500, 500);
  PImage alien = loadImage("invader.gif");
  theAlien = new Alien(10, 10, alien);
 frameRate(60);
}

void draw() {
  background(255);
  theAlien.move();
  theAlien.draw();
}
