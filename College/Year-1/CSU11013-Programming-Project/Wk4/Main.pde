Alien aliens[];
PImage invader;
Player thePlayer;
Bullet[] bullets;

void settings() {
  size(SCREENX, SCREENY);
}
void setup() {
  aliens = new Alien[10];
  invader = loadImage("invader.gif");
  initArray(aliens);
  thePlayer = new Player(SCREENY-MARGIN);
  bullets = new Bullet[0];
}
void draw() {
  background(255);
  moveArray(aliens);
  drawArray(aliens);
  thePlayer.move(mouseX -(PADDLEWIDTH/2));
  thePlayer.draw();
  moveBullets(bullets);
  drawBullets(bullets);
  collisions(bullets, aliens);
}
void initArray(Alien alienArr[]) {
  for (int i=0; i<alienArr.length; i++) {
    alienArr[i] = new Alien((1*i*invader.width), 0, invader);
  }
}
void drawArray(Alien alienArr[]) {
  for (int i = 0; i < alienArr.length; i++) {
    alienArr[i].draw();
  }
}
void moveArray(Alien alienArr[]) {
  for (int i = 0; i < alienArr.length; i++) {
    alienArr[i].move();
  }
}
void moveBullets(Bullet[] bullets) {
  for (int i=0; i<bullets.length; i++) {
    bullets[i].move();
  }
}
void drawBullets(Bullet[] bullets) {
  for (int i=0; i<bullets.length; i++) {
    bullets[i].draw();
  }
}
void mousePressed() {
  Bullet newBullet = new Bullet(mouseX);
  bullets = (Bullet[])append(bullets, newBullet);
}
void collisions(Bullet[] bullets, Alien[] alienArr) {
  for (int i=0; i<bullets.length; i++) {
    for (int j = 0; j < alienArr.length; j++)
      bullets[i].collide(alienArr[j]);
  }
}
