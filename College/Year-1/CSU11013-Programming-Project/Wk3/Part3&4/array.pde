int SCREENX = 500;
int SCREENY = 500;
Alien aliens[];
PImage invader;
PImage invader2;
void settings() {
  size(SCREENX, SCREENY);
}
void setup() {
  aliens = new Alien[10];
  invader = loadImage("invader.gif");
  invader2= loadImage("invader2.png");
  initArray(aliens);
}
void draw() {
  background(255);
  moveArray(aliens);
  drawArray(aliens);
}
void initArray(Alien alienArr[]) {
  for (int i=0; i<alienArr.length; i++) {
    if (int(random(0, 5))==int(random(0, 5))) alienArr[i] = new Alien((1*i*invader2.width), 0, invader2, true);
    else alienArr[i] = new Alien((1*i*invader.width), 0, invader);
  }
}
void drawArray(Alien alienArr[]) {
  for (int i = 0; i < alienArr.length; i++) {
    if (int(random(0, 500))==i) { // decreasing chance so they stay on screen longer
      alienArr[i].explode();
    }
    alienArr[i].draw();
  }
}
void moveArray(Alien alienArr[]) {
  for (int i = 0; i < alienArr.length; i++) {
    alienArr[i].move();
  }
}
