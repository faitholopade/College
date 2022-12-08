Alien aliens[];
PImage invader;
Player thePlayer;
Bullet[] bullets;
Bomb[] bombs;
PFont myFont;
Shield[] shields;


void settings() {
  size(SCREENX, SCREENY);
}
void setup() {
  aliens = new Alien[10];
  invader = loadImage("invader.gif");
  initArray(aliens);
  thePlayer = new Player(SCREENY-MARGIN);
  bullets = new Bullet[0];
  bombs = new Bomb[10];
  initBombs(bombs);
  myFont = loadFont("AgencyFB-Reg-48.vlw");
  shields = new Shield[4];
  initShields(shields);
}
void draw() {
  if (!thePlayer.hit) {
    if (allAliensDead(aliens)) {
      fill(0);
      background(255);
      text("You Win", SCREENX/2, SCREENY/2);
    } else {
      background(255);
      dropBomb(bombs, aliens);
      moveArray(aliens);
      drawArray(aliens);
      thePlayer.move(mouseX -(PADDLEWIDTH/2));
      thePlayer.draw();
      moveBullets(bullets);
      drawBullets(bullets);
      collisions(bullets, aliens);
      moveBombs(bombs);
      bombCollisions(bombs, thePlayer);
      drawBombs(bombs);
    }
  } else {
    background(255);
    fill(0);
    text("Game Over", SCREENX/2, SCREENY/2);
  }
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
void initBombs (Bomb[] bombs) {
  for (int i = 0; i< bombs.length; i++) {
    bombs[i] = new Bomb (1000, 1000);
  }
}
void moveBombs (Bomb[] bombs) {
  for (int i = 0; i < bombs.length; i++) {
    if (!bombs[i].offScreen()) {
      bombs[i].move();
    }
  }
}
void drawBombs(Bomb[] bombs) {
  for (int i = 0; i < bombs.length; i++) {
    if (!bombs[i].offScreen()) {
      bombs[i].draw();
    }
  }
}
void dropBomb(Bomb[] bombs, Alien[] aliens) {
  for (int i = 0; i < aliens.length; i++) {
    if (aliens[i].exploded == 0) {
      // can drop bomb
      if (bombs[i].offScreen()) {
        //randomly drop bomb
        if (int(random(1, 500))==1) {
          bombs[i] = new Bomb(aliens[i].x, aliens[i].y);
          //println("Bomb Dropped");
        }
      }
    }
  }
}
void bombCollisions(Bomb[] bombs, Player player) {
  for (int i = 0; i< bombs.length; i++) {
    if (bombs[i].collide(player)) {
      println("collide");
      player.hit=true;
    }
  }
}

boolean allAliensDead(Alien[] aliens) {
  boolean allDead = true;
  for (int i = 0; i< aliens.length; i++) {
    allDead = allDead && (aliens[i].exploded>=1);
  }
  return allDead;
}
void initShields(Shield[] shields) {
  for (int i = 0; i< shields.length; i++) {
    shields[i] = new Shield(100, 100);
  }
}
void drawShields(Shield[] shields) {
  for (int i = 0; i< shields.length; i++) {
    shields[i].draw();
  }
}
