class Star { //Star class for a starfield - Henry 23/02/22

  int x;
  int y;
  int z;
  int starRadius;
  int spinSpeed;
  int tint;
  float angle;

  Star() {
    x = int(random(-SCREENX, SCREENX));
    y = int(random(-SCREENY, SCREENY));
    angle = 0;
    starRadius = 5;
    spinSpeed = int(random(700, 1500));
  }



  void display() {

    fill(255);
    noStroke();

    pushMatrix();
    translate(SCREENX/2, SCREENY/2);
    rotate(angle);
    //circle(x, y, starRadius);#
    tint(255, 125);
    image(star, x, y, 20, 20);
    //print(x, y);

    popMatrix();
  }


  void move() {

    angle = (angle + (PI/(spinSpeed * spinMultiplier))) % (2*PI);
  }
}
