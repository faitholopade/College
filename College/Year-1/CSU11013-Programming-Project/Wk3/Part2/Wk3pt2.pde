/* Declare an array of Aliens */
Alien aliens[];
PImage invader;


void setup() {
  size(500, 500);
  /* create a new alien array */
  aliens = new Alien[10];
  /* load the images */
  invader = loadImage("invader.gif");
  /* initialise the array */
  init_array(aliens, invader);
}

void init_array(Alien theArray[], PImage invader) /* pass in your array, plus the image to use */ {
  for (int i=0; i<theArray.length; i++) {
    theArray[i] = new Alien ((1*i*invader.width), 0, invader);
    /* for each position in the array,  create a new alien at an appropriate
     starting point on the screen */
  }
}

void draw() {
  /* clear the screen */
  background(255);
  /* for each alien in the array - move the alien, then draw the alien */
  for (int i=0; i<aliens.length; i++) {
    aliens[i].move();
    aliens[i].draw();
  }
}
