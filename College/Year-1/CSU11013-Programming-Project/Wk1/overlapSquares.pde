  int i = 0;
  int j = 0;
  int k = 500;
  
  void setup(){
    size (500,  500);
  //  frameRate();
    
  }
  void draw() {
    background (255);
    noStroke();
    rect(i++, 250, 100, 100);
    fill(#14FF00);
    rect(350, j++, 100, 100);
    fill(#FAFF00);
    rect (k--, 200, 100, 100);
    fill(#00FFC1);
  }
