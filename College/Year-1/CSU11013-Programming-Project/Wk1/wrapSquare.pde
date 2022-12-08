  int i = 0;
  int k = -499;
  
  void setup(){
    size (500, 500);
    frameRate(60);
      
  }
  void draw() {
    background (255);
    noStroke();
    smooth();
    rect(i++, 150, 100, 100);
    fill(#F6FF00);
    rect (k++, 150, 100, 100);
      fill(#F6FF00);
    
    if (i >= 499) 
        i = 0;
    if (k >= 0)
      k = - 499;
  }
