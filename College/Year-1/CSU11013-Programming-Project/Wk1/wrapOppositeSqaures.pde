    int i = 0;
    int k = -499;
    int l = 900;
    int m = 400;
    
    void setup(){
      size (500, 500);
      frameRate(60);
        
    }
    void draw() {
      background (255);
      noStroke();
      smooth();
      fill(#F6FF00);
      rect(i++, 150, 100, 100);
      
      rect (k++, 150, 100, 100);
      fill(#ED0241);
      rect(l--, 300, 100, 100);
      
      rect (m--, 300, 100, 100);
        fill(#ED0241);
      
      if (i >= 499) {
          i = 0;
        k = -499;
        l = 900;
        m = 400;
      }
        
    }
