  float a = 0.0;
  float inc = TWO_PI/50.0;
  int i = 0; 
  int k = -499;
  int l = 900;
  int m = 400;
  int b;
  int c;
  
   
  void setup(){
  size(500, 500);
  int b = 0;
  int c = 0;
  }
  
  
  void draw(){
    background (255);
    noStroke();
  
  if (i < 400); 
  fill(255, 204, 0+c);
  rect(i++, 200+sin(a)*160.0, 100, 100);
  rect (k++, 200+sin(a)*160.0, 100, 100); 
  fill(148, 255, 0+c );
  rect(l--, 200+sin(a)*160.0, 100, 100);
  rect (m--, 200+sin(a)*160.0, 100, 100);
    
  
    if (i >= 499) {
        i = 0;
        k = - 499;
        l = 900;
        m = 400;
       
    }
     
    a = a + inc;
    if (b==0) c++;
    if (c>255) b = 1;
    if (b == 1) c--;
    if (c<=0) b = 0;
  }

// reference: https://processing.org/reference/sin_.html
