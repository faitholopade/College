int count;
float tileNum = 0;
void setup(){
  size(400,400);
}

void draw(){
  background(0);
  tileNum += 0.5;
  if(triSquare(tileNum)){
    triSquare(tileNum);
  } else {
    rect(100, 100, 100, 100);
  }
  
}

boolean triSquare(float amt){
  if(amt<50){
    beginShape();
    vertex(150+amt, 100);
    vertex(150-amt, 100);
    vertex(100, 200);
    vertex(200, 200);
    endShape(CLOSE);
    return true;
  } else {
    return false;
  }
}
