//Faith: added widget class to display data, this will be used for space object widgets 23/03
class Widget {
  int x, y, width, height;
  String label;
  int event;
  color widgetColor, labelColor;
  PFont widgetFont;
  boolean isStroke = false;
  boolean rectCorner;
  boolean selected;
  // Widget constructor given a position (x,y).
  Widget(String label, int event, int x, int y, int width, int height, PFont widgetFont, color widgetColor, boolean rectMode) { //int x, int y, int width, int height,
    this.x=x;
    this.y=y;
    this.width = width;
    this.height= height;
    this.label=label;
    this.event=event;
    this.widgetColor=widgetColor;
    this.widgetFont=widgetFont;
    this.rectCorner = rectMode;
    this.selected = false;
    labelColor= color(0);
  }

// sets widget boarder colour based on if mouse is over
  void setStroke(boolean set) {
    isStroke = set;
  }

  // draw(): Draws a widget.
  void draw() {
    if(selected){
      strokeWeight(6);
      stroke(255, 0, 0);
    }
    
    else if(isStroke){
      strokeWeight(4);
      stroke(255, 0, 0);
    }
    else {
      noStroke();
    
    }
    fill(widgetColor);
    if (rectCorner) rectMode(CORNER);
    else {
      rectMode(CENTER);
    }

    rect(x, y, width, height); //noStroke();
    fill(labelColor);
    textFont(widgetFont);
    textAlign(CENTER);
    text(label, x, y + 10, width, height);
    rectMode(CORNER);
    strokeWeight(4);
  }
  // getEvent(int mX, int mY): Returns the widget's event code if it is moused over. (See mouseMoved() and mousePressed() in main file.)
  int getEvent(int mX, int mY) {
    
    if(rectCorner){
      if (mX>x && mX < x+width && mY >y && mY <y+height) {
        return event;
      }
      return EVENT_NULL;
    }
    
    else{
      
      if (mX>x - width/2 && mX < x+width/2 && mY >y-height/2 && mY <y+height/2) {
        return event;
      }
      return EVENT_NULL;
    }
    
  }
  void setXandY(int x, int y) {
    this.x = x;
    this.y = y;
  }
}
