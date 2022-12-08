//Faith: added checkbox class to be used to implement button system 31/03
//class CheckBoxWidget extends Widget {

//  int scale;
//  boolean clicked;


// constuctor

//  CheckBoxWidget(int x, int y, int scale, color widgetColor, PFont widgetFont, boolean clicked, int event) {
//    this.x=x;
//    this.y=y;
//    this.widgetWidth = scale;
//    this.widgetHeight= scale;
//    this.widgetColor=widgetColor;
//    this.widgetFont=widgetFont;
//    this.label = "";
//    this.event = event;
//    this.clicked = clicked;
//    labelColor= color(255, 255, 255);
//    strokeColor = color(0);
//  }

// draw

//  void draw() {
//    if (mouseX > x && mouseX < x + widgetWidth
//      && mouseY > y && mouseY < y + widgetHeight) {
//      strokeColor = color(255, 255, 255);
//    } else {
//      strokeColor = color(0, 0, 0);
//    }
//    stroke(strokeColor);
//    fill(widgetColor);
//    rect(x, y, widgetWidth, widgetHeight);
//    fill(labelColor);
//    textFont(widgetFont);
//    text(label, x+8, y+widgetHeight-8);
//  }

//clicked 

//  void click() {
//    if (!clicked) {
//      label = "X";
//      clicked = !clicked;
//    } else {
//      label = "";
//      clicked = !clicked;
//    }
//  }

// clear clicked result

//  void clear() {
//    label = "";
//    clicked = false;
//  }
//}
