//Faith: added button class to tie in buttons with field

class Button {

  int x, y;
  int buttonWidth = 0;
  int buttonHeight = 0;
  String currentField;
  boolean selected = false;
  
  // constructor: used to initialise a new button object
  Button(int x, int y, String field)
  {
    this.x = x;
    this.y = y;
    this.currentField = field;
  }

//checkClick(): allows us to know if button has been clicked and displays output based on result
  void checkClick() {
    if (mouseX >= x && mouseX < x+buttonWidth &&
      mouseY >= y && mouseY < y+buttonHeight) {
      currentField = null;
      selected = true;
    } else {
      selected = false;
    }
  }

//draw(): draws button object to screen
  void draw() {
    String field = null;
    if (selected) {
    } else {
    }
    rect(x, y, buttonWidth, buttonHeight);
  }
}
