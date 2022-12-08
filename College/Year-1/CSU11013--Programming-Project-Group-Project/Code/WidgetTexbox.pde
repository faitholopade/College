
//Henry: TextBox class for user to enter data
class TextBox extends Widget {

  boolean selected;
  String userText;
  int textLength;
  int TEXTSIZE = 24;


  TextBox(String label, int event, int x, int y, int width, int height, PFont widgetFont, color widgetColor) {

    super(label, event, x, y, width, height, widgetFont, widgetColor, false);
    selected = false;
    userText = "";
    textLength = 0;
    labelColor = color(0);
  }

  void setSelect(boolean sel) {
    this.selected = sel;
  }



  private void addText(char text) {
    // IF THE TEXT WIDHT IS IN BOUNDARIES OF THE TEXTBOX
    if (textWidth(userText + text) < width) {
      userText += text;
      textLength++;
    }

    println("Adding " + text);
    println(userText);
  }

  private void backSpace() {
    if (textLength - 1 >= 0) {
      userText = userText.substring(0, textLength - 1);
      textLength--;
    }
  }
  void pressedKey(char key, int keyCode) {

    if (selected) {
      if (keyCode == (int)BACKSPACE) {
        this.backSpace();
      } else if (keyCode == 32) {
        // SPACE
        addText(' ');
      } else if (keyCode == (int)ENTER) {
        activeQuery = queryData(dataPoints, queryField, userText);
        return;
      } else {
        // CHECK IF THE KEY IS A LETTER OR A NUMBER
        boolean isKeyCapitalLetter = (key >= 'A' && key <= 'Z');
        boolean isKeySmallLetter = (key >= 'a' && key <= 'z');
        boolean isKeyNumber = (key >= '0' && key <= '9');

        if (isKeyCapitalLetter || isKeySmallLetter || isKeyNumber) {
          addText(key);
        }
      }
    }
  }

  void draw() {
    if (isStroke) {
      stroke(255, 0, 0);
    } else {
      noStroke();
    }
    fill(widgetColor);
    rectMode(CENTER);
    rect(x, y, width, height); //noStroke();
    fill(labelColor);
    textFont(widgetFont);
    textAlign(CENTER);
    text(userText, x, y);
    rectMode(CORNER);
  }
}
