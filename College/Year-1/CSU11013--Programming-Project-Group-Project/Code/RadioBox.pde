//Faith: added radiobox class to be used later 31/03
//class RadioBoxWidget extends Widget {

//  int x;
//  int y;
//  color widgetColor;
//  PFont widgetFont;
//  int clicked;
//  int[] event;
//  int chkBoxScale;


// array list of check boxes
//  ArrayList<CheckBoxWidget> chkboxWidgets = new ArrayList<CheckBoxWidget>();

// constructor 
//  RadioBoxWidget(int x, int y, color widgetColor, PFont widgetFont, int clicked, int[] event, int chkBoxScale) {
//    this.x=x;
//    this.y=y;
//    this.widgetColor=widgetColor;
//    this.widgetFont=widgetFont;
//    this.event = event;
//    this.clicked = clicked;
//    this.chkBoxScale = chkBoxScale;

//    chkboxWidgets.add(new CheckBoxWidget(x, y, 40, widgetColor, widgetFont, false, event[0]));
//    chkboxWidgets.add(new CheckBoxWidget(x, y + 50, 40, widgetColor, widgetFont, false, event[1]));
//    chkboxWidgets.add(new CheckBoxWidget(x, y + 100, 40, widgetColor, widgetFont, false, event[2]));

//    for (int i = 0; i < chkboxWidgets.size(); i++) {
//      if (i == clicked) {
//        chkboxWidgets.get(i).click();
//      }
//    }
//  }

// draw
//  void draw() {
//    for (int i = 0; i < chkboxWidgets.size(); i++) {
//      chkboxWidgets.get(i).draw();
//    }
//  }

// clicked
//  void click(int chkbox) {
//    chkboxWidgets.get(chkbox).click();
//    for (int i = 0; i < chkboxWidgets.size(); i++) {
//      if (i != chkbox) {
//        chkboxWidgets.get(i).clear();
//      }
//    }
//  }

// get event
//  int getEvent(int mX, int mY) {
//    for (int i = 0; i < chkboxWidgets.size(); i++) {
//      if (mX > chkboxWidgets.get(i).x && mX < chkboxWidgets.get(i).x + chkBoxScale && mY > chkboxWidgets.get(i).y && mY < chkboxWidgets.get(i).y + chkBoxScale) {
//        return event[i];
//      }
//    }
//    return EVENT_NULL;
//  }
//}
