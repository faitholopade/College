
void setup() {
  Widget widget1, widget2, widget3;
  size(400, 400);
  stdFont=loadFont("Chiller-Regular-36.vlw");
  textFont(stdFont);
  widget1=new Widget((width/2)-(100/2), 100, 100, 40, "red", color(100), stdFont, EVENT_BUTTON1);
  widget2=new Widget((width/2)-(100/2), 200, 100, 40, "green", color(100), stdFont, EVENT_BUTTON2);
  widget3=new Widget((width/2)-(100/2), 300, 100, 40, "blue", color(100), stdFont, EVENT_BUTTON3);
  widgetList = new ArrayList();
  widgetList.add(widget1);
  widgetList.add(widget2);
  widgetList.add(widget3);
}
void draw() {
  for (int i = 0; i<widgetList.size(); i++) {
    Widget aWidget = (Widget) widgetList.get(i);
    aWidget.draw();
  }
}
void mousePressed() {
  int event;
  for (int i = 0; i<widgetList.size(); i++) {
    Widget aWidget = (Widget) widgetList.get(i);
    event = aWidget.getEvent(mouseX, mouseY);
    switch(event) {
    case EVENT_BUTTON1:
      println("red");
      fill(#FA0505);
      rect(50, 50, 100, 100);
      aWidget.widgetColor = color(#FA0505);
      break;
    case EVENT_BUTTON2:
      println("green");
      fill(#098603);
      rect(50, 50, 100, 100);
      aWidget.widgetColor = color(#098603);
      break;
    case EVENT_BUTTON3:
      println("blue");
      fill(#0752E3);
      rect(50, 50, 100, 100);
      aWidget.widgetColor = color(#0752E3);
      break;
    }
  }
}
void mouseMoved() {
  int event;
  for (int i = 0; i<widgetList.size(); i++) {
    Widget aWidget = (Widget) widgetList.get(i);
    event = aWidget.getEvent(mouseX, mouseY);
    if (event!= EVENT_NULL) {
      aWidget.strokeColour = color(255);
    } else {
      aWidget.strokeColour = color(0);
    }
  }
}
