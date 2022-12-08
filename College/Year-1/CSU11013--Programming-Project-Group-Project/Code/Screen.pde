// Faith: added screen class to implment two screens in project 23/03
//henry: integrated stars with screen class
// Faith: added ground work for drop down widget, scroll bar/slider and search widget 29/03 (commented out for now because its causing lags)
//Faith: created method overloaded screen constructor to make screen with image as background
class Screen {
  Star[] starsList;
  BarChart chart;
  PImage imageDraw = loadImage("imageBack.jpg");
  ArrayList<Widget> widgetList = new ArrayList<Widget> ();
  ArrayList<DATACLASS> searchData;
  ArrayList<DATACLASS> defaultData;
  color backgroundColor;
  ArrayList<Widget> dropDownWidgets;
  boolean displayDropDown = false;
  int firstIndexOnScreen, lastIndexOnScreen; // faith: added to prevent user from scrolling off screen
  int numOfRowsOnScreen = 0; // 0 for now
  ArrayList<Widget> Widgets;
  Widget alphabetical;
  Widget scrollDownWidget;
  Widget sortByMass;
  Widget dropDownWidget;
  Widget sortByDiameter;
  Widget dropDownSearchWidget;
  float scrollBarX, scrollBarY = 0;
  int rowSize = 0; // values to be set later
  int columnSize = 0;
  int amountOfRows = 0;
  boolean defaultObjSet = false;
  int amountOfColumns = 0;
  int startOfTable =0;
  int yOffset = 0;
  int dropDownX = 0;
  int dropDownY = 0;
  int dropDownRowSize = 0;
  int dropDownColumnSize= 0;
  int scrollBarHeight, scrollBarWidth = 0;
  boolean dropDownSortStatus = false;
  boolean dropDownSearchStatus = false;
  int dropDownSearchY = 0;
  int scrollSpeed = 0;
  TextWidget search;
  int searchEvent = 0;
  PImage image;
  PFont dropDownFont, searchFont, mainScreenFont = loadFont("CopperplateGothic-Light-18.vlw");
  //Widget widget1, widget2;
  
  Slider slider;
// constructor with colour as background
  Screen(int backgroundColor, int numStars, Slider slider) {
    defaultData= new ArrayList<DATACLASS>();
    searchData = new ArrayList<DATACLASS>();
    println("[INIT] Creating Main Screen");
    this.backgroundColor = color(backgroundColor);
    widgetList = new ArrayList();
    starsList = new Star[numStars];
    initialiseStars(starsList);
    dropDownWidget = new Widget("Sort By:", DROP_DOWN, dropDownX, dropDownY, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224), false) ;
    // alphabetical = new Widget("Show Space Objects with Data Only", SORT_MAIN_ALPHA, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //sortByMass = new Widget("Mass", SORT_BY_MASS, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    // sortByDiameter= new Widget("Diameter", SORT_BY_DIAMETER, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //for sorting drop down
    //dropDownSearchWidget = new Widget("Search By:", SEARCH_DROP_DOWN, dropDownX, dropDownSearchY, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //search = new TextWidget(x, y, dropDownColumnSize, dropDownRowSize, "", color(151, 151, 151), searchFont, searchEvent, maxlength);
    firstIndexOnScreen = 0;
    lastIndexOnScreen = firstIndexOnScreen + numOfRowsOnScreen;
    this.slider = slider;
  }
// method overloaded constructor with image as background
  Screen(PImage img, int numStars)
  {
    defaultData= new ArrayList<DATACLASS>();
    searchData = new ArrayList<DATACLASS>();
    println("[INIT] Creating Main Screen");
    this.image = img;
    widgetList = new ArrayList();
    starsList = new Star[numStars];
    initialiseStars(starsList);
    dropDownWidget = new Widget("Sort By:", DROP_DOWN, dropDownX, dropDownY, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224), false) ;
    // alphabetical = new Widget("Show Space Objects with Data Only", SORT_MAIN_ALPHA, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //sortByMass = new Widget("Mass", SORT_BY_MASS, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    // sortByDiameter= new Widget("Diameter", SORT_BY_DIAMETER, x, y, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //for sorting drop down
    //dropDownSearchWidget = new Widget("Search By:", SEARCH_DROP_DOWN, dropDownX, dropDownSearchY, dropDownColumnSize, dropDownRowSize, dropDownFont, color(209, 224, 224));
    //search = new TextWidget(x, y, dropDownColumnSize, dropDownRowSize, "", color(151, 151, 151), searchFont, searchEvent, maxlength);
    firstIndexOnScreen = 0;
    lastIndexOnScreen = firstIndexOnScreen + numOfRowsOnScreen;
  }

  void drawBackground() {
    background(backgroundColor);
  }
// draw background with image
  void drawImageBack() {
    image(imageDraw, 0, 0);
  }

  void draw() {

    drawChart(chart);
    if(this.slider != null) this.slider.draw();

    moveStars(starsList);
    displayStars(starsList);
    drawWidgets();
    amountOfRows = dataPoints.size();
    firstIndexOnScreen = abs(yOffset / 50); //our first element drawn is the offset for y didvided by 50
    lastIndexOnScreen = firstIndexOnScreen + numOfRowsOnScreen;
    if (lastIndexOnScreen < dataPoints.size()) {
      //do nothing
    } else {
      lastIndexOnScreen = dataPoints.size();
    }

    //Faith: added down to add widget 29/03

    //draw drop down menu
    //textAlign(CENTER);
    //dropDownWidget.draw();
    //dropDownSearchWidget.draw();
    //textAlign(LEFT);

    //if (dropDownSortStatus) {
    //  alphabetical.draw();
    //  sortByMass.draw();
    //  sortByDiameter.draw();
    //}

    //if (dropDownSearchStatus) {
    //  search.draw();
    //}

    ////draw search for names of space objects
    //fill(#FFFFFF);
    //textFont(mainScreenFont);
    //search.draw();

    ////draw scrollBar
    //rect(scrollBarX, scrollBarY, scrollBarWidth, scrollBarHeight);
    //fill(0);
    //updateScrollBarDrawPosition();
  }

  //void sortByAlphabetical() {
  //  //sorts data points arraylist in alpahbetical order
  //  yOffset = 0;
  //  Collections.sort(dataPoints, new alphabeticalComp());
  //}

  //public void sortBySearch(String label) {
  //  //Function which takes a space object name  and searches the data point arraylist and adjusts the arraylist if a match is found
  //  yOffset = 0;
  //  if (!defaultObjSet) {
  //    for (int i = 0; i < dataPoints.size(); i++) {
  //      DATACLASS point = dataPoints.get(i);
  //      defaultData.add(point);
  //    }
  //    defaultObjSet = true;
  //  }
  //  searchData = new ArrayList<DATACLASS>();
  //  dataPoints = defaultData;

  //  for (int i = 0; i < dataPoints .size(); i++) {
  //    DATACLASS dPoint = dataPoints .get(i);
  //    String name = dPoint.getname();
  //    if (label.equalsIgnoreCase(name)) {
  //      searchData.add(dPoint);
  //      break;
  //    }
  //  }
  //  if (label.equals("")) {
  //    dataPoints  = defaultData;
  //  } else {
  //    dataPoints = searchData;
  //  }
  //}

  //void setDropDown() {
  //  if (!dropDownSortStatus) {
  //    dropDownSortStatus = true;
  //    alphabetical.setXandY(x,y);
  //    sortByMass.setXandY(x,y);
  //    sortByDiameter.setXandY(x,y);
  //  } else {
  //    alphabetical.setXandY(x,y);
  //    sortByMass.setXandY(x,y);
  //    sortByDiameter.setXandY(x,y);
  //    dropDownSortStatus = false;
  //  }
  //}

  //void setSearchDropDown() {
  //  if (!dropDownSearchStatus) {
  //    dropDownSearchStatus = true;
  //    search.setXandY(x, y);
  //  } else {
  //    search.setXandY(x, y);
  //    dropDownSearchStatus = false;
  //  }
  //}

  //void updateScrollBarDrawPosition() {
  //  //moves the scroll bar up and down the screen
  //  if (dataPoints.size()!=0) {
  //    if (yOffset != 0) {
  //      scrollBarY = 0;
  //      scrollBarY += scrollBarHeight;
  //      //System.out.println(scrollBarHeight);
  //    }
  //  }
  //}

  //int getFirstDrawnSpaceObject() {
  //  //finds which data is drawn at top of the screen
  //  return firstIndexOnScreen;
  //}

  //int getLastDrawnSpaceObject() {
  //  //finds data drawn at end of screen
  //  return lastIndexOnScreen;
  //}

  int getEvent(int mousex, int mousey) {
    int event = 0;
    for (int i = 0; i<widgetList.size(); i++) {
      Widget aWidget = (Widget) widgetList.get(i);
      if (aWidget.event == -1)continue;
      event = aWidget.getEvent(mousex, mousey);
      if (event!=EVENT_NULL) {
        return event;
      }
    }
    return event;
  }
  void drawWidgets() {
    for (int i = 0; i<widgetList.size(); i++) {
      Widget aWidget = (Widget) widgetList.get(i);
      aWidget.draw();
    }
  }
  void addWidget(Widget widget) {
    widgetList.add(widget);
  }

  void initialiseStars(Star[] stars)
  {

    //pushMatrix();
    //translate(SCREENX/2, SCREENY/2);

    for (int i = 0; i<stars.length; i++) {
      stars[i] = new Star();
    }

    //popMatrix();

    return;
  }

  void moveStars(Star[] stars) {
    for (int i = 0; i<stars.length; i++) {
      stars[i].move();
    }
  }

  void displayStars(Star[] stars) {
    for (int i = 0; i<stars.length; i++) {
      stars[i].display();
    }
  }

  void drawChart(BarChart chartToDraw) {

    if (chartToDraw != null) {
      chartToDraw.draw();
    }

    return;
  }
  // Faith: debug keys to use 31/03
  //void keyPressed() {
  //  if (focus == null) {
  //    if (key== 'a') {
  //      screen.sortByAlphabetical();
  //    }
  //    if (key== 'q') {
  //      screen.sortByMass();
  //    }
  //    if (key== 'd') {
  //      screen.sortByDiameter;
  //    }
  //    //if (key== '') {
  //    //  screen.
  //    //}
  //  }
  //}
}
