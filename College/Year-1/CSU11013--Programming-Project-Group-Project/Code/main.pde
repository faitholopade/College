/* //<>//
created by Faith Olopade 18/03/2022
 called a method in setup to read in data
 created arraylist to store multiple instances of class
 */

//Henry: Added logic to integrate class into loading structure, adding function to print the data - 3/19/2022
// Faith: updated loadData method to process tsv file 19/03/2022
// Manuel : added function to print values on the screen and a mouseWheel function to scroll down the list 20/03/2033
// Faith: added print line and text to let user know fonts are being loaded 23/03
//Faith: created a (simple) query result and set this to be the current query (in setup) 23/03
// Faith: added among us space sounds  23/03
// Manuel: upgraded the printData function - 24/03
//Henry: merged conflicts with latest main screen - 25/3
// Faith: full screen fucntion 30/03
//Faith: added buttons to filter between fields 1/04
// Faith: made screen background an image

import java.util.*;
import java.time.format.DateTimeFormatter; // for formatting date
import java.time.LocalDate;
import processing.sound.*;

ArrayList<DATACLASS> dataPoints;
ArrayList<DATACLASS> activeQuery;
PFont outputFont;
int initialYPositionForOutput = 30;
int currentYPositionTable = initialYPositionForOutput + 10;
Star[] stars;
PImage imageBack;

//String sortType;
BarChart theBarChart;
PImage star;
int currentScreen;
public static String currentField;

Screen mainScreen, queryScreen, activeScreen;
Button nameButton, massButton, diameterButton, perigeeButton, apogeeButton, lDateButton, statusButton;

Widget mainButton1;
Widget queryBoundingBoxLeft;
Widget queryBoundingBoxRight;

Widget button1, button2, button3, button4, button5, button6; // Textbox sort buttons

// Barchart sort buttons
ArrayList sortTypeWidgetList;
Widget widget1, widget2, widget3, widget4, widget5; 
color selectColor = color(255);


Slider mySlider;


TextBox textBox1;
String queryField = "name";
SoundFile mainTrack, clickEffect;

boolean userPressesReturn = false;
boolean dataPrinted = false;

int offsetForTable = 0;
boolean isNameVisible = false;                    // variables to allow for a correct slider implementations -- Manuel
boolean isApogeeVisible = false;


void settings()
{
  size(SCREENX, SCREENY);  // setting screen size 
}

void setup()
{
  //fullScreen();
  star = loadImage("star.png"); // loading star image
  background(0);
  loadData(); // calling function to load data
  outputFont = loadFont("CopperplateGothic-Light-18.vlw"); // text font
  println("[INIT] Loading Fonts");
  textFont(outputFont);  // text
  text("LOADING", (SCREENX / 2) - 0.2*SCREENX, SCREENY / 2); //text position


  mainButton1 = new Widget("Launch", MAIN_BUTTON, SCREENX/2, SCREENY/2, 100, 40, outputFont, color(255), false);  // launch button
  button1 = new Widget("Name", NAME_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 300, 150, 30, outputFont, color(255), false);  // name button
  button2 = new Widget("Status", STATUS_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 350, 150, 30, outputFont, color(255), false);  // status button
  button3 = new Widget("Mass", MASS_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 400, 150, 30, outputFont, color(255), false);  // mass button
  button4 = new Widget("Diameter", DIAMETER_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 450, 150, 30, outputFont, color(255), false);  //diameter button
  button5 = new Widget("Perigee", PERIGEE_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 500, 150, 30, outputFont, color(255), false);  // perigee button
  button6 = new Widget("Apogee", APOGEE_QUERY_BUTTON, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 550, 150, 30, outputFont, color(255), false);  // apogee button

  queryBoundingBoxLeft = new Widget("", -1, 0, 0, QUERY_BOUNDING_BOX_WIDTH, SCREENY, outputFont, color(100), true); // left border of info screen
  queryBoundingBoxRight = new Widget("", -1, SCREENX-QUERY_BOUNDING_BOX_WIDTH, 0, QUERY_BOUNDING_BOX_WIDTH, SCREENY, outputFont, color(100), true); // right border of info screen
  textBox1 = new TextBox("", QUERY_TEXTBOX, SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 200, 150, 30, outputFont, color(255));  // user input box
  
  
  widget1 = new Widget("", EVENT_BUTTON1, 50, 300, 15, 15, outputFont, selectColor, true); // various widgets
  widget2 = new Widget("", EVENT_BUTTON2, 50, 330, 15, 15, outputFont, selectColor, true);
  widget3 = new Widget("", EVENT_BUTTON3, 50, 360, 15, 15, outputFont, selectColor, true);
  widget4 = new Widget("", EVENT_BUTTON4, 50, 390, 15, 15, outputFont, selectColor, true);
  widget5 = new Widget("", EVENT_BUTTON5, 50, 420, 15, 15, outputFont, selectColor, true);
  




// adding widgets to various screens 
  // creating slider
  mySlider = new Slider(LEFT_LINE + 125, CENTRAL_LINE - 25, 150);

  // adding widgets to various screens
  mainScreen = new Screen(0, 500, null);
  mainScreen.addWidget(mainButton1);
  queryScreen = new Screen(0, 0, mySlider);
  queryScreen.addWidget(queryBoundingBoxLeft);
  queryScreen.addWidget(queryBoundingBoxRight);
  queryScreen.addWidget(textBox1);
  queryScreen.addWidget(button1);
  queryScreen.addWidget(button2);
  queryScreen.addWidget(button3);
  queryScreen.addWidget(button4);
  queryScreen.addWidget(button5);
  queryScreen.addWidget(button6);
  
  queryScreen.addWidget(widget1);
  queryScreen.addWidget(widget2);
  queryScreen.addWidget(widget3);
  queryScreen.addWidget(widget4);
  queryScreen.addWidget(widget5);

  activeScreen = mainScreen;

// setting query

  activeQuery = queryData(dataPoints, "name", "Vanguard I");

// creating bar chart

  theBarChart = new BarChart(dataPoints, sortType);
  
  
  queryScreen.chart = theBarChart;
  


  //Sound - Henry
  mainTrack = new SoundFile(this, "glitch-looking-at-the-night-sky.wav");
  clickEffect = new SoundFile(this, "scifi-click.wav");
  mainTrack.loop();
}

void draw()
{
  //background(0);
  //fill(255);



  //theBarChart.draw();

  //Logic to make the starfield work - Henry (translate to the center and translates back)

  screenTransitioner();
  //theBarChart.sortType = "mass";
  activeScreen.drawBackground();
  queryScreen.drawImageBack();


  if (activeScreen == queryScreen) printData(activeQuery, 1000);

  activeScreen.draw();
  if(activeScreen == queryScreen){
    displaySelectBoxText();
    textAlign(CENTER);
    textSize(15);
    text("Query(enter to search)", SCREENX - QUERY_BOUNDING_BOX_WIDTH/2, 170);
    textSize(17); //reset to default
  }
/*  if (dataPrinted && userPressesReturn)
  {
    theBarChart = new BarChart(activeQuery, "mass");
    queryScreen.chart = theBarChart;
    userPressesReturn = false;
    dataPrinted = false;
  }
*/
}
//Henry - added screen transitions 25/3
void screenTransitioner() {



  if (activeScreen == queryScreen) return;

  if (spinMultiplier == 0.03) {
    if (screenTransition == false) {
      if (millis() - millsSinceSpinMaxReached >= 2000) {
        activeScreen = queryScreen;
      }
    } else {
      screenTransition = false;
      //activeScreen = queryScreen;
      millsSinceSpinMaxReached = millis();
      return;
    }
  }

  if (screenTransition) {

    spinMultiplier = max(0.03, spinMultiplier * 0.95);
  }

  return;
}

// Faith: function to load data and create arraylist containing multiple instances of class
void loadData()
{
  table = loadTable("gcat10k.tsv", "header");
  // let user know data being read
  print("[INIT] Loading Dataset");
  //create new data point every space entity in file
  dataPoints = new ArrayList<DATACLASS>();
  //dataPoints contains the points from each entry (or line)

  String name;
  String launchDate;
  String status;
  String mass;
  String diameter;
  String perigee;
  String apogee;

  for (TableRow row : table.rows())
  {
    if (table.rows()!= null)
    {
      name = row.getString("Name");
      launchDate = row.getString("LDate");
      status = row.getString("Status");
      mass = row.getString("Mass");
      diameter = row.getString("Diameter");
      perigee = row.getString("Perigee");
      apogee = row.getString("Apogee");


      DATACLASS tempClass = new DATACLASS(name, launchDate, status, mass, diameter, perigee, apogee);
      dataPoints.add( new DATACLASS (name, launchDate, status, mass, diameter, perigee, apogee));
    }
  }
  //Todo: create the class representation and convert from array to class attributes/list (datapoints[0] = #JCAT, d[1] = satcat, etc...)

  //println(dataPoints); Data loaded correctly
}

//Henry: adding function to print data
void printData(ArrayList<DATACLASS> dataToPrint, int numEntries)
{
  offsetForTable += mySlider.dx;                                            // offset that increases or decreases with slider movement -- Manuel
  int initialXPosition = LEFT_LINE + 125 + offsetForTable;
  if (initialXPosition >= LEFT_LINE + 125)
    isNameVisible = true;
  mySlider.dx = 0;
  int localYPos = currentYPositionTable;
  dataPrinted = true;

  DATACLASS headers = new DATACLASS("Name", "Launch Date", "Status", "Mass", "Diameter", "Perigee", "Apogee");
  int textWidth = headers.printObject(initialXPosition, initialYPositionForOutput);                                // whether or not apogee is visible on the screen -- Manuel
  if (initialXPosition + textWidth < 1500)
    isApogeeVisible = true;

  // Faith: fixed length undefined error by changing to .size()
  for (int i = 0; i<dataToPrint.size(); i++)
  {
    DATACLASS temp = dataToPrint.get(i);

    if (localYPos < CENTRAL_LINE - 50 && localYPos > 50)
      temp.printObject(initialXPosition, localYPos);

    fill(255);
    textMode(LEFT);
    localYPos += 15;
  }

  return;
}


//henry adding query functionality 3/30/2022

ArrayList<DATACLASS> queryData(ArrayList<DATACLASS> data, String field, String text)
{

  ArrayList<DATACLASS> queryReturn = new ArrayList<DATACLASS>();

  initialYPositionForOutput = 30;
  currentYPositionTable = initialYPositionForOutput + 30;

  switch(field)
  {
  case "name":

    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).name.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  case "launchDate": //Needs to be dealt with seperately

    return queryReturn;

  case "status":
    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).status.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  case "mass":
    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).mass.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  case "diameter":
    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).diameter.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  case "perigee":
    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).perigee.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  case "apogee":
    for (int i = 0; i< data.size(); i++)
    {
      if (data.get(i).apogee.toUpperCase().contains(text.toUpperCase())) queryReturn.add(data.get(i));
    }
    return queryReturn;

  default:
    return queryReturn;
  }
}

//Henry: function to set all strokes to false 4/7

void setSelectionButtonStrokesOff() {

  button1.setStroke(false);
  button2.setStroke(false);
  button3.setStroke(false);
  button4.setStroke(false);
  button5.setStroke(false);
  button6.setStroke(false);
  textBox1.setStroke(false);
}

//Henry: function to set all selects to false 4/12
void setButtonSelectsOff(){
  
  button1.selected = false;
  button2.selected = false;
  button3.selected = false;
  button4.selected = false;
  button5.selected = false;
  button6.selected = false;
}

// Function to scroll down the list, both up and down - 20/03/22 - Manuel
void mouseWheel(MouseEvent event)
{
  if (event.getCount() > 0)
    currentYPositionTable -= 15;
  else if (event.getCount() < 0)
    currentYPositionTable += 15;
}


//Henry - 23/03/22 Star display, move and initialise functions






void mousePressed() {
  int event;

  event = activeScreen.getEvent(mouseX, mouseY);
  
  //Henry 4/12
  if(event >= NAME_QUERY_BUTTON && event <= DIAMETER_QUERY_BUTTON){
    setButtonSelectsOff();
  }
  
  switch(event) {
    
  case MAIN_BUTTON:
    println("button 1!");
    if(!screenTransition){
      screenTransition = true;
      clickEffect.play();
      //spinMultiplier = 0.2;
    }
    break;

  case QUERY_TEXTBOX:
    println("button 2!");
    textBox1.setSelect(true);
    clickEffect.play();
    break;


  case NAME_QUERY_BUTTON:
    button1.selected = true;
    queryField = "name";
    theBarChart = new BarChart(activeQuery, queryField);
    clickEffect.play();
    break;

  case STATUS_QUERY_BUTTON:
    button2.selected = true;
    queryField = "status";
    clickEffect.play();
    break;

  case APOGEE_QUERY_BUTTON:
    button6.selected = true;
    queryField = "apogee";
    clickEffect.play();
    break;

  case PERIGEE_QUERY_BUTTON:
    button5.selected = true;
    queryField = "perigee";
    clickEffect.play();
    break;

  case DIAMETER_QUERY_BUTTON:
    button4.selected = true;
    queryField = "diameter";
    clickEffect.play();
    break;

  case MASS_QUERY_BUTTON:
    button3.selected = true;
    queryField = "mass";
    clickEffect.play();
    break;
    
    //Meng: add select boxes
case EVENT_BUTTON1:
        sortType = "date";
        widget1.widgetColor = color(10);
        widget2.widgetColor = color(255);
        widget3.widgetColor = color(255);
        widget4.widgetColor = color(255);
        widget5.widgetColor = color(255);
        queryScreen.chart = new BarChart(activeQuery, "date");
        break;
      case EVENT_BUTTON2:
        sortType = "mass";
        widget2.widgetColor = color(10);
        widget1.widgetColor = color(255);
        widget3.widgetColor = color(255);
        widget4.widgetColor = color(255);
        widget5.widgetColor = color(255);
        queryScreen.chart = new BarChart(activeQuery, "mass");
        break;
      case EVENT_BUTTON3:
        sortType = "diameter";
        widget3.widgetColor = color(10);
        widget1.widgetColor = color(255);
        widget2.widgetColor = color(255);
        widget4.widgetColor = color(255);
        widget5.widgetColor = color(255);
        queryScreen.chart = new BarChart(activeQuery, "diameter");
        break;
      case EVENT_BUTTON4:
        sortType = "perigee";
        widget4.widgetColor = color(10);
        widget1.widgetColor = color(255);
        widget2.widgetColor = color(255);
        widget3.widgetColor = color(255);
        widget5.widgetColor = color(255);
        queryScreen.chart = new BarChart(activeQuery, "perigee");
        break;
      case EVENT_BUTTON5:
        sortType = "apogee";
        widget5.widgetColor = color(10);
        widget1.widgetColor = color(255);
        widget2.widgetColor = color(255);
        widget3.widgetColor = color(255);
        widget4.widgetColor = color(255);
        queryScreen.chart = new BarChart(activeQuery, "apogee");
        break;
  
  }
     }
  

//Henry: mousemoved function to change border colors
void mouseMoved() {

  int event;

  event = activeScreen.getEvent(mouseX, mouseY);
  switch(event) {
  case MAIN_BUTTON:
    //println("button 1!");
    mainButton1.setStroke(true);
    //spinMultiplier = 0.2;
    break;

  case QUERY_TEXTBOX:
    textBox1.setStroke(true);
    break;


  case NAME_QUERY_BUTTON:
    button1.setStroke(true);
    break;

  case STATUS_QUERY_BUTTON:
    button2.setStroke(true);
    break;

  case APOGEE_QUERY_BUTTON:
    button6.setStroke(true);
    break;

  case PERIGEE_QUERY_BUTTON:
    button5.setStroke(true);
    break;

  case DIAMETER_QUERY_BUTTON:
    button4.setStroke(true);
    break;

  case MASS_QUERY_BUTTON:
    button3.setStroke(true);
    break;

  default:
    mainButton1.setStroke(false);
    setSelectionButtonStrokesOff();
  }
}

void keyPressed() {

  int event;

  event = activeScreen.getEvent(mouseX, mouseY);

  switch (event) {

  default:
    break;
  }
  //Henry: part to enable key presses for textbox
  if (activeScreen == queryScreen) {

    println("queryscreen");
    if (textBox1.selected) {
      textBox1.pressedKey(key, keyCode);
    }
  }

  if (key == CODED)                            // added this part to allow the user to scroll left and right to observe the entire graph - Manuel - 31/03/22
  {

    if (keyCode == 39)                                                      // code for right and left arrow -- Manuel
    {
      println(queryScreen.chart.initialIndex);
      if (queryScreen.chart.initial_x > LEFT_LINE + 140)
      {
        if (queryScreen.chart.initialIndex >= 1)                              // prevents the fact that index -1 is accessed
        {
          queryScreen.chart.y_array[queryScreen.chart.initialIndex - 1] = queryScreen.chart.initial_y;
          queryScreen.chart.initialIndex -= 1;
          queryScreen.chart.initial_x -= 110;
        }
      }

      if (queryScreen.chart.initialIndex > 0 || queryScreen.chart.initial_x < LEFT_LINE + 140)
        queryScreen.chart.updateInitialX(5);
    } else if (keyCode == 37)
    {
      if (queryScreen.chart.initial_x + queryScreen.chart.BarWidth < LEFT_LINE + 100)
      {
        queryScreen.chart.initialIndex += 1;
        queryScreen.chart.initial_x += 110;
      }
      if (queryScreen.chart.initialIndex < 1000)
        queryScreen.chart.updateInitialX(-5);
    } else if (keyCode == 13)
      userPressesReturn = true;
  }



  return;
}

void mouseDragged()
{
  mySlider.move();                                                                      // function to move slider -- Manuel
}

//Meng: added the text for the select boxes
void displaySelectBoxText(){
  textFont(outputFont);
  fill(255);
  rectMode(CORNER);
  textAlign(LEFT);
  text("view the bar chart", 13, 255);
  text("by: ", 13, 275);
  text("date", 90, 313);
  text("mass", 90, 343); 
  text("diameter", 90, 373);
  text("perigee", 90, 403); 
  text("apogee", 90, 433);


}
