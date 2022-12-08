//Manuel and Meng: created a BarChart class 24/03

class BarChart
{
  int chartWidth, chartHeight;
  int BarWidth, BarHeight;
  int x, initial_y, max_y, current_y, initial_x;
  int numberOfBars;
  String sortType;
  final color BarChartColor;
  int [] results;
  double []diameters;
  String [] resultsName;
  String [] resultsYear;
  int [] y_array;
  PFont BarChartOutputFont;
  int BarResize = 1;
  int initialIndex;


  //Meng: added the constrcutor
  BarChart(ArrayList<DATACLASS> result, String sortType)
  {
    if (result == null)
      print("lol");
    initial_y = SCREENY - 80;
    initial_x = LEFT_LINE + 135;
    initialIndex = 0;
    numberOfBars = result.size();
    results = new int [numberOfBars];
    diameters = new double [numberOfBars];
    resultsName = new String [numberOfBars];
    resultsYear = new String [numberOfBars];
    y_array = new int[numberOfBars];
    BarChartColor = color(0, 0, 200);
    BarWidth = 70;
    this.sortType = sortType;
    //BarChartOutputFont = loadFont("AppleSDGothicNeo-Thin-32.vlw");;
    for (int i = 0; i < result.size(); i++) {
      DATACLASS temp = result.get(i);
      resultsName[i] = temp.name;
      //allow the program to know what the user want to sort the barChart by
      if (sortType == "launchDate") {
        String date = temp.launchDate;
        char A1 = date.charAt(0);
        char A2 = date.charAt(1);
        char A3 = date.charAt(2);
        char A4 = date.charAt(3);
        String A1string = Character.toString(A1);
        String A2string = Character.toString(A2);
        String A3string = Character.toString(A3);
        String A4string = Character.toString(A4);
        String year = A1string + A2string + A3string + A4string;
        resultsYear [i] = year;
        int yearInt = Integer.parseInt(year);
        results[i] = yearInt;
      } else if (sortType == "mass") {
        int mass = Integer.parseInt(temp.mass);
        results[i] = mass;
      } else if (sortType == "diameter") {
        double diameter = Double.parseDouble(temp.diameter);
        diameters[i] = diameter;
      } else if (sortType == "perigee") {
        int perigee = Integer.parseInt(temp.perigee);
        results[i] = perigee;
      } else if (sortType == "apogee") {
        int apogee = Integer.parseInt(temp.apogee);
        results[i] = apogee;
      }
    }

    for (int index = 0; index < numberOfBars; index++)
    {
      y_array[index] = initial_y;
    }
  }

  // this function was planned to be used, but it was not used in the final version
  int calculateMaxValue() {
    int temp;
    int max = 0;
    for (int index = 0; index < numberOfBars; index++)
    {
      temp = results[index];
      if (temp >= max) {
        max = temp;
      }
    }
    println(max);
    return max;
  }

  void draw() {                                                      // function to draw the BarChart (written by Meng and Manuel)
    if (sortType != "diameter"){
      x = initial_x;
      stroke(255);
      rect(LEFT_LINE + 125, initial_y, 10000, 0);
      rect(LEFT_LINE +125, initial_y - 240, 0, 240);
      triangle(LEFT_LINE + 125, initial_y - 250, LEFT_LINE + 120, initial_y - 225, LEFT_LINE + 130, initial_y - 225);
      displayLargerText("Bar Chart (sorted by " + sortType + ")", (SCREENX / 2) - 150, 400);
      //displaySmallerText(sortType, 105, initial_y - 170);
      for (int i = initialIndex; i < numberOfBars; i++) {
        if (sortType == "launchDate"){
          displaySmallerText(resultsYear[i], x , 200);
        }
        BarHeight = (results[i] * (SCREENY / 2)) / 1500;
        max_y = initial_y - BarHeight;
        current_y = y_array[i];
        if (current_y >= max_y)
        {
          current_y -= 2;
          y_array[i] = current_y;
        }
        fill(BarChartColor);
        if (x >= LEFT_LINE + 125){
        if ((initial_y - BarHeight) >= CENTRAL_LINE)
        {
          rect(x, current_y, BarWidth, (initial_y - current_y));
          String resultsTemp = String.valueOf(results[i]);
          displaySmallerText(resultsTemp, x + 15, current_y - 15);
        } else if ((initial_y - BarHeight) < CENTRAL_LINE && current_y > CENTRAL_LINE)
        {
          rect(x, current_y, BarWidth, (initial_y - current_y));
          String resultsTemp = String.valueOf(results[i]);
          displaySmallerText(resultsTemp, x + 15, current_y + 15);
        } else
        {
          rect(x, CENTRAL_LINE, BarWidth, (initial_y - CENTRAL_LINE));
          String resultsTemp = String.valueOf(results[i]);
          displaySmallerText(resultsTemp, x + 15, CENTRAL_LINE + 15);
        }

        int textXPosition = (x + (BarWidth / 2)) - ((int)textWidth(resultsName[i]) / 2);      // allows for a central allignment of the names -- Manuel
        if (i % 2 == 0) {
          displaySmallerText(resultsName[i], textXPosition, initial_y + 25);
        } else {
          displaySmallerText(resultsName[i], textXPosition, initial_y + 50);
        }
        
        }
        x = x + 110;
        if (x >= RIGHT_LINE + 30)
        {
          y_array[i] = initial_y;
          break;
        }
      }
    }
    else
   {
      x = initial_x;
      stroke(255);
      rect(LEFT_LINE + 125, initial_y, 1000, 0);
      rect(LEFT_LINE +125, initial_y - 240, 0, 240);
      triangle(LEFT_LINE + 125, initial_y - 250, LEFT_LINE + 120, initial_y - 225, LEFT_LINE + 130, initial_y - 225);
      displayLargerText("Bar Chart (sorted by " + sortType + ")", (SCREENX / 2) - 150, 400);
      //displaySmallerText(sortType, 105, initial_y - 170);
      for (int i = initialIndex; i < numberOfBars; i++) {
        double BarHeightDouble = (diameters[i]*50);
        double max_yDouble = initial_y - BarHeightDouble;
        current_y = y_array[i];
        if (current_y >= max_yDouble)
        {
          current_y -= 2;
          y_array[i] = current_y;
        }
        fill(BarChartColor);
         if (x >= LEFT_LINE + 125){
          rect(x, current_y, BarWidth, (initial_y - current_y));
          String resultsTemp = String.valueOf(diameters[i]);
          displaySmallerText(resultsTemp, x + 15, current_y - 15);
          int textXPosition = (x + (BarWidth / 2)) - ((int)textWidth(resultsName[i]) / 2);
          if (i % 2 == 0) {
            displaySmallerText(resultsName[i], textXPosition, initial_y + 25);
          } else {
            displaySmallerText(resultsName[i], textXPosition, initial_y + 50);
          }
        }

        x = x + 110;
        if (x >= RIGHT_LINE + 30)
        {
          y_array[i] = initial_y;
          break;
        }
      }
   }
  
  }

  //Meng: wrote the function the display text
  void displaySmallerText(String text, int textx, int texty) {
    textFont(outputFont);
    color textcolor = color(250);
    fill (textcolor);
    text (text, textx, texty);
  }

  void displayLargerText(String text, int textx, int texty) {
    //textFont(BarChartOutputFont);
    color textcolor = color(250);
    fill (textcolor);
    text (text, textx, texty);
  }

  void updateInitialX(int value)
  {
    initial_x = initial_x + value;
  }
}
