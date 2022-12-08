//Meng:  Created a class to store the datapoints and print function 20/03/2022
// Faith: added get and set methods that may be useful later on 20/03/2022
class DATACLASS
{
  String name, launchDate, status, mass, diameter, perigee, apogee;

  DATACLASS(String n, String l, String s, String m, String d, String p, String a)
  {
    name = n;
    launchDate = l;
    status = s;
    mass = m;
    diameter = d;
    perigee = p;
    apogee = a;
  }

  int printObject(int xPosition, int yPosition)
  {
    String outputString = "";
    int textWidth = 0;

    for (int attribute = 0; attribute < 7; attribute++)    // for each attribute, the xPosition changes to allow for a table effect
    {
      switch (attribute)
      {
      case 0:
        outputString = getname();
        break;
      case 1:
        outputString = getLaunchDate();
        break;
      case 2:
        outputString = getStatus();
        break;
      case 3:
        outputString = getMass();
        break;
      case 4:
        outputString = getDiameter();
        break;
      case 5:
        outputString = getPerigee();
        break;
      case 6:
        outputString = getApogee();
      }
      fill(255);
      textAlign(LEFT);
      text(outputString, xPosition, yPosition);
      textWidth += textWidth(outputString);
      if (attribute == 0)                                      // differences in the pixels to add, to allow for "clean" space between each variable -- Manuel
      {
        textWidth += 300;
        xPosition += 300;
      }
      else if (attribute == 1)
      {
        textWidth += 200;
        xPosition += 200;
      }
      else
      {
        textWidth += 100;
        xPosition += 100;
      }
    }
    
    return textWidth;
    
    

    //println(outputString);
    //print(yPosition);     for testing
  }

  // set methods allowing you to assign values  to varia
  void setname(String name) {
    this.name = name;
  }
  void setLaunchDate(String launchDate) {
    this.launchDate = launchDate;
  }
  void setStatus (String status) {
    this.status = status;
  }
  void setMass(String mass) {
    this.mass = mass;
  }
  void setDiameter(String diameter) {
    this.diameter = diameter;
  }
  void setPerigee(String perigee) {
    this.perigee = perigee;
  }
  void setApogee(String apogee) {
    this.apogee = apogee;
  }

// get methods that allow you to retrieve current value of variable 
  String getname() {
    return name;
  }
  String getMass() {
    return mass;
  }
  String getDiameter() {
    return diameter;
  }
  String getStatus() {
    return status;
  }
  String getApogee() {
    return apogee;
  }
  String getPerigee() {
    return perigee;
  }
  String getLaunchDate() {
    return launchDate;
  }
}
