// Faith: added slider class to be used for slider 30/03
class Slider {
  int sliderLength;
  float sliderPositionX;
  int sliderPositionY;
  float startPosition;
  int endPosition;
  int sliderWidth = 10; // values set to zero for now
  int handleWidth = 0;
  int dx;
  float lastXPosition;
  int initialXPosition;

// constructor
  Slider(int positionX, int positionY, int length) {
    sliderPositionX = (float)positionX;
    sliderPositionY = positionY;
    initialXPosition = positionX - 1;
    sliderLength = length-handleWidth;
    startPosition = sliderPositionX;
    endPosition = RIGHT_LINE;
    lastXPosition = positionX;
    dx = 0;
  }

  // draw(): Draws slider.
  void draw() {
    //println("slider draw!");
    fill(#FFFFFF);
    noStroke();
    rect(sliderPositionX, sliderPositionY, sliderLength+handleWidth, sliderWidth);   // Draw slider bar
    rect(startPosition, sliderPositionY, handleWidth, handleWidth);      // Draw handles
    //rect(endPosition, sliderPositionY, handleWidth, handleWidth);
  }

  // move(): Adjusts start and end handles of the slider, using mouse input.
  
  // Method to move the slider which is triggered by the mouseDragged() method. Initially written by Faith and modified by Manuel
  // Takes into account whether or not name and apogee are visible, which might limit the slider's movement and thus the movement of the table
  
  void move() {
    //Check if within slider range
    if (mouseX >= sliderPositionX && mouseX <= sliderPositionX + sliderLength && mouseY >= sliderPositionY && mouseY <= sliderPositionY + sliderWidth)
    {
      if (mouseX > lastXPosition + (sliderLength / 2) && sliderPositionX < endPosition && !isApogeeVisible)
      {
        dx = -4;
        sliderPositionX += 0.5;
        isNameVisible = false;
      }
      else if (mouseX < lastXPosition + (sliderLength / 2) && sliderPositionX > initialXPosition && !isNameVisible)
      {
        dx = 4;
        sliderPositionX -= 0.5;
        isApogeeVisible = false;
      }
      
      lastXPosition = sliderPositionX;
    }
            
      /*if (mouseX >= startPosition - handleWidth && mouseX <= startPosition+handleWidth*2)
      {
        if (mouseX > sliderPositionX)
          dx += 5;
        else if (mouseX < sliderPositionX)
          dx += -5;
        startPosition = constrain(mouseX, sliderPositionX, endPosition-handleWidth);       
      }
      
      else if (mouseX >= endPosition - handleWidth && mouseX <= endPosition+handleWidth*2)
      {
        dx = -1;
        endPosition = constrain(mouseX, startPosition+handleWidth, sliderPositionX+sliderLength);
      }
      
    } */
  }

  // getStart(): Returns value of the start handle along the slider.
  float getStart() {
    return ((float)startPosition-(float)sliderPositionX)/((float)sliderLength-(float)sliderPositionX);
  }

  // getEnd(): Returns value of the end handle along the slider.
  float getEnd() {
    return ((float)endPosition-(float)sliderPositionX)/((float)sliderLength-(float)sliderPositionX);
  }
}
