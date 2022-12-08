// Faith: added a constants tab to hold constants that need to be used across multiple classes and files 20/03/2022

// table object with data
Table table;

// screen size 
final int SCREENX = 1200;
final int SCREENY = 700;

// starfield
float spinMultiplier = 1;
boolean screenTransition = false;
int millsSinceSpinMaxReached = 0;

// bar chart
final int BAR_MAX_SIZE = 400;
String sortType = "perigee";


// sort 
final int DROP_DOWN = 0; // 0 for now
final int SORT_MAIN_ALPHA = 0;
final int SORT_BY_MASS = 0;
final int SORT_BY_DIAMETER = 0;
final int SEARCH_DROP_DOWN = 0;
final int QUERY_BOUNDING_BOX_WIDTH = 200;

final int CENTRAL_LINE = SCREENY / 2;
final int LEFT_LINE = 100;
final int RIGHT_LINE = SCREENX - 100;


//Henry: adding consts for query system

final int EVENT_NULL = -1;
final int MAIN_BUTTON = 0;
final int NAME_QUERY_BUTTON = 1;
final int STATUS_QUERY_BUTTON = 2;
final int MASS_QUERY_BUTTON = 3;
final int PERIGEE_QUERY_BUTTON = 4;
final int APOGEE_QUERY_BUTTON = 5;
final int DIAMETER_QUERY_BUTTON = 6;

final int QUERY_TEXTBOX = 7;


final int EVENT_BUTTON1 = 10;
final int EVENT_BUTTON2 = 11;
final int EVENT_BUTTON3 = 12;
final int EVENT_BUTTON4 = 13;
final int EVENT_BUTTON5 = 14;
