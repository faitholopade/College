// Task 2: Code modification for tracking register banks usage

#define __NWINDOW__ 8  // Macro for max number of banks allowed
int wused = 2;         // Number of windows currently in use (start with 2)
int overflows = 0;     // Counter for overflows
int underflows = 0;    // Counter for underflows
int depth = 0;         // Current call depth
int depthMax = 0;      // Current maximum depth reached
int count = 0;         // Counter for number of function calls

/* Handling register banks at function entry */
int fun_entry() {
    depth++;           // One more nested function call
    if (depth > depthMax)
        depthMax = depth;  // Updating depthMax
    if (wused == __NWINDOW__)  // If all the windows are used...
        overflows++;          // Then trigger overflow
    else
        wused++;              // Otherwise allocate one more window
    return 0;
}

/* Handling register banks at function exit */
int fun_exit() {
    depth--;           // One less nested function call
    if (wused == 2)    // If only 2 windows are currently allocated
        underflows++;  // Then trigger underflows
    else
        wused--;       // Otherwise deallocate one window
    return 0;
}

/* ack3way function modified to track register banks usage at entry and exit */
int ack3way(int m, int n, int p) {
    fun_entry();      // Modification at entry
    count++;
    int temp;
    if (p == 0) {
        fun_exit();
        return m + n;  // Possible exit
    }
    if (n == 0 && p == 1) {
        fun_exit();
        return 0;      // Possible exit
    }
    if (n == 0 && p == 2) {
        fun_exit();
        return 1;      // Possible exit
    }
    if (n == 0) {
        fun_exit();
        return m;      // Possible exit
    } else {
        temp = ack3way(m, n - 1, p);
        temp = ack3way(m, temp, p - 1);
        fun_exit();    // Possible exit
        return temp;
    }
}

/* Main function modified to track register banks usage at entry and exit */
int main() {
    fun_entry();
    ack3way(2, 3, 3);
    fun_exit();
    printf("Count: %d, Overflows: %d, Underflows: %d\n", count, overflows, underflows);
    return 0;
}
