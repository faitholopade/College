#include "postfix.h"


//** args is a character 
//nargs is number of elements in the stack
// evaluate expression stored as an array of string tokens
double evaluate_postfix_expression(char **args, int nargs) {
    struct double_stack *calcStack = double_stack_new(nargs);
    
    double num1;
    double num2;
    
    for (int i = 0; i < nargs; i++) {
        if (strcmp(args[i], "+") == 0) {
            num1 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num1);
            num2 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num2);
           // printf("adding %f and %f\n", num2, num1);
            double_stack_push(calcStack, num2 + num1);
        } else if (strcmp(args[i], "-") == 0) {
            num1 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num1);
            num2 = double_stack_pop(calcStack);
           // printf("pop: %f\n", num2);
           // printf("subtracting %f and %f \n", num2, num1);
            double_stack_push(calcStack, num2 - num1);
        } else if (strcmp(args[i], "X") == 0) {
            num1 = double_stack_pop(calcStack);
           // printf("pop: %f\n", num1);
            num2 = double_stack_pop(calcStack);
           // printf("pop: %f\n", num2);
            //printf("multiplying %f and %f\n", num2, num1);
            double_stack_push(calcStack, num2 * num1);
        } else if (strcmp(args[i], "/") == 0) {
            num1 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num1);
            num2 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num2);
            //printf("dividing %f and %f\n", num2, num1);
            double_stack_push(calcStack, num2 / num1);
        } else if (strcmp(args[i], "^") == 0) {
            num1 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num1);
            num2 = double_stack_pop(calcStack);
            //printf("pop: %f\n", num2);
            //printf("calculting %f to the power of %f\n", num2, num1);
            double_stack_push(calcStack, pow(num2, num1));
        } else {
            //printf("pushing value...\n");
            double doubleVal = atof(args[i]);
            //printf("push: %f\n", doubleVal);
            double_stack_push(calcStack, doubleVal);
        }
    }
    return double_stack_pop(calcStack);
}
