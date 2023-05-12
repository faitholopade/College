#include "infix.h"

int sym(char *symbol);

// evaluate expression stored as an array of string tokens
double evaluate_infix_expression(char ** args, int nargs) {
    struct double_stack * stack = double_stack_new(nargs);
    int postfix_index = 0;
    char ** postfix = malloc(sizeof(char *) * nargs);
    
    for(int i = 0; i < nargs; i++){
    //print current
      int symbol = operatorAssign(args[i]);
      switch (symbol) {
      case 0:
          postfix[postfix_index] = args[i];
          postfix_index++;
          break;
      case 1:
      case 2:
      case 3:
      case 4:
      case 5:
          while(top_op_equal_or_higher_precedence(stack, args[i], args) && (stack -> top) > 0){
            int op = double_stack_pop(stack);
            postfix[postfix_index] = args[op];
            postfix_index++;  
          }
          double_stack_push(stack, (double)i);
          break;
      case 6: 
          double_stack_push(stack, i); 
          break;
      case 7: 
          while(*args[(int)stack -> items[stack -> top - 1]] != '('){
            int op = double_stack_pop(stack);
            postfix[postfix_index] = args[op];
            postfix_index++;            
          }
          double_stack_pop(stack);
          break;
      default:
          break;
      }
   }
    while(stack -> top > 0){
      int stackItem = double_stack_pop(stack);
      postfix[postfix_index] = args[stackItem];
      postfix_index++; 
    }
    double postfixCalc = evaluate_postfix_expression(postfix, postfix_index);
    return postfixCalc;
}

//bool is_operator(char * token) {
  //  if ((*token == '+' || *token == '-' || *token == 'X' || *token == '/' || *token == '^' )){
    //  return true;
   // }
    //return false;
//}

int operatorPrecedence(char * args){
  int prec = 0;
  switch(*args){
    case '+':
      prec = 1;
      break;
    case '-':
      prec = 1;
      break;
    case 'X':
      prec = 2;
      break;
    case '/':
      prec = 2;
      break;
    case '^':
      prec = 3;
      break;
  }
  return prec;
}

bool top_op_equal_or_higher_precedence(struct double_stack * stack, char * op, char ** args){
  if(operatorPrecedence(args[(int)stack -> items[stack -> top - 1]]) >= operatorPrecedence(op)){
      return true;
  }
  return false;
}

int operatorAssign(char *symbol) {
  if (strcmp(symbol, "+") == 0) {
    return 1;
  } else if (strcmp(symbol, "-") == 0) {
      return 2;
  } else if (strcmp(symbol, "X") == 0) {
      return 3;
  } else if (strcmp(symbol, "/") == 0) {
      return 4;
  } else if (strcmp(symbol, "^") == 0) {
      return 5;
  } else if (strcmp(symbol, "(") == 0) {
      return 6;
  } else if (strcmp(symbol, ")") == 0) {
      return 7;
  } else {
      return 0;
  }
}


