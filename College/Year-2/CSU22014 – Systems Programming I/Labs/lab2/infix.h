#ifndef __INFIX_H__
#define __INFIX_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

#include "stack.h"
#include "postfix.h"


double evaluate_infix_expression(char ** infix, int nargs);
int operatorPrecedence(char * args);
bool top_op_equal_or_higher_precedence(struct double_stack * stack, char * op, char ** args);
bool is_operator(char * token);
int operatorAssign (char * sym);

#endif
