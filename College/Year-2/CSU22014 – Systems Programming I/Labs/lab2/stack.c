
#include "stack.h"

struct double_stack * double_stack_new(int max_size) {
	struct double_stack * result;
	
	//allocate space for stack header
	result = malloc(sizeof(struct double_stack));
	result -> max_size = max_size;
	result -> top = 0;
	//allocate space for data stores in stack
	result -> items = malloc(sizeof(double) * max_size);
	
	//return a pointer to the newly allocated stack
	return result;
}

// push a value onto the stack
void double_stack_push(struct double_stack * this, double value) {
	//assert(this -> top < this-> max_size);
	this -> items [this -> top] = value;
	this -> top++;
}

// pop a value from the stack
double double_stack_pop(struct double_stack * this) {
//	assert (this -> top > this -> 0);
	this -> top--;
	return this -> items [this -> top];
}


