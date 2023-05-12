//   list_string.c
//   David Gregg
//   January 2021

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "deque.h"

// create a new empty deque
struct deque * new_deque() {
    //create a new deque
    struct deque * new = malloc(sizeof(struct deque));
    new->elements = NULL;
    new->size = 0;
    new->nelements = 0;
    return new;
}

// push a string to the front of the deque
void push_front_deque(struct deque * this, char * str) {
    //check if the deque is empty
    if (this->nelements == 0) {
        //if it is, create a new array of size 1
        this->elements = malloc(sizeof(char *));
        this->size = 1;
        this->elements[0] = str;
        this->nelements++;
    } else {
        //if it isn't, check if the array is full
        if (this->nelements == this->size) {
            //if it is, create a new array of size 2x the old one
            char ** new = malloc(sizeof(char *) * this->size * 2);
            //copy the old array into the new one
            for (int i = 0; i < this->size; i++) {
                new[i] = this->elements[i];
            }
            //free the old array
            free(this->elements);
            //set the old array to the new one
            this->elements = new;
            this->size = this->size * 2;
        }
        //shift all the elements in the array to the right
        for (int i = this->nelements; i > 0; i--) {
            this->elements[i] = this->elements[i - 1];
        }
        //add the new string to the front of the array
        this->elements[0] = str;
        this->nelements++;
    }
}

// pop a string from the front of the deque
char * pop_front_deque(struct deque * this) {
    //check if the deque is empty
    if (this->nelements == 0) {
        //if it is, return an error
        return "ERROR";
    } else {
        //if it isn't, save the first element in the array
        char * first = this->elements[0];
        //shift all the elements in the array to the left
        for (int i = 0; i < this->nelements - 1; i++) {
            this->elements[i] = this->elements[i + 1];
        }
        this->nelements--;
        //return the first element
        return first;
    }
}

// push a string to the back of the deque
void push_back_deque(struct deque * this, char * str) {
    //check if the deque is empty
    if (this->nelements == 0) {
        //if it is, create a new array of size 1
        this->elements = malloc(sizeof(char *));
        this->size = 1;
        this->elements[0] = str;
        this->nelements++;
    } else {
        //if it isn't, check if the array is full
        if (this->nelements == this->size) {
            //if it is, create a new array of size 2x the old one
            char ** new = malloc(sizeof(char *) * this->size * 2);
            //copy the old array into the new one
            for (int i = 0; i < this->size; i++) {
                new[i] = this->elements[i];
            }
            //free the old array
            free(this->elements);
            //set the old array to the new one
            this->elements = new;
            this->size = this->size * 2;
        }
        //add the new string to the back of the array
        this->elements[this->nelements] = str;
        this->nelements++;
    }
}

// pop a value from the back of the deque
char * pop_back_deque(struct deque * this) {
    //check if the deque is empty
    if (this->nelements == 0) {
        //if it is, return an error
        return "ERROR";
    } else {
        //if it isn't, save the last element in the array
        char * last = this->elements[this->nelements - 1];
        this->nelements--;
        //return the last element
        return last;
    }
}

// free the memory used by the deque
void free_deque(struct deque * this) {
    //free the array
    free(this->elements);
    //free the deque
    free(this);
}

// return a string from the deque at position "index", where the
// item at the front of the deque has index 0, and subsequent
// items are numbered successively. If there are fewer than index+1
// items in the deque, return a string containing the value "ERROR"
char * item_at_deque(struct deque * this, int index) {
    //check if the index is out of bounds
    if (index >= this->nelements) {
        //if it is, return an error
        return "ERROR";
    } else {
        //if it isn't, return the string at the index
        return this->elements[index];
    }
}



