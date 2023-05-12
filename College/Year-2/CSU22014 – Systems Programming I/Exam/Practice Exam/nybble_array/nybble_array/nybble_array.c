//   nybble_array.c
//   David Gregg
//   December 2021

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include "nybble_array.h"


// create a new array of nybbles with space for "size"
// nybbles and initialize the values of the array to zero
struct nybble_array * nybble_array_new(int size) {
  //create new array of nybbles
  struct nybble_array * result = malloc(sizeof(struct nybble_array));
  result->size_in_nybbles = size;
  result->size_in_bytes = (size + 1) / 2;
  //create new array of bytes
  result->data_bytes = malloc(result->size_in_bytes);
  //set all elements in byte array to 0
  for ( int i = 0; i < result->size_in_bytes; i++ ) {
    result->data_bytes[i] = 0;
  }
  return result;
}

// return the nybble value at position index
unsigned get_nybble_value(struct nybble_array * this, int index) {
  unsigned result = 0;
  //return upper four bits of byte if index is even
  if ( index % 2 == 0 ) { 
    result = this->data_bytes[index / 2] >> 4;
  } else { //return lower four bits of byte if index is odd
    result = this->data_bytes[index / 2] & 0x0F;
  }
  return result;
}

// set the nybble at position index to value
void set_nybble_value(struct nybble_array * this, int index, unsigned value) {
  if ( index % 2 == 0 ) { //set upper four bits of byte if index is even
    this->data_bytes[index / 2] = (this->data_bytes[index / 2] & 0x0f) | (value << 4);
  } else { //set lower four bits of byte if index is odd
    this->data_bytes[index / 2] = (this->data_bytes[index / 2] & 0xf0) | value;
  }
}

// free the memory used by a nybble array
void nybble_array_free(struct nybble_array * this) {
  //free byte array
  free(this->data_bytes);
  //free nybble array
  free(this);
}

// given an array of unsigned integers with values 0 to 15 create
// a new nybble_array with the same values
struct nybble_array * unsigned_to_nybble_array(unsigned * array, int size) {
  //create new nybble array
  struct nybble_array * result = nybble_array_new(size);
  //set each element in unsigned int array to corresponding position in nybble array
  for ( int i = 0; i < size; i++ ) {
    set_nybble_value(result, i, array[i]);
  }
  return result;
}

// given an array of nybbles, create a new array of unsigned integers
// with the same values
unsigned * nybble_array_to_unsigned(struct nybble_array * this) {
  //create new unsigned int array
  unsigned * result = malloc(this->size_in_nybbles * sizeof(unsigned));
  //get all elements in nybble array and set them to corresponding position in unsigned int array
  for ( int i = 0; i < this->size_in_nybbles; i++ ) {
    result[i] = get_nybble_value(this, i);
  }
  return result;
}

// print the raw byte content of the nybble array
void print_raw_bytes_nybble_array(struct nybble_array * this) {
  for ( int i = 0; i < this->size_in_bytes; i++ ) {
    printf("%x ", this->data_bytes[i]);
  }
  printf("\n");
}

