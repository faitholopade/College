// code for a huffman coder


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "huff.h"

// create a new huffcoder structure
struct huffcoder *  huffcoder_new()
{
}

// count the frequency of characters in a file; set chars with zero
// frequency to one
void huffcoder_count(struct huffcoder * this, char * filename)
{
  //initialize frequencies to zero
  for(int i =; i < NUM_CHARS; i++){
    this -> freqs[i] = 0;
  }

  //open the file
  FILE * file = fopen(filename, "r");
  unsigned char c;
  c = fgetc(file); //get char from file

  while(!feof(file)){ //checking if at end of file before reading from file 
    this -> freqs[c]++; //each char in c is an integer so we increment each char to get to the next one
    c = fgets(file); // by doing this we will be able to count how many characters in file 
  }

}

//huffcoder_build_tree HELPER FUNCTION

//create simple char 
struct huffchar * create_simple_char(char c, int freq){
  struct huffhcar * new_char; // creating new char
  new_char = malloc(sizeof(huffchar));
  new_char -> freq = freq; // frequency of char set to passed freq value 
  new_char -> u.c = c; //char c in union set to c passed into function
  new_char -> seqno = (unsigned) c;
  new_char -> is_compound = 0;

  return new_char;
}
//create compound char 
struct huffchar * create_compound_char(struct huffchar * left, struct huffchar * right, 
                                          int freq, int seqno){
  struct huffhcar * new_char; // creating new char
  new_char = malloc(sizeof(huffchar));
  new_char -> freq = freq; // frequency of char set to passed freq value 
  new_char -> seqno = seqno; //help differentiate between chars if they end up having same frequency
  new_char -> is_compound = 1;
  new_char -> u.compound.left = left; //char compounds in union set to compounds passed into function
  new_char -> u.compound.right = right; //.left and .right allow us access field within compound 
  return new_char;
}

// using the character frequencies build the tree of compound
// and simple characters that are used to compute the Huffman codes
void huffcoder_build_tree(struct huffcoder * this)
{
    struct huffchar * chars[MAX_CHARS]; // could do malloc also

    int seqno = MAX_CHARS;
    //initialize array with simple chars
    for(int i = 0; i < MAX_CHARS; i++){
        chars[i] = create_simple_char(i, this -> freqs[i]);
    }

    int index;
    for(int i = 0; i < MAX_CHARS - 1; i++){
        //find smallest char neded so we can create compound chars from least frequenct simple chars
        index = find_least_frequent(chars);
        struct huffchar * smallest = chars[index];
        chars[index] = NULL;

        //if we found least frequent look for next smallest (least frequent)
        int index2 = find_least_frequent(chars);
        struct huffchar *next_smallest = chars[index2];
        chars[index2] = NULL;

        //we then need to create  new compound char with these simple chars
        struct huffchar * compound;
        compound = create_compound_char(smallest -> freq + next_smallest _> freq, seqno, smallest, next_smallest); 
        seqno++;

        chars[index] = compound; //putting compound back into array so size goes down by 1 eqch time s 
        //we tke two things out and put back in 
    }

    //only root of tree left
    this -> tree = chars[index];
}


//huffcoder_tree2table HELPER FUNCTION
void huffcoder_tree2table_rec(struct huffcoder * this, struct huffchar * node, char * path, int depth){
  if(node -> is_compound == 0){ //simple char we want to fill in table entry for this char
    int char_index = (unsigned char) node -> u.c;
    this -> code_lengths[char_index] = depth; //length of encoidng is depth
    char * new_string = malloc(sizeof(char) * (depth + 2)); //string that contains chraracters
    path[depth] = '\0'; //need to put null at end of string we are copying or copying will go on forever 
    strcpy(new_string, path); //copy path to this node into new string 
  }
  else{ //compound character
    path[depth] = '0';
    huffcoder_tree2table_rec(this, node -> u.compound.left, path, depht);
    path[depth] = '1';
    huffcoder_tree2table_rec(this, node -> u.compound.right, path, depth);
  }
}
// using the Huffman tree, build a table of the Huffman codes
// with the huffcoder object
void huffcoder_tree2table(struct huffcoder * this)
{
    //given huffman code how would we compute codes for all characters 
    //we would need to walk the tree in some way (depth first search/ post order) 
    //we will do this walk recursively 
    //we will want to keep string of 0's adn 1's as we walk tree and we can use this for encoding to track path we followed to get to certain node
    char path [NUM_CHARS];
    //walk the tree
    huffcoder_tree2table_rec(this -> root, path, 0);
}


// print the Huffman codes for each character in order;
// you should not modify this function
void huffcoder_print_codes(struct huffcoder * this)
{
  for ( int i = 0; i < NUM_CHARS; i++ ) {
    // print the code
    printf("char: %d, freq: %d, code: %s\n", i, this->freqs[i], this->codes[i]);;
  }
}

// encode the input file and write the encoding to the output file
void huffcoder_encode(struct huffcoder * this, char * input_filename,
    char * output_filename)
{

}

//encode string to a file with huffcoder
void encode(char * str, struct huffcoder * huff, char *filename){
  //create bitfile to write to 
  struct bitfile * file = bitfile_open2write(filename);

  for(int i = 0; str[i] != NULL; i++){ //go through each char and encode
      unsigned char c = str[i]; //current char in string 
      char * code = huff -> codes[c]; //find out what code of char is 
      int length = huff -> code_lengths[i]; //code length
      for(int j = 0; j < length; j++){//go through each char in code
          int bit;
          if(code[j] = '0'){
            bit = 0;
          }
          else{
            bit = 1;
          }
          bitfile_write(file, bit); //call function to write each bit to file
      }
  }
}

// decode the input file and write the decoding to the output file
void huffcoder_decode(struct huffcoder * this, char * input_filename,
    char * output_filename)
{
}

//decode file to a string
void decode(char * dest, struct huffcoder * buff, char * filename){
      //create bitfile to write to 
  struct bitfile * file = bitfile_open2read(filename);
  int dest_counter = 0;

  int bit = bitfile_read(file);
  struct huffcoder * current = huff -> tree; //current node, based on bit we go left or right
  while(!bitfile_eof(file)){ //we decode by walking tree until we hit base of the tree, we get bit and either go left or right
    while(current -> compound == 1){ //as longas current is a compound node we keep going down tree
      if(bit = 0){
        //go left
        current = current -> u.compound.left;
      }
      else{
        //go right
        current = current -> u.compound.right;
      }
      bit = bitfile_read(file);
    }
    //the current character is a simple character at leaf of tree
    dest[dest_counter] = current -> u.c;
    //as we are at bottom of tree we need to go back to the top to decode the next character
    current = huff -> tree;
  } 

}
