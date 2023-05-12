// code for a huffman coder


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include "huff.h"

// create a new huffcoder structure
struct huffcoder * huffcoder_new(){
    struct huffcoder *result = malloc(sizeof(struct huffcoder));
    for (int i = 0; i < NUM_CHARS; i++) {
        result->freqs[i] = 0;
        result->code_lengths[i] = 0;
        result->codes[i] = 0;
    }
    result->tree = malloc(sizeof(struct huffchar) * NUM_CHARS);
    result->tree->freq = 1;
    result->tree->is_compound = 0;
    result->tree->seqno = 0;
    result->tree->u.c = '\0';
    result->tree->u.compound.left = (struct huffchar *)malloc(sizeof(struct huffchar));
    result->tree->u.compound.right = (struct huffchar *)malloc(sizeof(struct huffchar));

    return result;
}

// count the frequency of characters in a file; set chars with zero
// frequency to one
void huffcoder_count(struct huffcoder *this, char *filename){
 FILE *file = fopen(filename, "r");
    unsigned char c;
    while (!feof(file)) {
        c = fgetc(file);
        this->freqs[(int)c]++;
    }
    //set zero frequencies to 1
    for (int i = 0; i < NUM_CHARS; i++) {
        if (this->freqs[i] == 0)
            this->freqs[i] = 1;
    }
    fclose(file); 
}


//helper functions for huffcoder_build_tree
struct huffchar * create_simple_char(char c, int freq){
    struct huffchar * new_char;
    new_char = malloc(sizeof(struct huffchar));
    new_char -> freq = freq;
    new_char -> u.c = c;
    new_char -> seqno = (unsigned) c; //used to break ties between two character that are equally frequent 
    //for simple character seqno is ascii val of char
    //for compound character seqno is order in which compound char created 
    new_char -> is_compound = 0;

    return new_char;
}

struct huffchar * create_compound_char(struct huffchar *left, struct huffchar *right, 
                        int freq, int seqno){
    struct huffchar * new_char;
    new_char = malloc(sizeof(struct huffchar));
    new_char -> freq = freq;
    new_char -> is_compoud = 1;
    new_char -> seqno = seqno;
    new_char -> u.compound.left = left;
    new_char -> u.compound.right = right;
}
// using the character frequencies build the tree of compound
// and simple characters that are used to compute the Huffman codes
void huffcoder_build_tree(struct huffcoder *this, char *filename){
    struct huffchar * chars[MAX_CHARS]; //array of chars
    int seqno = MAX_CHARS;
    for (int i = 0; i < MAX_CHARS; i++)
    {
        chars[i] = create_simple_char(i, this -> freqs[i]);                       //initialize all positions with a simple character

    }
    int index;
    for (int i = 0; i < MAX_CHARS - 1; i++)
    {
        index = find_least_frequent(chars); //find least frequent and one with next smallest 
        struct huffchar * smallest = chars[index];
        chars[index] = NULL;
        int index2 = find_least_frequent(chars);
        struct huffchar * next_smallest = chars[index2];
        char[index2] = NULL; 
        struct huffchar * compound;
        compound = create_compound_char(smallest-> freq + next_smallest->freq, seqno , smallest, next_smallest);
        seqno++;
        chars[index] = compound; //we took two simple characters out and put their compound into array
        //so when we get to the end there should e one super compound left (i.e. root of tree)
    }


    this -> tree = char[index]; //setting tree root
}

//helper for huffcoder_tree2table
void tree2table_recursive(struct huffcoder * this,struct huffchar * node, char *path, int depth){
    if(node-> is_compound == 0){ //if not a compoond node
        //if it's a simple character fill in the table entry for this char
        int char_index = (unsigned char)node -> u.c;
        this -> code_length[char_index] =  depth; //how many 0's and 1's for this character
        char * new_string = malloc(sizeof(char) * (depth + 1)); //+1 for null in string 
        //copy path to thiis node into new string 
        path[depth] = '\0'; //add null character to the end of path 
        strcpy(new_string, path);
    }
    else{//node is a compound 
      path[depth] = '0';
      tree2table_recursive(this, node -> u.compound.left, path, depth+1);
      path[depth] = '1';
      tree2table_recursive(this, node -> u.compound.rigth, path, depth+1);
    }
}
// using the Huffman tree, build a table of the Huffman codes
// with the huffcoder object
//array path can only have max length = height of tree
//height of tree restricted tonumber of characters in tree
void huffcoder_tree2table(struct huffcoder *this){
    char path[NUM_CHARS];
    //once we have this path we can start to walk the tree 
    tree2table_recursive(this -> root, path, 0);

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

// decode the input file and write the decoding to the output file
void huffcoder_decode(struct huffcoder * this, char * input_filename,
    char * output_filename)
{
}


