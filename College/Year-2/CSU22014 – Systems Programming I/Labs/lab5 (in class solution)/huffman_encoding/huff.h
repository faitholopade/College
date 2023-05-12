// header file for Huffman coder

#ifndef HUFF_H
#define HUFF_H

#define NUM_CHARS 256

// node in a Huffman tree is either a compound char (internal node)
// or a simple char (leaf)
struct huffchar { // can be simple or compound which is a pointer to other characters
  int freq; // how character shows 
  int is_compound; // is it compound or simple?
  int seqno; // numeric orginal ascii code of character 
  union { // can be one thing or another thing so we have struct compound or char c but not both
    struct {
      struct huffchar * left;
      struct huffchar * right;
    } compound;
    unsigned char c;
  } u;
};


struct huffcoder {
  int freqs[NUM_CHARS];
  int code_lengths[NUM_CHARS];
  char * codes[NUM_CHARS];
  struct huffchar * tree;
};

// create a new huffcoder structure
struct huffcoder *  huffcoder_new();

// count the frequency of characters in a file; set chars with zero
// frequency to one
void huffcoder_count(struct huffcoder * this, char * filename);

struct huffchar * create_simple_char(char c, int freq);
struct huffchar * create_compound_char(struct huffchar * left, struct huffchar * right, 
                                          int freq, int seqno);
// using the character frequencies build the tree of compound
// and simple characters that are used to compute the Huffman codes
void huffcoder_build_tree(struct huffcoder * this);

// using the Huffman tree, build a table of the Huffman codes
// with the huffcoder object
void huffcoder_tree2table(struct huffcoder * this);

// print the Huffman codes for each character in order
void huffcoder_print_codes(struct huffcoder * this);

// encode the input file and write the encoding to the output file
void huffcoder_encode(struct huffcoder * this, char * input_filename,
      char * output_filename);

// decode the input file and write the decoding to the output file
void huffcoder_decode(struct huffcoder * this, char * input_filename,
      char * output_filename);

#endif // HUFF_H
