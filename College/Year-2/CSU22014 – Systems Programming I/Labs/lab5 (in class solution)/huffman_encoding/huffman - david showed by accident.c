//david code shown by accident in recording
struct huffcoder *  huffcoder_new()
{
    struct huffcoder * result;
    int i;
    result = malloc(sizeof(struct huffcoder));
    result -> tree = NULL;
    for(i = 0; i < NUM_CHARS; i++){
      result -> freqs[i] = 0;
      result -> code_lengths[i] = 0;
      result -> codes[i] = malloc(sizeof(char) * (NUM_CHARS + 1));
    }
  return result;
}

// david code shown by accident in recording 
void huffcoder_count(struct huffcoder * this, char * filename)
{
    FILE * file;
    char c;
    int i;
    int nfreqs = NUM_CHARS;

    //initialize frequencies
    for(i = 0; i < nfreqs; i++){
      this -> freqs[i] = 0;
    }

    //open file
    file = fopen(filename, "r");
    assert(file != NULL);

    c = fgetc(file); //attempt to read a byte
    while(!feof(file)){
      //careful of C implementation with signed characters
      unsigned char tmp = (unsigned) c;
      this -> freqs[temp]++;
      c = fgetc(file);
    }
    fclose(file);

    //if any character does no appear set its freq to 1
    for(i = 0; i < nfreqs; i++){
      assert(this -> freqs[i] >= 0){
        if(this -> freqs[i] == 0){
          this -> freqs[i] = 1;
        }
      }
    }
}

//david code shown in recording 
//find two nodes in the list with smallest frequencies
void find_minima(struct huffchar **list, int length, int * idx1, int * idx2){
  int min1, min2, i;

  assert(length >= 2){
    //inital setup
    min1 = 0;
    min2 = 1;

    //ensure min1 <= min2
    if(list[min1] -> freq > list[min2] -> freq){
      int tmp = min1;
      min1 = min2;
      min2 = tmp;
    }

    //sequential search for minima
    //a binary heap would be faster but more complicated
    for(i = 2; i < length; i++){
      if(list[i] -> freqs < list[min] -> && )
    }
  }
}
// encode the input file and write the encoding to the output file
void huffcoder_encode(struct huffcoder * this, char * input_filename,
    char * output_filename)
{
    FILE * infile;
    FILE * outfile;

    const char EOT = 4;

    //open the files
    infile = fopen(input_filename, "r");
    outfile = fopen(output_filename, "w+");

    asser(infile != NULL);

    //encode each character of the input file 
    unsigned char c;
    c = fgetc(infile);
    while(!feof(infile)){
      fprint(outfile, "%s", this -> codes[c]);
      c = fgetc(infile);
    }
    fclose(infile);
}