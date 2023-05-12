//we want to write bits 1 bit at a time 
//we will always need byte we are writing to

//can't write bit to file directly so we write each bit to a byte and when byte is full we then write that to file 

struct bitfile{
    FILE * file;
    unsigned char buffer;
    int count;
    char mode; //tells us whether to read or write
}

struct bitfile * open2write(char * filename){
    struct bitfile * result = malloc(sizeof(bitfile));
    result -> file = fopen(filename, "w"); //w for writing
    result -> count = 0;
    result -> buffer = 0;
    mode = 'w';
}

void bitfile_write(struct bitfile * this, int bit){
    //buffer must not be full
    this -> buffer = this -> buffer | bit << count; //store bit in buffer
    this -> count++;
    //if buffer is full flush it to the file (output ot file)
    if(this -> count == 8){
        fputc(this -> buffer, this -> file);
        this -> count = 0;
        this -> buffer = 0;
    }
}