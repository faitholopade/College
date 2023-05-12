unsigned char * compress(char * input, int * result_length){
    int in_length = strlen(input);
    int out_bytes = (in_length/4) + ((in_length % 4) = 0); // number of bytes we need
    unsigned char * output = malloc(sizeof(unsigned char) * out_bytes);
    for(int i = 0; i < in_length; i+){
        int encoding = char2encoding(input[i]); // we now have a 2 bit encoding 
        int byte_position = i % 4; // tells us which byte of the array we are looking in
        int output_position = i/4; //position in the output array 
        //clear target bits
        output[output_position] &= !(3 << (byte_position * 2));
        //insert new bits
        output[output_position] |= encoding << (byte_position * 2);
    } 
    *result_length = in_length;
    return output;
}