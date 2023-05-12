//adding floating point numbers
float pairwise_sum(float * a, int size){
    float * partials = malloc(sizeof(float) * size);
    int size_of_partials = size;
    while (size_of_partials > 1){
        for(int i = 0; i < size_of_partials - 1; i+= 2){
            partials[i/2] = partials[i] + partials[i + 1];
        }
        if(size_of_partials % 2 == 1){
            partials[(size_of_partials/2) + 1] = partials[size_of_partials - 1];
        }
        size_of_partials = size_of_partials/2;
    }
}

float pairwise_sum_rec(float * a, int sum){ //keep splitting array until we have 1 or two elements in sub part, if 2 we add together if 1 we pass to next level 


}