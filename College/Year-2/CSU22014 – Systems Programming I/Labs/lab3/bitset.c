#include "bitset.h"

// create a new, empty bit vector set with a universe of 'size' items
struct bitset * bitset_new(int size) {
	struct bitset * result = malloc(sizeof(struct bitset)); //result pointer to empty bitset
	result -> universe_size = size;
	result -> size_in_words = size / (sizeof(uint64_t) * 8); 
	if(size % 64 != 0) {
		result -> size_in_words++;
	}
	result -> bits = malloc(sizeof(uint64_t) * result -> size_in_words); // result -> bits = calloc(0, sizeof(uint64_t) * result -> size_in_words);
	for(int i = 0; i < result -> size_in_words; i++){
		result -> bits[i] = 0;
	}
	return result;
}

// get the size of the universe of items that could be stored in the set
int bitset_size(struct bitset * this) {
	return this -> universe_size;
}

// get the number of items that are stored in the set
int bitset_cardinality(struct bitset * this){
	int num_of_items = 0;
    for(int i = 0; i < this -> universe_size; i++){
    	if(bitset_lookup(this, i)){
    		num_of_items++;
    	}
    }
    return num_of_items;
}

// check to see if an item is in the set
int bitset_lookup(struct bitset * this, int item){
	int item_in_words = item / (sizeof(uint64_t) * 8);
    int item_len_in_bits = item % (sizeof(uint64_t) * 8);
    uint64_t mask = 1ULL << item_len_in_bits;
    uint64_t result = this -> bits[item_in_words] & mask;
    return result >> item_len_in_bits;
}

// add an item, with number 'item' to the set
// has no effect if the item is already in the set
void bitset_add(struct bitset * this, int item) {
	int item_in_words = item / (sizeof(uint64_t) * 8);
    int item_len_in_bits = item % (sizeof(uint64_t) * 8);
    uint64_t mask = 1ULL << item_len_in_bits;
    this -> bits[item_in_words] = this -> bits[item_in_words] | mask;
}

// remove an item with number 'item' from the set
void bitset_remove(struct bitset * this, int item) {
	int item_in_words = item / (sizeof(uint64_t) * 8);
    int item_len_in_bits = item % (sizeof(uint64_t) * 8);
    uint64_t mask = 1ULL << item_len_in_bits;
    this -> bits[item_in_words] = this -> bits[item_in_words] ^ mask;
}

// place the union of src1 and src2 into dest;
// all of src1, src2, and dest must have the same size universe
void bitset_union(struct bitset * dest, struct bitset * src1,
    struct bitset * src2) {
    if((dest -> universe_size == src1 -> universe_size) && (src2 -> universe_size == src1 -> universe_size)){
      for(int i = 0; i < dest -> size_in_words; i++){
          dest -> bits[i] = dest -> bits[i] | (src1 -> bits[i] | src2 -> bits[i]);
      }
    }
}

// place the intersection of src1 and src2 into dest
// all of src1, src2, and dest must have the same size universe
void bitset_intersect(struct bitset * dest, struct bitset * src1,
    struct bitset * src2) {
    if((dest -> universe_size == src1 -> universe_size) && (src2 -> universe_size == src1 -> universe_size)){
      for(int i = 0; i < dest -> size_in_words; i++){
          dest -> bits[i] = dest -> bits[i] | (src1 -> bits[i] & src2 -> bits[i]);
      }
    }
}
