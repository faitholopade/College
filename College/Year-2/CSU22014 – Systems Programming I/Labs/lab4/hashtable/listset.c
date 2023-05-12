#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// include the header files with the declarations of listset
#include "listset.h"

// create a new, empty linked list set
struct listset * listset_new() {
    struct listset * result = malloc(sizeof(struct listset));
	assert (result != NULL);
	result -> head = malloc(sizeof(struct listnode));
    result -> head = NULL;
    return result;
}

/* check to see if an item is in the set
   returns 1 if in the set, 0 if not */
int listset_lookup(struct listset * this, char * item) {
	for(struct listnode *p = this -> head; p != NULL; p = p -> next){
		if(strcmp(p -> str, item) == 0){
			return 1;
		}
	}
	return 0;
}

// add an item, with number 'item' to the set
// has no effect if the item is already in the set.
// New items that are not already in the set should
// be added to the start of the list
void listset_add(struct listset * this, char * item) {
	if(listset_lookup(this, item) == 0){ //ensuring item not in list yet
		struct listnode * newHead = malloc(sizeof(struct listnode));
		newHead -> next = this -> head;
		this -> head = newHead;
		newHead -> str = item;
	}
	// return; //item already in list
}

// remove an item with number 'item' from the set
void listset_remove(struct listset * this, char * item) {
	struct listnode *curr = this -> head;
	struct listnode *prev = this -> head;
	while(curr != NULL){
		if(strcmp(curr -> str, item) == 0){
			if(curr == this -> head){
				this -> head = this -> head -> next;
			}
			else{
				prev -> next = curr -> next;
			}
			free(curr);
			break;
		}
		prev = curr;
		curr = curr -> next;
	}
}
	// if(this -> head == NULL){
	// 	return;
	// }
	// if(listset_lookup(this, item) == 1){ //in list
	// 	if(strcmp(this -> head -> str, item) == 0){
	// 		struct listnode * prevNode = this -> head;
	// 		this -> head = prevNode -> next;
	// 		free(prevNode);
	// 		return;	
  	// 	}

	// 	for(struct listnode *p = this -> head; p -> next != NULL; p = p -> next){
	// 		if(strcmp(p -> next -> str, item) == 0){
	// 			struct listnode *prevNode = p -> next;
	// 			p -> next = prevNode -> next;
	// 			free(prevNode)
	// 		}
	// 	}
	// }
	// 	if(this -> head == NULL){
	// 		return; //null list
	// 	}
		
	// 	//struct listnode * p = malloc(sizeof(struct listnode));
	// 	//p = this -> head;
		
	// 	if(strcmp(item, this -> head -> str) == 0){ //item to be removed is head
	// 		struct listnode *t = this -> head;
	// 		this -> head = this -> t -> next;
	// 		free(t);
	// 		return;
	// 	}
		
	// 	//traversing to find prev and pointing it to next + 1 to delete node
	// 	for(struct listnode * p = this -> head; p != NULL; p = p -> next){
	// 		if(strcmp(item, p -> str) == 0){
	// 			struct listnode *t = p -> next;
	// 			p -> next = t -> next;
	// 			//p -> next = p -> next -> next; 
	// 			free(t); //p
	// 		}
	// 		return;
	// 	}
	// }
	// return;	

  
// place the union of src1 and src2 into dest
void listset_union(struct listset * dest, struct listset * src1,
		   struct listset * src2) { //adding nodes from both lists to destination
	
	
	for (struct listnode *p1 = src1 -> head; p1 != NULL; p1 = p1 -> next) {
        listset_add(dest, p1 -> str);
    }
    for (struct listnode * p2 = src2 -> head; p2 != NULL; p2 = p2 -> next) {
        listset_add(dest, p2 -> str);
    }
}

// place the intersection of src1 and src2 into dest
void listset_intersect(struct listset * dest, struct listset * src1,
		       struct listset * src2) { //adding node to dest only if in both
	for (struct listnode * p1 = src1 -> head; p1 != NULL; p1 = p1 -> next) {
        if (listset_lookup(src2, p1 -> str) == 1) {
            listset_add(dest, p1 -> str);
        }
    }
}

// return the number of items in the listset
int listset_cardinality(struct listset * this) {
	int cnt = 0;
	struct listnode * p = malloc(sizeof(struct listset));
	p = this -> head;
	while(p != NULL){
		p = p -> next;
		cnt ++;
	}
	return cnt;
}
	// if(this -> head == NULL){
	// 	return cnt;
	// }else{
	// 	for (p = this -> head; p != NULL; p = p -> next) {
	// 		cnt++;
	// 	}
	// }
// }

// print the elements of the list set
void listset_print(struct listset * this) {
  struct listnode * p;

  for ( p = this->head; p != NULL; p = p->next ) {
    printf("%s, ", p->str);
  }
  printf("\n");
}