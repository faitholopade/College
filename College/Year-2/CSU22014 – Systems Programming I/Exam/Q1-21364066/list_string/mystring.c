//   mystring.c
//   David Gregg
//   December 2022

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "mystring.h"

// (a) create a new empty list string
struct mystring * new_empty_mystring(){
	//create new list string
	struct mystring * list_string = malloc(sizeof(struct mystring));
	struct ls_node * head = malloc(sizeof(struct ls_node));
	//set head to null
	head -> next = NULL;
	//set head of list string to node we just created
    list_string -> head = head;
    //return list string
    return list_string;
}

// (b) create a new list string with the same contents as a normal
// NULL-terminated C string
struct mystring * new_array2mystring(char * text) {
	//create new empty string
	struct mystring * new_string = new_empty_mystring();
	//current node pointing to head of string
	struct ls_node * curr_node = new_string -> head;

	//iterate over each character in text
	for(int i = 0; i < strlen(text); i++){
		//new node for each character
		struct ls_node * char_node = malloc(sizeof(struct ls_node));
		//set new node's character to character in text
		char_node -> c = text[i];
		//set next node to null until we get to next character
		char_node -> next = NULL;

		 //if current node is null set head to new node
        if(curr_node == NULL){
            new_string -> head = char_node;
            curr_node = char_node;
        }
        
        //otherwise string is not empty so set next node to new node
        else{
            curr_node -> next = char_node;
            curr_node = char_node;
        }
	}

	//return new string
	return new_string;
}

// (c) create a new mystring that is a copy of str
struct mystring * mystring_copy(struct mystring * str) {
	//create new empty string that will be copy of str
	struct mystring * cpy_string = new_empty_mystring();
	//current node pointing to head of string
	struct ls_node * curr_node = str -> head;

	//iterate over each chracter in str
	while(curr_node != NULL){
		//new node for each character
		struct ls_node * char_node = malloc(sizeof(struct ls_node));
		//set new node's character to node's character in str
		char_node -> c = curr_node -> c;
		//set next node to null until we get to next character
		char_node -> next = NULL;

		//if string is empty set head to new node
		if(cpy_string -> head == NULL){
			cpy_string -> head = char_node;
			curr_node = char_node;
		}
		
		//otherwise string is not empty so set next node to new node
		else{
			curr_node -> next = char_node;
			curr_node = char_node;
		}
	}

	//return copy of string
	return cpy_string;
}

// (d) create a new mystring that is the concatenation of two
// mystrings
struct mystring * mystring_concatenate(struct mystring * str1, struct mystring * str2) {
	//create new string with contents of str1 
	struct mystring * concat_string = mystring_copy(str1);
	//current node pointing to head of new string
	struct ls_node * curr_node = concat_string  -> head;

	//iterate over each character in new string until we get to end of string 
	while(curr_node -> next != NULL){
		curr_node = curr_node -> next;
	}

	//once at the end of the string we set next pointer of last character
	//to first character of copy of str2
	curr_node -> next = mystring_copy(str2) -> head;

	//return concatenated string
	return concat_string;
}

// (e) create a new mystring containing the same characters as str,
// but in reverse order
struct mystring * mystring_reverse(struct mystring * str) {
	//create new empty string to store reversed string
	struct mystring * rev_str = new_empty_mystring();
	//current node pointing to first characters node of str
	struct ls_node * curr_node = str -> head;

	//iterate over each character in str using node
	while(curr_node != NULL){
		//create new node for each character
		struct ls_node * char_node = malloc(sizeof(struct ls_node));
		//set new node's character to current node's character in str
		char_node -> c = curr_node -> c;
		//add new node to front of reversed string
		char_node -> next = rev_str -> head;
		//set head of new string to new node
		rev_str -> head = char_node;
		//progress to next character in str
		curr_node = curr_node -> next;
	}

	//return reversed string
	return rev_str;
}

// is there a match starting at this point in the text
int match_substring(struct ls_node * text, struct ls_node * str) {
	//iterate over each character in str
	while(str != NULL){
		//if we reach the end of text before we reach end of str
		//then str can't be substring of text so return 0
		if(text == NULL){
			return 0;
		}
		//if characters don't match then return 0
		if(text -> c != str -> c){
			return 0;
		}
		//otherwise we have not reached end of text 
		//and characters match so progress to next character in text and in str
		text = text -> next;
		str = str -> next;
	}
	//if we reach this point then all characters in str have been matched so we return 1
	return 1;
}

// (f) return 1 if str is a substring of text; 0 otherwise
int mystring_substring(struct mystring * text, struct mystring * str) {
	//current node pointing to first characters node of text
	struct ls_node * curr_node = text -> head;

	//iterate over each character in text using node 
	while(curr_node != NULL){
		//if there is a match return 1 
		if(match_substring(curr_node, str -> head)){
			return 1;
		}
		//otherwise progress to next character in text	
		curr_node = curr_node -> next;
	}

	//end of text reached and no match found so return 0
	return 0;
}

// (g) free the memory used by the mystring
void mystring_free(struct mystring * str) {
	//current node pointing to first characters node of str
	struct ls_node * curr_node = str -> head;

	//iterate over each character in str
	do{
		//store next node so we can access it after freeing current node
		struct ls_node * next_node = curr_node -> next;
		//for each character in str free memory used by its node 
		free(curr_node);
		//progress to next characters node
		curr_node = next_node;
	} while (curr_node != NULL);

	//once all nodes freed, free memory used by str
	free(str);
}
