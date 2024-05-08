%{
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct var {
    char * name;
    int value;
    bool set;
};

struct table {
    struct var * vars;
    int size;
};

struct table * table_new();
int table_get_attribute(struct table * t, char * name);
void table_set_attribute(struct table * t, int value, char * name);
void table_print(struct table * t);
void table_free(struct table * t);
int yylex();
void yyerror(char *s);

struct table * global_vars = NULL;
%}

/* Assign, Variable */ 
%token ASSIGN NUM
%token VAR
/* Operators */
%left ADD MINUS
%left MUL DIV
%right POW
/* Brackets */
%token OPEN_BRACKET CLOSE_BRACKET
/* End of Statement, End of Line */
%token EOS EOL
/* Print */
%token PRINT

%union {
    char * text;
    int intval;
}

%type<intval> expr NUM 
%type<text> VAR
%%
input: 
 | input statement EOL
 ; 

statement: 
 | statement VAR ASSIGN expr EOS { 
    table_set_attribute(global_vars, $4, $2); 
 }
 | statement PRINT expr EOS { printf("%d\n", $3); } 
 ;

expr: NUM
 | VAR { 
        
    int index = table_get_attribute(global_vars, $1);
    //printf("VAR TEST = %d\n", global_vars->vars[index].value);
    if (index != -1) {
        $$ = global_vars->vars[index].value;
    }
    else {
        yyerror("syntax error");
        return 0;
    }
 }
 | expr ADD expr { $$ = $1 + $3; }
 | expr MINUS expr { $$ = $1 - $3; }
 | MINUS expr %prec MINUS { $$ = -$2; }
 | expr MUL expr { $$ = $1 * $3; }
 | expr DIV expr { $$ = $1 / $3; }
 | OPEN_BRACKET expr CLOSE_BRACKET { $$ = $2; }
 ; 

%%

int main() {
    global_vars = table_new();
    yyparse();
    table_free(global_vars);
    return 0;
}

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
    exit(0);
}

// create new variables
struct table * table_new() {
    struct table * t = malloc(sizeof(struct table));
    const int SIZE = 13;
    t->size = SIZE; // default table size
    t->vars = malloc(sizeof(struct var) * SIZE);
    for (int i = 0; i < SIZE; i++) {
        t->vars[i].set = false;
    }
    return t;
}

// get value of variable given name
// return the index of the structure
// return: -1 failure
//         0 or more index
int table_get_attribute(struct table * t, char * name) {
    for (int i = 0; i < t->size; i++) {
        if (t->vars[i].set && (strcmp(t->vars[i].name,name) == 0)) {
            return i;
        }
    }
    return -1;
}

// set table given name of vars array
void table_set_attribute(struct table * t, int value, char * name) {
    // if tables is full increase size by 2 + 1
    if (t->size == 0 || t->vars[t->size - 1].set) {
        int old_size = t->size;
        int new_size = (old_size << 1) + 1;  // t->size * 2 + 1
        if (realloc(t->vars, sizeof(struct var) * new_size) != NULL) {
            t->size = new_size;
            for (int i = old_size; i < new_size; i++) {
                t->vars[i].set = false;
            }
        }
        else {
            printf("Error reallocating memory\n");
            return;
        }
    }
    // check and set the variables when not set
    for (int i = 0; i < t->size; i++) {
        if (!t->vars[i].set || (t->vars[i].set && (strcmp(t->vars[i].name, name) == 0))) {
            t->vars[i].name = name;
            t->vars[i].value = value;
            t->vars[i].set = true;
            break;
        }
    }
}

// print all variables values and their names
void table_print(struct table * t) {
    printf("| Name\t| Val\t| Set \t|\n");
    for (int i = 0; i < t->size; i++) {
        if (t->vars[i].set) {
            printf("| %s\t|", t->vars[i].name);
            printf(" %d\t|", t->vars[i].value);
            t->vars[i].set ? printf(" true\t|\n") : printf(" false\n|\n");
        }
    }
}

// free memory for table
void table_free(struct table * t) {
    free(t->vars);
    free(t);
}
