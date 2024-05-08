%{
#include <stdio.h>
#include <stdlib.h>
int yylex();
void yyerror(char *s);
void print_roman(int number);   
%}

/* declare tokens */

/* roman numerals and operators */
%token ROMAN_NUMERAL 
%token ADD MINUS MUL DIV
%token OPEN_PAREN CLOSE_PAREN
%token EOL

%left ADD 
%left MINUS 
%left MUL
%left DIV
%%

calcnum:
 | calcnum expr EOL { print_roman($2); printf("\n"); }
 ;

expr: roman
 | expr ADD expr { $$ = $1 + $3; }
 | expr MINUS expr { $$ = $1 - $3; }
 | MINUS expr { $$ = -$2; }
 | expr MUL expr { $$ = $1 * $3; }
 | expr DIV expr { ($3 != 0) ? $$ = $1 / $3 : yyerror("syntax error"); }
 | OPEN_PAREN expr CLOSE_PAREN { $$ = $2; }
 ;

roman: ROMAN_NUMERAL 
 | roman ROMAN_NUMERAL { $$ = $1 + $2; }
 ;
%%

int main() {
    yyparse();
    return 0;
}

/* Source:  
    https://www.geeksforgeeks.org/converting-decimal-number-lying-between-1-to-3999-to-roman-numerals/
*/
void print_roman(int number) {   
    if (number < 0) {
        printf("-");
        number = -number;
    }
    if (number != 0) {
        int num[] = {1,4,5,9,10,40,50,90,100,400,500,900,1000};
        char* sym[] = {"I","IV","V","IX","X","XL","L","XC","C","CD","D","CM","M"};
        int i = 12;
        while (number > 0) {
            int div = number / num[i];
            number = number % num[i]; 
            while (div--) {
                printf("%s", sym[i]);
            }
            i--;
        } 
    }   
    else {
        printf("Z");
    }
}

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
    exit(0);
}