%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char *s); // Note the use of `const`
int yylex(void);
%}

%token ROMAN_NUMERAL 
%token EOL

%%

calcnum: 
 | calcnum roman EOL { printf("%d\n", $2); }
 ;

roman: ROMAN_NUMERAL 
 | roman ROMAN_NUMERAL { $$ = $1 + $2; }
 ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
    exit(0);
}

int main(void) {
    yyparse();
    return 0;
}
