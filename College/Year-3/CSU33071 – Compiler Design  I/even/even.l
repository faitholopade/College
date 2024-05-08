%{
#include <stdio.h>
#include <stdlib.h>
int evenCount = 0;
%}

%%

[+-]?[0-9]+ {
    int number = atoi(yytext);
    if (number % 2 == 0) {
        evenCount++;
    }
}

\n { /* Action for newline, just to acknowledge its presence, no operation needed */ }

%%

int main() {
    yylex();
    printf("%d\n", evenCount);
    return 0; // Ensuring we return 0 as per your requirement
}
