%{
#include "roman.tab.h"
void yyerror(const char *s);
%}

%option noyywrap

%%
I{4,} { yyerror("syntax error"); }
X{4,} { yyerror("syntax error"); }
C{4,} { yyerror("syntax error"); }
M{4,} { yyerror("syntax error"); }
L{2,} { yyerror("syntax error"); }
V{2,} { yyerror("syntax error"); }
D{2,} { yyerror("syntax error"); }
I(V|X|L|C|M)I { yyerror("syntax error"); }
CMC { yyerror("syntax error"); }
CCM { yyerror("syntax error"); }
IIX { yyerror("syntax error"); }
XIII { yyerror("syntax error"); }
VL { yyerror("syntax error"); }
IL { yyerror("syntax error"); }
VX { yyerror("syntax error"); }
LC { yyerror("syntax error"); }
VC { yyerror("syntax error"); }
IC { yyerror("syntax error"); }
LLD { yyerror("syntax error"); }
LD { yyerror("syntax error"); }
IM { yyerror("syntax error"); }
DM { yyerror("syntax error"); }
XD { yyerror("syntax error"); }
VD { yyerror("syntax error"); }
XM { yyerror("syntax error"); }
ID { yyerror("syntax error"); }
VM { yyerror("syntax error"); }

IV { yylval = 4; return ROMAN_NUMERAL; }
IX { yylval = 9; return ROMAN_NUMERAL; }
XL { yylval = 40; return ROMAN_NUMERAL; }
XC { yylval = 90; return ROMAN_NUMERAL; }
CD { yylval = 400; return ROMAN_NUMERAL; }
CM { yylval = 900; return ROMAN_NUMERAL; }
I { yylval = 1; return ROMAN_NUMERAL; }
V { yylval = 5; return ROMAN_NUMERAL; }
X { yylval = 10; return ROMAN_NUMERAL; }
L { yylval = 50; return ROMAN_NUMERAL; }
C { yylval = 100; return ROMAN_NUMERAL; }
D { yylval = 500; return ROMAN_NUMERAL; }
M { yylval = 1000; return ROMAN_NUMERAL; }
\n  { return EOL; }
.
%%