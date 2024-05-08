%{
#include "calcwithvariables.tab.h"
%}

%option noyywrap


ENGLISH_LOWER [a-z]
ENGLISH_UPPER [A-Z]
ASSIGNMENT_OPERATOR ":="
NUMBER [0-9]
END_OF_STATEMENT ";"

PRINT "print"

END_OF_LINE "\n"

PLUS "+"
MINUS "-"
MUL "*"
DIV "/"
OPEN_BRACKET "("
CLOSE_BRACKET ")"

%%
{PRINT} { return PRINT; }
{ENGLISH_LOWER}+ { yylval.text = strdup(yytext); return VAR; }
{ASSIGNMENT_OPERATOR} { return ASSIGN; }
{NUMBER}+ { yylval.intval = atoi(yytext); return NUM; }
{END_OF_STATEMENT} { return EOS; }
{END_OF_LINE} { return EOL; }
{PLUS} { return ADD; }
{MINUS} { return MINUS; }
{MUL} { return MUL; }
{DIV} { return DIV; }
{OPEN_BRACKET} { return OPEN_BRACKET; }
{CLOSE_BRACKET} { return CLOSE_BRACKET; }
{ENGLISH_UPPER} { yyerror("syntax error"); }
\.   { yyerror("syntax error"); }

%%