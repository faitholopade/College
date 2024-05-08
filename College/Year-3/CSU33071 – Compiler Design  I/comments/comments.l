%{
%}

%%
"**".*        {}
\"(.|\n)*\"        {printf("%s",yytext);}
"}"|"{"        {printf("syntax error\n"); return 0;}
"{"[^"\}"]*"}"    {}
%%

int main(){
    yylex();
    return 0;
}
