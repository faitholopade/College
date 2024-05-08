%option nodefault
%{
const int CURRENT_YEAR = 2024;
const int TWO_THOUSAND = 2000;
const int NINTY_HUNDRED = 1900;
%}

/* Years 2013-present */
YYY [0-9][0-9][0-9]
/* Years 1987-2012 */
YY [0-9][0-9]

/* County Present 2013-present */
CCP C|CE|CN|CW|D|DL|G|KE|KK|KY|L|LD|LH|LM|LS|MH|MN|MO|OY|RN|SO|T|W|WH|WX|WW 
/* Counties not found in CCP (county present) 1987-2013 */
CCD LK|TN|TS|W|WD

/* Sequence number is 0-9 1-6 times */
SEQNUM [0-9]{1,6}

WHITESPACE [ \t\n]+

%%
{YYY}-{CCP}-{SEQNUM}{WHITESPACE} {
    int year = 0;
    char * new_year = (char *)malloc(sizeof(char) * 2); 
    new_year[0] = yytext[0];
    new_year[1] = yytext[1];
    year = atoi(new_year);
    free(new_year);
    
    year += TWO_THOUSAND;
    /* years since registration */
    year = CURRENT_YEAR - year;
    printf("%d\n", year);
}
{YY}-({CCP}|{CCD})-{SEQNUM}{WHITESPACE} {
    int year = 0;
    char * new_year = (char *)malloc(sizeof(char) * 2); 
    new_year[0] = yytext[0];
    new_year[1] = yytext[1];
    year = atoi(new_year);
    free(new_year);

    /* T is invalid in {YY} format */
    if (yytext[3] == 'T')
        printf("INVALID\n"); 
    /* The years 87 to 13 do not have plates */
    /* These are 90's plates never existed */
    else if (year > 12 && year < 87)
        printf("INVALID\n");
    /* Early 2000's, 2000 to 2012 */
    else if (year <= 12 && year >= 0) {
        year += TWO_THOUSAND;
        /* years since registration */
        year = CURRENT_YEAR - year;
        printf("%d\n", year);
    }
    else {
        /* Must be a valid 19's plate */
        year += NINTY_HUNDRED;
        /* years since registration */
        year = CURRENT_YEAR - year;
        printf("%d\n", year);
    }
}
{WHITESPACE}
.* { printf("INVALID\n"); }
%%

int main() {
    yylex();
    return 0;
}