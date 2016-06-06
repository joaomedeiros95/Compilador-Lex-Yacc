%{
/* C Declarations: */
#include <stdio.h>
#include "y.tab.h" /* definitions of IF, ID, NUM,... */
%}
/* Lex Definitions: */
digits [0-9]
%%
char	     	                            {yylval.sValue = strdup(yytext);return(TYPE);}
void	     	                            {yylval.sValue = strdup(yytext);return(TYPE);}
build	     	                            {yylval.sValue = strdup(yytext);return(BUILD);}
const		                                {yylval.sValue = strdup(yytext);return(CONST);}
float                           	        {yylval.sValue = strdup(yytext);return(TYPE); }
int			                                {yylval.sValue = strdup(yytext);return(TYPE);}
string			                            {yylval.sValue = strdup(yytext);return(TYPE);}
if                                          {yylval.sValue = strdup(yytext);return IF;}
elsif                                       {yylval.sValue = strdup(yytext);return ELSIF;}
else                                        {yylval.sValue = strdup(yytext);return ELSE;}
for                                         {yylval.sValue = strdup(yytext);return FOR;}
while                                       {yylval.sValue = strdup(yytext);return WHILE;}
switch                                      {yylval.sValue = strdup(yytext);return SWITCH;}
case                                        {yylval.sValue = strdup(yytext);return CASE;}
default                                     {yylval.sValue = strdup(yytext);return DEFAULT;}
stop                                        {yylval.sValue = strdup(yytext);return STOP;}
return                                      {yylval.sValue = strdup(yytext);return RETURN;}
print                                       {yylval.sValue = strdup(yytext);return PRINT;}
read                                        {yylval.sValue = strdup(yytext);return READ;}
free                                        {yylval.sValue = strdup(yytext);return FREE;}
alloc                                       {yylval.sValue = strdup(yytext);return ALLOC;}
[a-zA-Z][a-zA-Z0-9]*                        {yylval.sValue = strdup(yytext);return ID;}
({digits}+)                                 {yylval.iValue = atoi(yytext);return NUM;}
(-{digits}+)                                {yylval.iValue = atoi(yytext);return NUM;}
({digits}*\.{digits}+)                      {yylval.fValue = atof(yytext);return REAL;}
\"(\\.|[^\\"])*\"                           {yylval.sValue = strdup(yytext);return STRING;}
\'(\\.|[^\\'])*\'                           {yylval.sValue = strdup(yytext);return STRING;}
([#].+\n)                                   {yylineno++;}
"("                                         {yylval.sValue = strdup(yytext);return(LEFT_PAREN);}
")"                                         {yylval.sValue = strdup(yytext);return(RIGHT_PAREN);}
"["                                         {yylval.sValue = strdup(yytext);return(LEFT_BRACKET);}
"]"                                         {yylval.sValue = strdup(yytext);return(RIGHT_BRACKET);}
"{"                                         {yylval.sValue = strdup(yytext);return(LEFT_BRACE);}
"}"                                         {yylval.sValue = strdup(yytext);return(RIGHT_BRACE);}
";"                                         {yylval.sValue = strdup(yytext);return(SEMICOLON);}
":"					                        {yylval.sValue = strdup(yytext);return(COLON);}
","                                         {yylval.sValue = strdup(yytext);return(COMMA);}
"+"                                         {yylval.sValue = strdup(yytext);return(OP_PLUS);}
"-"                                         {yylval.sValue = strdup(yytext);return(OP_MINUS);}
"*"                                         {yylval.sValue = strdup(yytext);return(OP_TIMES);}
"/"                                         {yylval.sValue = strdup(yytext);return(OP_DIV);}
"%"                                         {yylval.sValue = strdup(yytext);return(OP_REST);}
"&&"                                        {yylval.sValue = strdup(yytext);return(OP_AND);}
"||"                                        {yylval.sValue = strdup(yytext);return(OP_OR);}
"!"                                         {yylval.sValue = strdup(yytext);return(OP_NOT);}
"!="                                        {yylval.sValue = strdup(yytext);return(OP_NOT_EQUAL);}
"="                                         {yylval.sValue = strdup(yytext);return(OP_ATR);}
"=="                                        {yylval.sValue = strdup(yytext);return(OP_EQUAL);}
"<"                                         {yylval.sValue = strdup(yytext);return(OP_LESS);}
"<="                                        {yylval.sValue = strdup(yytext);return(OP_LESS_EQUAL);}
">"                                         {yylval.sValue = strdup(yytext);return(OP_GREATHER);}
">="                                        {yylval.sValue = strdup(yytext);return(OP_GREATHER_EQUAL);}
"++"  			                            {yylval.sValue = strdup(yytext);return(INC_OP);}
"--"  			                            {yylval.sValue = strdup(yytext);return(DEC_OP);}
"+="			                            {yylval.sValue = strdup(yytext);return(ADD_ASSIGN);}
"-="			                            {yylval.sValue = strdup(yytext);return(SUB_ASSIGN);}
"*="			                            {yylval.sValue = strdup(yytext);return(MUL_ASSIGN);}
"/="	                                    {yylval.sValue = strdup(yytext);return(DIV_ASSIGN);}
[\n]                                        {yylineno++;}
[ \t\r]+
.                                           

%%

#ifndef yywrap
   int yywrap (void) {return 1;}
#endif
