%{
#include <string.h>
#include "cbu.h"
#define MAXSYM	100
#define	MAXSYMLEN 20
char symtbl[MAXSYM][MAXSYMLEN];
int lineno=1;
int insertsym();
int maxsym=0;
char s[MAXSYMLEN];
int temp;

%}
sp		[ \t]
ws		{sp}+
nl		\n
eletter	[A-Za-z]
hletter	[\xb0-\xfe][\xa0-\xfe]
letter	({eletter}|{hletter})
digit	-?[0-9]+
id		{letter}({letter}|{digit})*
comment "//".*
%%
{comment}		{ /* do nothing */ }
{ws}			{/* do nothing */}
{nl}			{lineno++; }
쁘라스|"+"		{return(ADD); }
마이나스|"-"	{return(SUB); }
꼽표|"*"		{return(MUL); }
노나|"/"		{return(DIV); }
따블			{return(TWICE); }
따따블			{return(TTWICE); }
의				{return(TO); }
승				{return(SQR); }
느라|":="		{return(ASSGN); }
더해느라|"+="	{return(ASSIGNADD);}
빼느라|"-="		{return(ASSIGNSUB);}
꼽해느라|"*="	{return(ASSIGNMUL);}
노나느라|"/="	{return(ASSIGNDIV);}
"++"			{return(INCREASE);}
"--"			{return(DECREASE);}
"!"				{return(STMTEND); }
이랑|랑|이|가	{return(WITH); }
영판이다		{return(EQUAL); }
영판다르다		{return(NOTEQUAL); }
보다크다		{return(GREATER); }
보다작다		{return(LESS); }
보다크거나같다	{return(GREATEREQUAL); }
보다작거나같다	{return(LESSEQUAL); }
마				{return(IF); }
아나\?			{return(THEN); }
모린다			{return(IFEND); }
그라믄이건		{return(ELIF); }
그라믄			{return(ELSE); }
점마			{return(DO); }
뭐라노			{return(WHILE); }
\"				{return(QUOTE); }
\{				{return(OPENBRACKET); }
\}				{return(CLOSEBRACKET); }
드가자\~		{return(START); };
고마해라\~		{return(END); }
{id}			{temp=insertsym(yytext); yylval=MakeNode(ID, temp); return(ID);} 						
{digit}			{sscanf(yytext, "%d", &temp); yylval=MakeNode(NUM, temp); return(NUM);}
.				{printf("invalid token %s\n", yytext); }
%%

int insertsym(s)
char *s;
{
int i;

	for(i=0;i<maxsym; i++) {
		if(strcmp(s, symtbl[i])==0) return(i);
		}
	if (i<MAXSYM-1) {
		strcpy(symtbl[maxsym],s);
		maxsym++;
		return(maxsym-1);
		}
	else
		{printf("symbol table overflow\n");}
	return(0);
}

yywrap() {}