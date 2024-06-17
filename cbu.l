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
�ڶ�|"+"		{return(ADD); }
���̳���|"-"	{return(SUB); }
��ǥ|"*"		{return(MUL); }
�볪|"/"		{return(DIV); }
����			{return(TWICE); }
������			{return(TTWICE); }
��				{return(TO); }
��				{return(SQR); }
����|":="		{return(ASSGN); }
���ش���|"+="	{return(ASSIGNADD);}
������|"-="		{return(ASSIGNSUB);}
���ش���|"*="	{return(ASSIGNMUL);}
�볪����|"/="	{return(ASSIGNDIV);}
"++"			{return(INCREASE);}
"--"			{return(DECREASE);}
"!"				{return(STMTEND); }
�̶�|��|��|��	{return(WITH); }
�����̴�		{return(EQUAL); }
���Ǵٸ���		{return(NOTEQUAL); }
����ũ��		{return(GREATER); }
�����۴�		{return(LESS); }
����ũ�ų�����	{return(GREATEREQUAL); }
�����۰ų�����	{return(LESSEQUAL); }
��				{return(IF); }
�Ƴ�\?			{return(THEN); }
�𸰴�			{return(IFEND); }
�׶���̰�		{return(ELIF); }
�׶��			{return(ELSE); }
����			{return(DO); }
�����			{return(WHILE); }
\"				{return(QUOTE); }
\{				{return(OPENBRACKET); }
\}				{return(CLOSEBRACKET); }
�尡��\~		{return(START); };
�����ض�\~		{return(END); }
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