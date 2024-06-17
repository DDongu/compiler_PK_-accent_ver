%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG	0

#define	 MAXSYM	100
#define	 MAXSYMLEN 20
#define	 MAXTSYMLEN	15
#define	 MAXTSYMBOL	MAXSYM/2

#define STMTLIST 500

typedef struct nodeType {
	int token;
	int tokenval;
	struct nodeType *son;
	struct nodeType *brother;
	} Node;

#define YYSTYPE Node*
	
int tsymbolcnt=0;
int errorcnt=0;

const int postfixSize=1;
int sqrN=0;
int sqr=0;
int twice=2;
int ttwice=4;
int label_count=0;
int if_stmt_endpoint=0;
int condition_label=0;
int if_count=0;
int while_count=0;

FILE *yyin;
FILE *fp;

extern char symtbl[MAXSYM][MAXSYMLEN];
extern int maxsym;
extern int lineno;

void DFSTree(Node*);
Node * MakeOPTree(int, Node*, Node*);
Node * MakeNode(int, int);
Node * MakeListTree(Node*, Node*);
void codegen(Node* );
void prtcode(int, int);

void dwgen();
int	gentemp();
void assgnstmt(int, int);
void numassgn(int, int);
void addstmt(int, int, int);
void substmt(int, int, int);
int insertsym(char *);
%}

%token ADD SUB MUL DIV TWICE TTWICE TO SQR SQRID SQRNUM INCREASE DECREASE ASSGN ASSIGNADD ASSIGNSUB ASSIGNMUL ASSIGNDIV
%token ID NUM STMTEND START END ID2 ID_ASSIGN OPENBRACKET CLOSEBRACKET QUOTE
%token NONE WITH EQUAL NOTEQUAL GREATER LESS GREATEREQUAL LESSEQUAL CONDITION IFSTATEMENT IF THEN IFEND ELIF ELSE DO WHILE

%%
program	: START stmt_list END	{ if (errorcnt==0) {codegen($2); dwgen();} }
		;

// 각 문장에 대한 식 List
stmt_list	: 	stmt_list stmt 	{ $$=MakeListTree($1, $2); }
			|	stmt			{ $$=MakeListTree(NULL, $1); }
			| 	error STMTEND	{ errorcnt++; yyerrok;}
			;

stmt	:	stmt_expr
		|	stmt_do_while
		|	stmt_if_else		{ $$=MakeOPTree(IFSTATEMENT, $1, NULL); }
		;

stmt_expr	:	ID ASSGN expr_calcA STMTEND	{ $1->token = ID2; $$=MakeOPTree(ASSGN, $1, $3);}
			|	ID ASSIGNADD expr_calcA STMTEND {$1->token = ID_ASSIGN; $$=MakeOPTree(ASSIGNADD, $1, $3);}
			|	ID ASSIGNSUB expr_calcA STMTEND {$1->token = ID_ASSIGN; $$=MakeOPTree(ASSIGNSUB, $1, $3);}
			|	ID ASSIGNMUL expr_calcA STMTEND {$1->token = ID_ASSIGN; $$=MakeOPTree(ASSIGNMUL, $1, $3);}
			|	ID ASSIGNDIV expr_calcA STMTEND {$1->token = ID_ASSIGN; $$=MakeOPTree(ASSIGNDIV, $1, $3);}
			|	expr_postfix
			|	condition
			;

stmt_condition	:	condition		{ $$=MakeOPTree(CONDITION, $1, NULL); }
				;
				
stmt_do_while	:	OPENBRACKET expr_do_while  { $$=MakeOPTree(WHILE, $2, NULL); }
				;

				;

stmt_if_else :	stmt_if_else stmt_else	{ $$=MakeOPTree(NONE, $1, $2); }
			|	stmt_if_else stmt_elif	{ $$=MakeOPTree(NONE, $1, $2); }
			|	stmt_if					{ $$=MakeOPTree(NONE, $1, NULL); }
			;

stmt_else	:	ELSE stmt_list IFEND	{ $$=MakeOPTree(ELSE, $2, NULL); }
			;

stmt_elif	:	ELIF expr_if IFEND		{ $$=MakeOPTree(IF, $2, NULL); }
			;

stmt_if		:	IF expr_if IFEND 		{ $$=MakeOPTree(IF, $2, NULL); }
			;


expr_do_while	:	stmt_list CLOSEBRACKET WHILE QUOTE stmt_condition QUOTE  { $$=MakeListTree($1, $5); }

expr_if		:	QUOTE stmt_condition QUOTE THEN stmt_list	{ $$=MakeListTree($2, $5); }
			;

condition	:	condition WITH expr_calcA EQUAL   			{ $$=MakeOPTree(EQUAL, $1, $3); }
			|	condition WITH expr_calcA NOTEQUAL 			{ $$=MakeOPTree(NOTEQUAL, $1, $3); }
			|	condition WITH expr_calcA GREATER 			{ $$=MakeOPTree(GREATER, $1, $3); }
			|	condition WITH expr_calcA LESS 				{ $$=MakeOPTree(LESS, $1, $3); }
			|	condition WITH expr_calcA GREATEREQUAL 		{ $$=MakeOPTree(GREATEREQUAL, $1, $3); }
			|	condition WITH expr_calcA LESSEQUAL 		{ $$=MakeOPTree(LESSEQUAL, $1, $3); }
			|	expr_calcA							
			;	

expr_postfix :	ID INCREASE STMTEND { $1->token = ID_ASSIGN; $$=MakeOPTree(INCREASE, $1, NULL);}
			|	ID DECREASE STMTEND { $1->token = ID_ASSIGN; $$=MakeOPTree(DECREASE, $1, NULL);}
			;

expr_calcA	: 	expr_calcA ADD expr_calcB	{ $$=MakeOPTree(ADD, $1, $3); }
			|	expr_calcA SUB expr_calcB	{ $$=MakeOPTree(SUB, $1, $3); }
			|	expr_calcB
			;	

expr_calcB	:	expr_calcB DIV expr_calcC	{ $$=MakeOPTree(DIV, $1, $3); }
			|	expr_calcB MUL expr_calcC	{ $$=MakeOPTree(MUL, $1, $3); }
			|	expr_calcC
			;

expr_calcC	:	expr_calcC TO term SQR		{ sqrN=$1->tokenval; sqr=$3->tokenval; $$=MakeOPTree(SQR, $1, NULL); }
			|	expr_calcC TWICE			{ $$=MakeOPTree(TWICE, $1, NULL); }
			|	expr_calcC TTWICE			{ $$=MakeOPTree(TTWICE, $1, NULL); }
			|	term
			;


term	:	ID		{ /* ID node is created in lex */ }
		|	NUM		{ /* NUM node is created in lex */ }
		|	DO		{ $$=MakeNode(DO, NULL); }
		;

%%
int main(int argc, char *argv[]) 
{
	printf("2019038077 KimDongGyu complier (ehdrb1205@naver.com)\n");
	
	if (argc == 2)
		yyin = fopen(argv[1], "r");
	else {
		printf("Usage: cbu inputfile\noutput file is 'a.asm'\n");
		return(0);
		}
		
	fp=fopen("a.asm", "w");
	
	yyparse();
	
	fclose(yyin);
	fclose(fp);

	if (errorcnt==0) 
		{ printf("Successfully compiled. Assembly code is in 'a.asm'.\n");}
}

yyerror(s)
char *s;
{
	printf("%s (line %d)\n", s, lineno);
}


Node * MakeOPTree(int op, Node* operand1, Node* operand2)
{
Node * newnode;

	newnode = (Node *)malloc(sizeof (Node));
	newnode->token = op;
	newnode->tokenval = op;
	newnode->son = operand1;
	newnode->brother = NULL; 		// 추가되는 노드에는 형제X
	operand1->brother = operand2;
	return newnode;
}

Node * MakeNode(int token, int operand)
{
Node * newnode;

	newnode = (Node *) malloc(sizeof (Node));
	newnode->token = token;
	newnode->tokenval = operand; 
	newnode->son = newnode->brother = NULL;
	return newnode;
}

Node * MakeListTree(Node* operand1, Node* operand2)
{
Node * newnode;
Node * node;

	if (operand1 == NULL){
		newnode = (Node *)malloc(sizeof (Node));
		newnode->token = newnode-> tokenval = STMTLIST;
		newnode->son = operand2;
		newnode->brother = NULL;
		return newnode;
		}
	else {
		node = operand1->son;
		while (node->brother != NULL) node = node->brother;
		node->brother = operand2;
		return operand1;
		}
}

void codegen(Node * root)
{
	DFSTree(root);
}

void DFSTree(Node * n)
{
	if (n==NULL) return;
	DFSTree(n->son);
	prtcode(n->token, n->tokenval);
	DFSTree(n->brother);
	
}

// systble에는 100길이의 문자열 20개가 올 수 있음
void prtcode(int token, int val)
{
	switch (token) {
	case ID:
		fprintf(fp,"RVALUE %s\n", symtbl[val]);
		break;

	case ID2:
		fprintf(fp, "LVALUE %s\n", symtbl[val]);
		break;
	
	case ID_ASSIGN:
		fprintf(fp, "LVALUE %s\n", symtbl[val]); 
		fprintf(fp, "RVALUE %s\n", symtbl[val]);
		break;

	case NUM:
		fprintf(fp, "PUSH %d\n", val);
		break;
	
	case INCREASE:
		fprintf(fp, "PUSH %d\n", postfixSize);
		fprintf(fp, "+\n");
		fprintf(fp, ":=\n");
		break;

	case DECREASE:
		fprintf(fp, "PUSH %d\n", postfixSize);
		fprintf(fp, "-\n");
		fprintf(fp, ":=\n");
		break;

	case ASSGN:
		fprintf(fp, ":=\n");
		break;

	case ASSIGNADD:
		fprintf(fp, "+\n");
		fprintf(fp, ":=\n");
		break;

	case ASSIGNSUB:
		fprintf(fp, "-\n");
		fprintf(fp, ":=\n");
		break;

	case ASSIGNMUL:
		fprintf(fp, "*\n");
		fprintf(fp, ":=\n");
		break;

	case ASSIGNDIV:
		fprintf(fp, "/\n");
		fprintf(fp, ":=\n");
		break;

	case ADD:
		fprintf(fp, "+\n");
		break;
	
	case SUB:
		fprintf(fp, "-\n");
		break;
	
	case MUL:
		fprintf(fp, "*\n");
		break;

	case DIV:
		fprintf(fp, "/\n");
		break;

	case SQR:
		for(int i=0; i<sqr-1; i++) { 
			fprintf(fp, "PUSH %d\n", sqrN);
			fprintf(fp, "*\n");
		}
		break;


	case TWICE:
		fprintf(fp, "PUSH 2\n");
		fprintf(fp, "*\n");
		break;
	
	case TTWICE:
		fprintf(fp, "PUSH 4\n");
		fprintf(fp, "*\n");
		break;
	
	case EQUAL:
		fprintf(fp, "-\n");
		fprintf(fp, "GOTRUE label%d\n", label_count);
		break;
	
	case NOTEQUAL:
		fprintf(fp, "-\n");
		fprintf(fp, "GOFALSE label%d\n", label_count);
		break;
	
	case GREATER:
		fprintf(fp, "-\n");
		fprintf(fp, "GOPLUS condition%d\n", condition_label);
		fprintf(fp, "GOTO label%d\n", label_count);
		fprintf(fp,"LABEL condition%d\n", condition_label++);
		break;
	
	case LESS:
		fprintf(fp, "-\n");
		fprintf(fp, "GOMINUS condition%d\n", condition_label);
		fprintf(fp, "GOTO label%d\n", label_count);
		fprintf(fp,"LABEL condition%d\n", condition_label++);
		break;

	case GREATEREQUAL:
		fprintf(fp, "-\n");
		fprintf(fp, "GOMINUS label%d\n", label_count);
		break;
	
	case LESSEQUAL:
		fprintf(fp, "-\n");
		fprintf(fp, "GOPLUS label%d\n", label_count);
		break;

	case IFSTATEMENT:
		fprintf(fp,"LABEL IFLabel%d\n", if_count++);
		break;

	case IF:
		/* if실행문 */
		fprintf(fp,"GOTO IFLabel%d\n", if_count);
		fprintf(fp,"LABEL label%d\n", label_count++);
		/* else 실행문 */
		break;

	case ELSE:
		/* else 실행문 */
		fprintf(fp,"LABEL label\n", label_count++);
		/* 다음 코드 */
		break;

	case DO:
		fprintf(fp,"LABEL loop%d\n", while_count++);
		break;

	case WHILE:
		fprintf(fp, "GOTO loop%d\n", while_count-1);
		fprintf(fp, "LABEL label%d\n", label_count++);
		break;



	case STMTLIST:
	case NONE:
	default:
		break;
	};
}


/*
int gentemp()
{
char buffer[MAXTSYMLEN];
char tempsym[MAXSYMLEN]="TTCBU";

	tsymbolcnt++;
	if (tsymbolcnt > MAXTSYMBOL) printf("temp symbol overflow\n");
	itoa(tsymbolcnt, buffer, 10);
	strcat(tempsym, buffer);
	return( insertsym(tempsym) ); // Warning: duplicated symbol is not checked for lazy implementation
}
*/
void dwgen()
{
int i;
	fprintf(fp, "HALT\n");
	fprintf(fp, "$ -- END OF EXECUTION CODE AND START OF VAR DEFINITIONS --\n");

// Warning: this code should be different if variable declaration is supported in the language 
	for(i=0; i<maxsym; i++) 
		fprintf(fp, "DW %s\n", symtbl[i]);
	fprintf(fp, "END\n");
}

