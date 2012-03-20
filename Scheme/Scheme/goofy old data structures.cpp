// Scheme.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Helpers */
typedef int BOOL;

#define TRUE 1;
#define FALSE 0;

BOOL contains(char* list, char el)
{
	for (int i = 0; i < strlen(list); i++) {
		if(list[i] == el) return TRUE;
	}
	return FALSE;
}

/* ==== Scanner and Lexer ==== */

/* Data structures for scanner and lexer */

typedef struct {
    char value;
	int offset;
} CHARACTER;

char* ATOM_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890`-=+/*";
char* LPAREN_CHARS = "(";
char* RPAREN_CHARS = ")";
char* WHITESPACE_CHARS = " \r\n\t";

typedef enum {
	ATOM,
	LPAREN,
	RPAREN,
	WHITESPACE, 
	NOTHING
} TOKEN_TYPE;

typedef struct {
	CHARACTER* text;
	TOKEN_TYPE type;
	int offset;
	int length;	
} TOKEN;

/* END data structures for scanner and lexer */

CHARACTER* scan(char* inputBuffer)
{
	int length = strlen(inputBuffer);
	CHARACTER* output = (CHARACTER*)malloc(sizeof(CHARACTER) * length);

	for(int i = 0; i < length; i++)	{
		CHARACTER currentChar;
		currentChar.value = inputBuffer[i];
		currentChar.offset = i;
		output[i] = currentChar;
	}

	return output;
}

void print_scanned(CHARACTER* buffer, int length)
{
	for(int i = 0; i < length; i++)	{
		CHARACTER current = buffer[i];
		printf("%i : %c\n", current.offset, current.value);
	}
}

CHARACTER* getCharSubstring(CHARACTER* source, int start, int length)
{
	CHARACTER* dest = (CHARACTER*)malloc(sizeof(CHARACTER) * length);
	for (int i = 0; i < length; i++) {
		dest[i] = source[start + i];
	}
	return dest;
}

TOKEN* lex(CHARACTER* inputChars, int length, int* outputLength)
{
	TOKEN_TYPE currentType = NOTHING;
	CHARACTER currentChar;

	TOKEN* tokens = (TOKEN*)malloc(sizeof(TOKEN) * length);

	int index = 0;
	int tokenIndex = 0;

	while (index < length) {
		currentChar = inputChars[index];
		
		if(contains(WHITESPACE_CHARS, currentChar.value)) {

			TOKEN whiteSpace;
			whiteSpace.type = WHITESPACE;
			whiteSpace.offset = index;

			while (contains(WHITESPACE_CHARS, currentChar.value) && index < length) {
				currentChar = inputChars[++index];
			}

			whiteSpace.length = index - whiteSpace.offset;
			whiteSpace.text = getCharSubstring(inputChars, whiteSpace.offset, whiteSpace.length);

			tokens[tokenIndex] = whiteSpace;

			tokenIndex++;
		}
		else if(contains(LPAREN_CHARS, currentChar.value)) {

			TOKEN lparen;
			lparen.type = LPAREN;
			lparen.offset = index;

			lparen.length = 1;
			lparen.text = (CHARACTER*)malloc(sizeof(CHARACTER));
			*lparen.text = currentChar;

			tokens[tokenIndex] = lparen;

			tokenIndex++;
			index++;
		}
		else if(contains(RPAREN_CHARS, currentChar.value)) {

			TOKEN rparen;
			rparen.type = RPAREN;
			rparen.offset = index;

			rparen.length = 1;
			rparen.text = (CHARACTER*)malloc(sizeof(CHARACTER));
			*rparen.text = currentChar;

			tokens[tokenIndex] = rparen;

			tokenIndex++;
			index++;
		}
		else if(contains(ATOM_CHARS, currentChar.value)) {
		
			TOKEN atom;
			atom.type = ATOM;
			atom.offset = index;

			while (contains(ATOM_CHARS, currentChar.value) && index < length) {
				currentChar = inputChars[++index];
			}

			atom.length = index - atom.offset;
			atom.text = getCharSubstring(inputChars, atom.offset, atom.length);

			tokens[tokenIndex] = atom;

			tokenIndex++;	
		}
	}

	*outputLength = tokenIndex;

	return tokens;
}

/* Printing helpers for lexer */

char* getTextForToken(TOKEN toPrint)
{
	if(toPrint.text[0].value == ' ')
		return "SPACE";

	char* str = (char*)malloc(sizeof(char) * (toPrint.length));

	for (int i = 0; i < toPrint.length; i++) {
		str[i] = toPrint.text[i].value;
	}

	str[toPrint.length] = (char)0;

	return str;
}

char* getTokenTypeString(TOKEN_TYPE type)
{
	switch(type)
	{
		case ATOM: return "ATOM";
		case LPAREN: return "LPAREN";
		case RPAREN: return "RPAREN";
		case WHITESPACE: return "WHITESPACE";
		case NOTHING: return "NOTHING";
	}
}
void printTokens(TOKEN* tokens, int length)
{
	for (int i = 0; i < length; i++) {
		TOKEN current = tokens[i];
		printf("offset: %i length: %i type: %s text: %s\n", current.offset, current.length, getTokenTypeString(current.type), getTextForToken(current));
	}
}

void printTokenizedString(char* inputString)
{
	int length = strlen(inputString);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(inputString), length, &tokenLength);
	printTokens(tokens, tokenLength);
}

/* END Printing helpers for lexer */

/* ==== END Scanner and Lexer ==== */

/* ==== Parser ==== */

/* Data structures for parser */

typedef struct ast_node AST_NODE;
typedef struct ast_node_list AST_NODE_LIST;

struct ast_node_list {
	AST_NODE* head;
	AST_NODE_LIST* tail;
};

struct ast_node {
	TOKEN payload;
	AST_NODE_LIST* children;
};

AST_NODE_LIST* appendNode(AST_NODE_LIST* nodeList, AST_NODE* toAdd)
{
	if(!nodeList) {
		AST_NODE_LIST* toAddListItem = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		return toAddListItem;
	}
	if (nodeList->tail) {
		appendNode(nodeList->tail, toAdd);
		return nodeList;
	}
	else {
		AST_NODE_LIST* toAddListItem = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		nodeList->tail = toAddListItem;
		return nodeList;
	}
}

/* END Data structures for parser */

/* Tree Printer */

CHARACTER* getCharactersFromString(char* string)
{
	int length = strlen(string);
	CHARACTER* characters = (CHARACTER*)malloc(sizeof(CHARACTER) * length);
	for (int i = 0; i < length; i++) {
		CHARACTER currentChar;
		currentChar.offset = i;
		currentChar.value = string[i];
		characters[i] = currentChar;
	}
	return characters;
}

AST_NODE makeTestTree()
{
	AST_NODE* root = (AST_NODE*)malloc(sizeof(AST_NODE));
	TOKEN rootToken;
	rootToken.length = 1;
	rootToken.text = getCharactersFromString("(");
	root->payload = rootToken;

	AST_NODE* define = (AST_NODE*)malloc(sizeof(AST_NODE));
	TOKEN defToken;
	defToken.length = 6;
	defToken.text = getCharactersFromString("define");
	define->payload = defToken;
	define->children = 0;

	AST_NODE* foo = (AST_NODE*)malloc(sizeof(AST_NODE));
	TOKEN fooToken;
	fooToken.length = 3;
	fooToken.text = getCharactersFromString("foo");
	foo->payload = fooToken;
	foo->children = 0;
	
	AST_NODE* bar = (AST_NODE*)malloc(sizeof(AST_NODE));
	TOKEN barToken;
	barToken.length = 3;
	barToken.text = getCharactersFromString("bar");
	bar->payload = barToken;
	bar->children = 0;

	root->children = 0;
	root->children = appendNode(root->children, define);
	appendNode(root->children, foo);
	appendNode(root->children, bar);

	return *root;
}

void printTree(AST_NODE root)
{
	if (!root.children) {
		printf("%s", getTextForToken(root.payload));
		return;
	}

	printf("(");

	AST_NODE_LIST currentChild = *root.children;
	while (currentChild.tail) {
		printTree(*currentChild.head);
		currentChild = *currentChild.tail;
		printf(" ");
	}
	printTree(*currentChild.head);

	printf(")");
}

void testTree()
{
	AST_NODE testTree = makeTestTree();
	printTree(testTree);
}

/* END Tree Printer */

AST_NODE_LIST* consNode(AST_NODE* toAdd, AST_NODE_LIST* nodeList)
{
		AST_NODE_LIST* toAddListItem = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = nodeList;
		return toAddListItem;
}

AST_NODE* parse(TOKEN* tokens, int length)
{
	AST_NODE* rootNode = (AST_NODE*)malloc(sizeof(AST_NODE));
	rootNode->children = 0;

	AST_NODE_LIST* parentStack = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
	parentStack->head = rootNode;
	parentStack->tail = 0;

	for(int i = 0; i < length; i++) {

		TOKEN currentToken = tokens[i];

		if (currentToken.type == LPAREN || currentToken.type == ATOM) {
			AST_NODE* newASTNode = (AST_NODE*)malloc(sizeof(AST_NODE));
			newASTNode->payload = currentToken;
			newASTNode->children = 0;
			
			parentStack->head->children = appendNode(parentStack->head->children, newASTNode);

			if (currentToken.type == LPAREN)
				parentStack = consNode(newASTNode, parentStack); // push
		}
		else if (currentToken.type == RPAREN) {
			parentStack = parentStack->tail; // pop
		}
	}

	// assume the fake "root" node has exactly one child for now
	return parentStack->head->children->head;
}

void testParser(char* toParse)
{
	int length = strlen(toParse);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(toParse), length, &tokenLength);
	AST_NODE* root = parse(tokens, tokenLength);
	printTree(*root);
}

/* ==== END Parser ==== */

/* ==== Tree Parser ==== */

/* Data structures for tree parser */

typedef struct expr EXPR;
typedef struct expr_list EXPR_LIST;

struct expr_list {
	EXPR* head;
	EXPR_LIST* tail;
};

typedef enum {
	NUMERIC,
	STRING, 
	BOOLEAN,
	LIST
} CONST_TYPE;

typedef struct CONST_EXPR {
	CONST_TYPE type;
	union {
		double numeric;
		char* string;
		BOOL boolean;
		EXPR_LIST* list; // this is unsafe cause these MUST be all const_exprs in the list
	} data;
};

typedef struct VAR_EXPR {
	char* name;
};

typedef enum {
	PLUS,
	MINUS,
	DIV,
	MULT,
	EQUALS
} PRIM_TYPE;

typedef struct PRIM_APP_EXPR {
	PRIM_TYPE primitive;
	EXPR_LIST* args;
};

typedef struct FUN_APP_EXPR {
	VAR_EXPR* function;
	EXPR_LIST* args;
};

typedef struct COND_EXPR {
	EXPR_LIST* clauses;
};

typedef enum {
	VARIABLE,
	CONSTANT,
	PRIM_APP,
	FUN_APP,
	COND
} EXPR_TYPE;

struct expr {
	EXPR_TYPE type;
	union {
		VAR_EXPR* variable;
		CONST_EXPR* constant;
		PRIM_APP_EXPR* prim_app;
		FUN_APP_EXPR* fun_app;
		COND_EXPR* cond;
	} data;
};

EXPR_LIST* appendExprNode(EXPR_LIST* nodeList, EXPR* toAdd)
{
	if(!nodeList) {
		EXPR_LIST* toAddListItem = (EXPR_LIST*)malloc(sizeof(EXPR_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		return toAddListItem;
	}
	if (nodeList->tail) {
		appendExprNode(nodeList->tail, toAdd);
		return nodeList;
	}
	else {
		EXPR_LIST* toAddListItem = (EXPR_LIST*)malloc(sizeof(EXPR_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		nodeList->tail = toAddListItem;
		return nodeList;
	}
}

EXPR_LIST* consExprNode(EXPR* toAdd, EXPR_LIST* nodeList)
{
		EXPR_LIST* toAddListItem = (EXPR_LIST*)malloc(sizeof(EXPR_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = nodeList;
		return toAddListItem;
}

/* END Data structures for tree parser */

char* NUMERIC_CHARS = "1234567890";
char* PRIMITIVE_CHARS = "+-/*=";

BOOL isNumber(AST_NODE* node)
{
	if (node->children) 
		return false;

	char* text = getTextForToken(node->payload);
	for (int i = 0; i < strlen(text); i++) {
		if (!contains(NUMERIC_CHARS, text[i]))
			return FALSE;
	}

	return TRUE;
}

BOOL isPrimitive(AST_NODE* node)
{
	return !node->children 
		&& node->payload.length == 1 
		&& contains(PRIMITIVE_CHARS, node->payload.text[0].value);
}


double eval(AST_NODE* astNode)
{
	if(!astNode->children) { // it's an atom
		if (isNumber(astNode)) {
			return atof(getTextForToken(astNode->payload));
		}
	}
	else { // it's a combination only 2 args for now

		AST_NODE* headNode = astNode->children->head;

		if (isPrimitive(headNode)) {

			double leftOperand = eval(astNode->children->tail->head);
			double rightOperand = eval(astNode->children->tail->tail->head);

			PRIM_TYPE primType;

			if (headNode->payload.text[0].value == '+') {
				return leftOperand + rightOperand;
			}
			else if (headNode->payload.text[0].value == '-') {
				return leftOperand - rightOperand;
			}
			else if (headNode->payload.text[0].value == '/') {
				return leftOperand / rightOperand;
			}
			else if (headNode->payload.text[0].value == '*') {
				return leftOperand * rightOperand;
			}
		}
	}
}

EXPR* parseTree(AST_NODE* astNode)
{
	EXPR* expr = (EXPR*)malloc(sizeof(EXPR));

	if(!astNode->children) { // it's an atom
		if (isNumber(astNode)) {
			CONST_EXPR* cexpr = (CONST_EXPR*)malloc(sizeof(CONST_EXPR));
			cexpr->type = NUMERIC;
			cexpr->data.numeric = atof(getTextForToken(astNode->payload));

			expr->type = CONSTANT;
			expr->data.constant = cexpr;	
		}
		else {
			VAR_EXPR* vexpr = (VAR_EXPR*)malloc(sizeof(VAR_EXPR));
			vexpr->name = getTextForToken(astNode->payload);
			
			expr->type = VARIABLE;
			expr->data.variable = vexpr;	
		}
	}
	else { // it's a combination
		AST_NODE* headNode = astNode->children->head;
		if (isPrimitive(headNode)) {
			PRIM_APP_EXPR* pexpr = (PRIM_APP_EXPR*)malloc(sizeof(PRIM_APP_EXPR));

			if (headNode->payload.text[0].value == '+') {
				pexpr->primitive = PLUS;
			}
			else if (headNode->payload.text[0].value == '-') {
				pexpr->primitive = MINUS;
			}
			else if (headNode->payload.text[0].value == '/') {
				pexpr->primitive = DIV;
			}
			else if (headNode->payload.text[0].value == '*') {
				pexpr->primitive = MULT;
			}
			else if (headNode->payload.text[0].value == '=') {
				pexpr->primitive = EQUALS;
			}


		}

		EXPR* headExpr = parseTree(headNode);
	}

	return expr;
}

/* ==== END Tree Parser ==== */

/* ==== Interpreter ==== */

void testInterpreter(char* inputString)
{
	int length = strlen(inputString);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(inputString), length, &tokenLength);
	AST_NODE* root = parse(tokens, tokenLength);
	printf("%f", eval(root));
}

/* END Interpreter */

int _tmain(int argc, _TCHAR* argv[])
{
	//printf("%i" ,contains(ATOM_CHARS, ' '));

	char* testString1 = "(+ (fun symA 4 (asdf ss oo6)) (- sym2 4 dfg))";
	char* testString2 = "(define foo (lambda x (+ x 3)))";

	//printTokenizedString(testString1);
	//printTokenizedString(testString2);

	//testTree();

	//testParser(testString1);

	testInterpreter("(* (+ 6 7) (+ (- (* 7 10) 1) 2))");

	char c = (char)getchar();
	return 0;
}

