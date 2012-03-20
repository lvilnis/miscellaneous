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

char* ATOM_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890?-=+/*";
char* BOOL_DELIMITER = "#";
char* BOOL_CHARS = "tfTF";
char* QUOTE_CHARS = "'`";
char* STRING_DELIMITER = "\"";
char* LPAREN_CHARS = "(";
char* RPAREN_CHARS = ")";
char* WHITESPACE_CHARS = " \r\n\t";

typedef enum {
	ATOM,
	LPAREN,
	RPAREN,
	WHITESPACE, 
	BOOLEANTKN,
	STRINGTKN,
	QUOTE,
	ENDOFFILE
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

	for(int i = 0; i < length; i++)	
	{
		CHARACTER currentChar;
		currentChar.value = inputBuffer[i];
		currentChar.offset = i;
		output[i] = currentChar;
	}

	return output;
}


char* getTextForToken(TOKEN toPrint)
{
	if(toPrint.text[0].value == ' ')
		return "SPACE";

	char* str = (char*)malloc(sizeof(char) * (toPrint.length));

	for (int i = 0; i < toPrint.length; i++) 
	{
		str[i] = toPrint.text[i].value;
	}

	str[toPrint.length] = (char)0;

	return str;
}

CHARACTER* getCharSubstring(CHARACTER* source, int start, int length)
{
	CHARACTER* dest = (CHARACTER*)malloc(sizeof(CHARACTER) * length);

	for (int i = 0; i < length; i++) 
		dest[i] = source[start + i];

	return dest;
}

TOKEN* lex(CHARACTER* inputChars, int length, int* outputLength)
{
	TOKEN_TYPE currentType;
	CHARACTER currentChar;

	TOKEN* tokens = (TOKEN*)malloc(sizeof(TOKEN) * (length + 1));

	int index = 0;
	int tokenIndex = 0;

	while (index < length) 
	{
		currentChar = inputChars[index];
		
		if(contains(WHITESPACE_CHARS, currentChar.value)) 
		{
			TOKEN whiteSpace;
			whiteSpace.type = WHITESPACE;
			whiteSpace.offset = index;

			while (contains(WHITESPACE_CHARS, currentChar.value) && index < length) 
				currentChar = inputChars[++index];

			whiteSpace.length = index - whiteSpace.offset;
			whiteSpace.text = getCharSubstring(inputChars, whiteSpace.offset, whiteSpace.length);

			tokens[tokenIndex] = whiteSpace;

			tokenIndex++;
		}
		else if(contains(LPAREN_CHARS, currentChar.value)) 
		{
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
		else if(contains(RPAREN_CHARS, currentChar.value)) 
		{
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
		else if(contains(ATOM_CHARS, currentChar.value)) 
		{
			TOKEN atom;
			atom.type = ATOM;
			atom.offset = index;

			while (contains(ATOM_CHARS, currentChar.value) && index < length) 
				currentChar = inputChars[++index];

			atom.length = index - atom.offset;
			atom.text = getCharSubstring(inputChars, atom.offset, atom.length);

			tokens[tokenIndex] = atom;

			tokenIndex++;	
		}
		else if(contains(QUOTE_CHARS, currentChar.value))
		{
			TOKEN quote;
			quote.type = QUOTE;
			quote.offset = index;

			quote.length = 1;
			quote.text = (CHARACTER*)malloc(sizeof(CHARACTER));
			*quote.text = currentChar;

			tokens[tokenIndex] = quote;

			tokenIndex++;
			index++;
		}
		else if(currentChar.value == '\"')
		{
			TOKEN string;
			string.type = STRINGTKN;
			string.offset = index;

			while (index < length && inputChars[index + 1].value != '\"') 
				currentChar = inputChars[++index];

			currentChar = inputChars[index += 2];

			string.length = index - string.offset;
			string.text = getCharSubstring(inputChars, string.offset, string.length);

			tokens[tokenIndex] = string;
			tokenIndex++;	
		}
		else if(currentChar.value == '#')
		{
			TOKEN boolean;
			boolean.type = BOOLEANTKN;
			boolean.offset = index;

			boolean.length = 2;
			boolean.text = getCharSubstring(inputChars, boolean.offset, 2);

			tokens[tokenIndex] = boolean;

			tokenIndex++;
			index+=2;
		}
	}

	TOKEN eof;
	eof.type = ENDOFFILE;
	eof.offset = index;
	eof.length = 0;

	tokens[tokenIndex++] = eof;

	*outputLength = tokenIndex;

	return tokens;
}

/* Printing helpers for lexer */

void print_scanned(CHARACTER* buffer, int length)
{
	for(int i = 0; i < length; i++)	{
		CHARACTER current = buffer[i];
		printf("%i : %c\n", current.offset, current.value);
	}
}
char* getTokenTypeString(TOKEN_TYPE type)
{
	switch(type)
	{
		case ATOM: return "ATOM";
		case LPAREN: return "LPAREN";
		case RPAREN: return "RPAREN";
		case WHITESPACE: return "WHITESPACE";
		case STRINGTKN: return "STRING";
		case BOOLEANTKN: return "BOOLEAN";
		case QUOTE: return "QUOTE";
		case ENDOFFILE: return "EOF";
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

AST_NODE* parse(TOKEN* tokens);
AST_NODE* parseSexpr(TOKEN* tokens);
AST_NODE* parseSexpr(TOKEN* tokens, AST_NODE* rootNode, int* numConsumed);

AST_NODE_LIST* consNode(AST_NODE* toAdd, AST_NODE_LIST* nodeList)
{
		AST_NODE_LIST* toAddListItem = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = nodeList;
		return toAddListItem;
}

AST_NODE* parse(TOKEN* tokens)
{
	AST_NODE* rootNodeForReals = (AST_NODE*)malloc(sizeof(AST_NODE));
	rootNodeForReals->payload.type == LPAREN;
	rootNodeForReals->children = 0;

	//AST_NODE* rootNode = (AST_NODE*)malloc(sizeof(AST_NODE));
	//rootNode->payload.type == LPAREN;
	//rootNode->children = 0;

//	rootNodeForReals->children = consNode(rootNode, rootNodeForReals->children);

	int* numConsumed = (int*)malloc(sizeof(int));
	*numConsumed = 0;
	AST_NODE* populatedRoot = parseSexpr(tokens, rootNodeForReals, numConsumed);
	return populatedRoot;
}

AST_NODE* parseSexpr(TOKEN* tokens, AST_NODE* rootNode, int* numConsumed)
{
	AST_NODE_LIST* parentStack = (AST_NODE_LIST*)malloc(sizeof(AST_NODE_LIST));
	parentStack->head = rootNode;
	parentStack->tail = 0;
	do
	{
		TOKEN currentToken = tokens[(*numConsumed)++];

		if (currentToken.type == RPAREN) 
		{
			parentStack = parentStack->tail; // pop
		}
		else if (currentToken.type == QUOTE)
		{
			AST_NODE* newASTNode = (AST_NODE*)malloc(sizeof(AST_NODE));
			newASTNode->payload = currentToken;
			newASTNode->children = 0;
			parseSexpr(tokens, newASTNode, numConsumed);

			parentStack->head->children = appendNode(parentStack->head->children, newASTNode);
		}
		else if (currentToken.type != WHITESPACE && currentToken.type != ENDOFFILE)
		{
			AST_NODE* newASTNode = (AST_NODE*)malloc(sizeof(AST_NODE));
			newASTNode->payload = currentToken;
			newASTNode->children = 0;
			
			parentStack->head->children = appendNode(parentStack->head->children, newASTNode);

			if (currentToken.type == LPAREN)
			{
				parentStack = consNode(newASTNode, parentStack); // push
			}
		}
	}
	while (parentStack->tail);

	// assume the fake "root" node has exactly one child for now
	return parentStack->head;
}

void testParser(char* toParse)
{
	toParse = strcat("(",strcat(toParse, ")"));
	int length = strlen(toParse);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(toParse), length, &tokenLength);
	AST_NODE* root = parse(tokens);
	printTree(*root);
}

/* ==== END Parser ==== */

/* ==== Tree Parser ==== */

/* Data structures for tree parser */

typedef struct lisp_data LISP_DATA;
typedef struct lisp_data_list LISP_DATA_LIST;

struct lisp_data_list {
	LISP_DATA* head;
	LISP_DATA_LIST* tail;
};

typedef enum {
	NUMERIC,
	STRING,
	SYMBOL, 
	BOOLEAN,
	QUOTED,
	LIST
} DATA_TYPE;

typedef struct lisp_data {
	DATA_TYPE type;
	union {
		double numeric;
		char* symbol;
		char* string;
		BOOL boolean;
		LISP_DATA_LIST* list;
		LISP_DATA* quoted;
	} data;
};

LISP_DATA_LIST* appendExprNode(LISP_DATA_LIST* nodeList, LISP_DATA* toAdd)
{
	if(!nodeList) 
	{
		LISP_DATA_LIST* toAddListItem = (LISP_DATA_LIST*)malloc(sizeof(LISP_DATA_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		return toAddListItem;
	}
	if (nodeList->tail) 
	{
		appendExprNode(nodeList->tail, toAdd);
		return nodeList;
	}
	else 
	{
		LISP_DATA_LIST* toAddListItem = (LISP_DATA_LIST*)malloc(sizeof(LISP_DATA_LIST));
		toAddListItem->head = toAdd;
		toAddListItem->tail = 0;
		nodeList->tail = toAddListItem;
		return nodeList;
	}
}

LISP_DATA_LIST* consExprNode(LISP_DATA* toAdd, LISP_DATA_LIST* nodeList)
{
		LISP_DATA_LIST* toAddListItem = (LISP_DATA_LIST*)malloc(sizeof(LISP_DATA_LIST));
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
	for (int i = 0; i < strlen(text); i++) 
	{
		if (!contains(NUMERIC_CHARS, text[i]))
			return FALSE;
	}

	return TRUE;
}

LISP_DATA* parseTree(AST_NODE* astNode)
{
	LISP_DATA* currentData = (LISP_DATA*)malloc(sizeof(LISP_DATA));

	if (astNode->payload.type == LPAREN) 
	{
		currentData->type = LIST;
		currentData->data.list = 0;
		AST_NODE_LIST* childList = astNode->children;
		while (childList) 
		{
			currentData->data.list = appendExprNode(currentData->data.list, parseTree(childList->head));
			childList = childList->tail;
		}
	}
	else if (astNode->payload.type == QUOTE)
	{
		currentData->type = QUOTED;
		currentData->data.quoted = parseTree(astNode->children->head);
	}
	else if (astNode->payload.type == ATOM)
	{
		if (isNumber(astNode)) 
		{
			currentData->type = NUMERIC;
			currentData->data.numeric = atof(getTextForToken(astNode->payload));
		}
		else 
		{
			currentData->type = SYMBOL;
			currentData->data.symbol = getTextForToken(astNode->payload);
		}
	}
	else if (astNode->payload.type == BOOLEANTKN)
	{
		currentData->type = BOOLEAN;
		currentData->data.boolean = !stricmp(getTextForToken(astNode->payload), "#t");
	}
	else if (astNode->payload.type == STRINGTKN)
	{
		currentData->type = STRING;
		currentData->data.string = getTextForToken(astNode->payload);
	}

	return currentData;
}

void printLispData(LISP_DATA* data)
{
	if(!data) 
	{
		printf("()");
		return;
	}

	if (data->type == NUMERIC) 
	{
		printf("%f", data->data.numeric);
	}
	else if (data->type == SYMBOL) 
	{
		printf("%s", data->data.symbol);
	}
	else if (data->type == STRING) 
	{
		printf("%s", data->data.string);
	}
	else if (data->type == BOOLEAN) 
	{
		printf(data->data.boolean ? "#t" : "#f");
	}
	else if (data->type == QUOTED) 
	{
		printf("`");
		printLispData(data->data.quoted);
	}
	else if (data->type == LIST) 
	{
		printf("(");

		LISP_DATA_LIST* currentItem = data->data.list;
		while (currentItem)
		{
			printLispData(currentItem->head);
			currentItem = currentItem->tail;
			if (currentItem) printf(" ");
		}

		printf(")");
	}
}

void printLispDataList(LISP_DATA_LIST* list)
{
	if (!list) return;

	while (list)
	{
		printLispData(list->head);
		list = list->tail;
	}
}

void testTreeParser(char* inputString)
{
	int length = strlen(inputString);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(inputString), length, &tokenLength);
	AST_NODE* root = parse(tokens);
	LISP_DATA* data = parseTree(root);
	printLispData(data);
}

/* ==== END Tree Parser ==== */

/* ==== Interpreter ==== */

LISP_DATA_LIST* evlist(LISP_DATA_LIST* list, LISP_DATA* env);
LISP_DATA* eval(LISP_DATA* exp, LISP_DATA* env);
LISP_DATA* apply(LISP_DATA* proc, LISP_DATA_LIST* args);

LISP_DATA_LIST* evlist(LISP_DATA_LIST* list, LISP_DATA* env)
{
	LISP_DATA_LIST* evaluated = 0;

	LISP_DATA_LIST* current = list;
	while (current) 
	{
		evaluated = appendExprNode(evaluated, eval(current->head, env));
		current = current->tail;
	}

	return evaluated;
}

BOOL isPrimitive(LISP_DATA* exp)
{
	return exp->type == SYMBOL 
			&& strlen(exp->data.symbol) == 1 
			&& contains(PRIMITIVE_CHARS, exp->data.symbol[0]);
}

LISP_DATA* bindIdentifier(LISP_DATA* env, LISP_DATA* name, LISP_DATA* value)
{
	LISP_DATA_LIST* newList = 0;
	newList = consExprNode(name, consExprNode(value, newList));

	LISP_DATA* newFrame = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	newFrame->type = LIST;
	newFrame->data.list = newList;
	
	LISP_DATA* newEnv = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	newEnv->type = LIST;
	newEnv->data.list = env ? consExprNode(newFrame, env->data.list) : consExprNode(newFrame, 0);

	return newEnv;
}

LISP_DATA* popBinding(LISP_DATA* env)
{
	LISP_DATA* newEnv = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	newEnv->type = LIST;
	newEnv->data.list = env->data.list->tail;

	return newEnv;
}

LISP_DATA* lookup(LISP_DATA* symbol, LISP_DATA* env)
{
	LISP_DATA_LIST* currentFrame = env->data.list; 
	while (currentFrame) 
	{
		if(currentFrame->head->data.list->head->data.symbol[0] == symbol->data.symbol[0]) 
		{
			return currentFrame->head->data.list->tail->head;
		}
		currentFrame = currentFrame->tail;
	}
}

LISP_DATA* makeClosure(LISP_DATA_LIST* args, LISP_DATA* env)
{
	LISP_DATA* closureString = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	closureString->type = SYMBOL;
	closureString->data.symbol = "closure";

	LISP_DATA* argsList = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	argsList->type = LIST;
	argsList->data.list = args;

	LISP_DATA_LIST* closure = consExprNode(closureString, consExprNode(argsList, consExprNode(env, 0)));

	LISP_DATA* closureData = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	closureData->type = LIST;
	closureData->data.list = closure;

	return closureData;

	// need to make a data copy function
}

LISP_DATA* eval(LISP_DATA* exp, LISP_DATA* env)
{
	printf("eval: ");
	printLispData(exp);
	printf("\n");

	if (exp->type == LIST)
	{
		if (!exp->data.list) 
			return exp;

		LISP_DATA* car = exp->data.list->head;

		if (isPrimitive(car)) 
		{
			return apply(car, evlist(exp->data.list->tail, env));
		}
		else if (car->type == SYMBOL && !stricmp(car->data.symbol, "define")) 
		{
			LISP_DATA_LIST* toBind = exp->data.list->tail->head->data.list;
			env = bindIdentifier(env, toBind->head, eval(toBind->tail->head, env));
			return 0;
		}
		else if (car->type == SYMBOL && !stricmp(car->data.symbol, "let")) 
		{
			LISP_DATA_LIST* toBind = exp->data.list->tail->head->data.list;
			env = bindIdentifier(env, toBind->head, eval(toBind->tail->head, env));
			LISP_DATA* body = eval(exp->data.list->tail->tail->head, env);
			env = popBinding(env);
			return body;
		}
		else if (car->type == SYMBOL && !stricmp(car->data.symbol, "lambda")) 
		{
			return makeClosure(exp->data.list->tail, env);
		}
		else 
		{
			return apply(eval(car, env), evlist(exp->data.list->tail, env));
		}
	}
	if (exp->type == SYMBOL) 
	{
		return lookup(exp, env);
	}
	if (exp->type == QUOTED) 
	{
		return exp->data.quoted;
	}

	return exp;
}

LISP_DATA* apply(LISP_DATA* proc, LISP_DATA_LIST* args)
{
	printf("apply: ");
	printLispData(proc);
	printf(" to ");
	LISP_DATA* wrappedArgs = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	wrappedArgs->type = LIST;
	wrappedArgs->data.list = args;
	printLispData(wrappedArgs);
	printf("\n");

	if (proc->type == LIST 
		&& proc->data.list->head->type == SYMBOL 
		&& !stricmp(proc->data.list->head->data.symbol, "closure")) 
	{
			LISP_DATA_LIST* nameToBind = proc->data.list->tail->head->data.list->head->data.list;
			LISP_DATA_LIST* valueToBind = args;

			LISP_DATA* env =  proc->data.list->tail->tail->head;
			LISP_DATA* body = proc->data.list->tail->head->data.list->tail->head;

			while (nameToBind) 
			{
				env = bindIdentifier(env, nameToBind->head, valueToBind->head);
				nameToBind = nameToBind->tail;
				valueToBind = valueToBind->tail;
			}

			return eval(body, env);
	}

	if (isPrimitive(proc)) 
	{
		LISP_DATA* newNumber = (LISP_DATA*)malloc(sizeof(LISP_DATA));
		newNumber->type = NUMERIC;
		LISP_DATA_LIST* arg = args;

		switch (proc->data.symbol[0])
		{
			case '+':	
				newNumber->data.numeric = 0;
				while (arg) 
				{
					newNumber->data.numeric += arg->head->data.numeric;
					arg = arg->tail;
				}
				break;
			case '*':	
				newNumber->data.numeric = 1;
				while (arg) 
				{
					newNumber->data.numeric *= arg->head->data.numeric;
					arg = arg->tail;
				}
				break;
			case '-':	
				newNumber->data.numeric = arg->head->data.numeric;
				arg = arg->tail;
				while (arg) 
				{
					newNumber->data.numeric -= arg->head->data.numeric;
					arg = arg->tail;
				}
				break;
			case '/':	
				newNumber->data.numeric = arg->head->data.numeric;
				arg = arg->tail;
				while (arg) 
				{
					newNumber->data.numeric /= arg->head->data.numeric;
					arg = arg->tail;
				}
				break;
		}
		
		//printf("operator: %c left: %f right:%f new number: %f\n",
		//	proc->data.string[0],
		//	args->head->data.numeric, 
		//	args->tail->head->data.numeric,
		//	newNumber->data.numeric);

		return newNumber;
	}

	return 0;
}

LISP_DATA_LIST* evalProgram(LISP_DATA* root)
{
	return evlist(root->data.list, 0);
}

void testInterpreter(char* inputString)
{
	int length = strlen(inputString);
	int tokenLength = 0;
	TOKEN* tokens = lex(scan(inputString), length, &tokenLength);
	AST_NODE* root = parse(tokens);

	LISP_DATA* rootList = (LISP_DATA*)malloc(sizeof(LISP_DATA));
	rootList->type = LIST;
	rootList->data.list = 0;

	AST_NODE_LIST* currentChild = root->children;

	while (currentChild)
	{
		rootList->data.list = consExprNode(parseTree(currentChild->head), rootList->data.list);
		currentChild = currentChild->tail;
	}

	LISP_DATA_LIST* evaluated = evalProgram(rootList);

	printLispDataList(evaluated);
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

	//testInterpreter("(* (+ 6 7) (+ (- (* 7 10) 1) 2))");
	testInterpreter("((lambda () ((((lambda (y) (lambda (x) (lambda (z) (+ x y z)))) 10) 67) 6)) ()) `(a)");
	//testInterpreter("`\"hello world\"");
	//testTreeParser("(* (+ 6 7) (+ (- (* 7 10) 1) 2))");

	char c = (char)getchar();
	return 0;
}

 