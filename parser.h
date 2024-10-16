#pragma once

#include "main.h"
#include "files.h"

extern const char* asttext[];
extern const char* tokentext[];

enum TOKENTYPE {
    T_NL = '\n',

	T_RETURN = 'r',
	T_CONTINUE = 'c',
	T_BREAK = 'b',
	T_IF = 'i',
	T_ELIF = 'l',
	T_ELSE = 'e',
	T_FOR = 'f',
	T_WHILE = 'w',

    T_IDENTIFIER = '0',
	T_NUMBER = '1',
	T_FLOAT = '2',
	T_STRING = '3',
	T_MSTRING = '4',

	T_ADD = '+',
    T_SUB = '-',
	T_MUL = '*',
	T_DIV = '/',
	T_MOD = '%',
	T_AND = '&',
	T_OR = '|',
	T_XOR = '^',
	T_LESS = '<',
	T_GREAT = '>',
	T_NOT = '!',
	T_EQ = '=',

    T_ORB = '(',
	T_CRB = ')',
    T_OCB = '{',
    T_CCB = '}',
    T_OSB = '[',
    T_CSB = ']',
	T_DOT = '.',
	T_COMMA = ',',
	T_COLON = ':',
	T_SCOLON = ';',

	T_IMPORT = '#',
    T_EXTERNAL = '~',

	T_TERNARY = '?',
	T_AT = '@',
    T_THIS = '$',
	T_CATNL = '\\',

    T_ADDEQ = 'A',
	T_SUBEQ = 'S',
	T_MULEQ = 'M',
	T_DIVEQ = 'D',
	T_MODEQ = 'O',
	T_ANDEQ = 'N',
	T_OREQ = 'R',
	T_XOREQ = 'X',
	T_LESSEQ = 'L',
	T_GREATEQ = 'G',
	T_NOTEQ = 'T',
	T_EQEQ = 'E',
};

// LEXER STRUCTS

struct token {
    TOKENTYPE type;
    const char* begin;
    const char* end;
    const char* line;
};

struct token_list {
    token t;
    token_list* next;
};

// PARSER STRUCTS

enum VARIABLETYPE {
    V_UNKNOWN,

    V_OBJECT,

    V_FUNCTION,
	V_NUMBER,
	V_FLOAT,
	V_STRING,
	V_ARRAY
};

struct variable {
    VARTYPE type;

    char* name;
    bool constant;

    void* value;
};

struct variable_list {
    variable v;
    variable_list* next;
};

struct Scope {
    union {
        Scope* parent;
        //node* parentnode;
    };
    //std::vector<variable> variables;
	variable_list* variables;
    variable_list* returns;
};

enum NODETYPE {
    AST_UNKNOWN,
    AST_STATEMENT,
    AST_BSTATEMENT, // b for block
    AST_JSTATEMENT, // j for jump
    AST_CSTATEMENT, // c for conditional
    AST_ISTATEMENT, // i for iteration
    AST_EXPRESSION,
    AST_BEXPRESSION, // b for block
    AST_AEXPRESSION, // a for assignment
    AST_FUNCTION,

    AST_IDENTIFIER,
    AST_ARRAY,
    AST_OBJECT
    // Add more node types as needed
};

struct node_list;

#define EXPRTYPE TOKENTYPE

struct node {
    NODETYPE type;
    Scope* scope;
    linked_list<node_list> nodes;
    EXPRTYPE exprtype;
    union {
        void* vptr;
        //const char* strptr;
        node_list* nlptr;
        token_list* tptr;
    };
};

struct node_list {
    node n;
	node_list* next;
};

struct Parser {
    node* root;

    token_list* tokens;
    union {
        //node* node;
        Scope* scope;
    };
    //std::vector<std::pair<char*, node>> ids;

    //std::vector<char*> imports;
    //std::vector<char*> exports;
};

#define RTYPEL node_list*

token_list* tokenize(SFile file);
void parse(Parser& parser, token_list* tokens);

RTYPEL lparse(Parser& parser);
RTYPEL rparse(Parser& parser);

RTYPEL assignmentparse(Parser& parser);
RTYPEL comparisonparse(Parser& parser);
RTYPEL notparse(Parser& parser);
RTYPEL addparse(Parser& parser);
RTYPEL mulparse(Parser& parser);

RTYPEL bracketsparse(Parser& parser);
RTYPEL dotparse(Parser& parser);
RTYPEL idparse(Parser& parser);