enum {
  T_EOF = '\0',
  N_END = T_EOF,

  T_NL = '\n',
  N_NEWLINE = T_NL,
  T_CATNL = '\\',

  T_RETURN = 'r',
  N_RETURN = T_RETURN,
  T_CONTINUE = 'n',
  N_CONTINUE = T_CONTINUE,
  T_BREAK = 'b',
  N_BREAK = T_BREAK,
  T_IF = 'i',
  N_IF = T_IF,
  T_ELSE = 'e',
  N_IFELSE = T_ELSE,
  T_FOR = 'f',
  N_FOR = T_FOR,
  T_WHILE = 'w',
  N_WHILE = T_WHILE,

  T_IDENTIFIER = '0',
  N_VARIABLE = T_IDENTIFIER,
  T_NUMBER = '1',
  N_NUMBER = T_NUMBER,
  T_DOUBLE = '2',
  N_DOUBLE = T_DOUBLE,
  T_STRING = '3',
  N_STRING = T_STRING,
  T_MSTRING = '4',
  N_MSTRING = T_MSTRING,
  T_RANGE = '5',
  N_RANGE = T_RANGE,

  T_ADD = '+',
  N_ADD = T_ADD,
  T_SUB = '-',
  N_SUB = T_SUB,
  T_MUL = '*',
  N_MUL = T_MUL,
  T_DIV = '/',
  N_DIV = T_DIV,
  T_MOD = '%',
  N_MOD = T_MOD,
  T_AND = '&',
  N_AND = T_AND,
  T_OR = '|',
  N_OR = T_OR,
  T_XOR = '^',
  N_XOR = T_XOR,
  T_LESS = '<',
  N_LESS = T_LESS,
  T_GREAT = '>',
  N_GREAT = T_GREAT,
  T_NOT = '!',
  N_NOT = T_NOT,
  T_EQ = '=',
  N_ASSIGN = T_EQ,

  T_ORB = '(',
  T_CRB = ')',
  T_OCB = '{',
  T_CCB = '}',
  T_OSB = '[',
  T_CSB = ']',

  N_CALL = 'c',
  N_UNARY = 'u',
  N_BLOCK = 'l',
  N_OBJECT = 'o',
  N_SUBSCRIPT = 's',
  N_ARRAY = 'a',
  T_DOT = '.',
  N_MEMBER = T_DOT,
  T_COMMA = ',',
  N_COMMA = T_COMMA,
  T_COLON = ':',
  T_SCOLON = ';',

  T_IMPORT = '#',
  T_EXTERNAL = '~',

  T_ASK = '?',
  T_AT = '@',
  N_FUNCTION = T_AT,
  T_THIS = '$',
  T_VARG = 'V',
  N_VARG = T_VARG,

  T_ADDEQ = 'A',
  N_ADDASSIGN = T_ADDEQ,
  T_SUBEQ = 'S',
  N_SUBASSIGN = T_SUBEQ,
  T_MULEQ = 'M',
  N_MULASSIGN = T_MULEQ,
  T_DIVEQ = 'D',
  N_DIVASSIGN = T_DIVEQ,
  T_MODEQ = 'O',
  N_MODASSIGN = T_MODEQ,
  T_ANDEQ = 'N',
  N_ANDASSIGN = T_ANDEQ,
  T_OREQ = 'R',
  N_ORASSIGN = T_OREQ,
  T_XOREQ = 'X',
  N_XORASSIGN = T_XOREQ,
  T_LESSEQ = 'L',
  N_LESSEQ = T_LESSEQ,
  T_GREATEQ = 'G',
  N_GREATEQ = T_GREATEQ,
  T_NOTEQ = 'T',
  N_NOTEQ = T_NOTEQ,
  T_EQEQ = 'E',
  N_EQEQ = T_EQEQ,
};
typedef uint16_t CTType;

#define NODEFMT "%s(%d:%d)[%zu]"
#define NODEFMTV(x) CTType_name((x).type), (x).loc.line, (x).loc.col, (x).value

static const char *CTType_name(CTType t) {
  switch (t) {
  case N_END:
    return "EOF END";
  case N_NEWLINE:
    return "NEWLINE";
  case T_CATNL:
    return "CAT";
  case N_RETURN:
    return "RETURN";
  case N_CONTINUE:
    return "CONTINUE";
  case N_BREAK:
    return "BREAK";
  case N_IF:
    return "IF";
  case N_IFELSE:
    return "IF&ELSE";
  case N_FOR:
    return "FOR";
  case N_WHILE:
    return "WHILE";

  case N_VARIABLE:
    return "VARIABLE";
  case N_NUMBER:
    return "NUMBER";
  case N_DOUBLE:
    return "DOUBLE";
  case N_STRING:
    return "STRING";
  case N_MSTRING:
    return "MSTRING";
  case N_RANGE:
    return "RANGE";

  case N_ADD:
    return "ADD";
  case N_SUB:
    return "SUB";
  case N_MUL:
    return "MUL";
  case N_DIV:
    return "DIV";
  case N_MOD:
    return "MOD";
  case N_AND:
    return "AND";
  case N_OR:
    return "OR";
  case N_XOR:
    return "XOR";
  case N_LESS:
    return "LESS";
  case N_GREAT:
    return "GREAT";
  case N_NOT:
    return "NOT";
  case N_ASSIGN:
    return "ASSIGN";

  case T_ORB:
    return "'('";
  case T_CRB:
    return "')'";
  case T_OSB:
    return "'['";
  case T_CSB:
    return "']'";
  case T_OCB:
    return "'{'";
  case T_CCB:
    return "'}'";

  case N_CALL:
    return "CALL";
  case N_UNARY:
    return "UNARY";
  case N_BLOCK:
    return "BLOCK";
  case N_OBJECT:
    return "OBJECT";
  case N_SUBSCRIPT:
    return "SUBSCRIPT";
  case N_ARRAY:
    return "ARRAY";
  case N_MEMBER:
    return "MEMBER";
  case N_COMMA:
    return "COMMA";

  case T_COLON:
    return "COLON";
  case T_SCOLON:
    return "SCOLON";
  case T_IMPORT:
    return "IMPORT";
  case T_EXTERNAL:
    return "EXTERNAL";
  case T_ASK:
    return "ASK";
  case N_FUNCTION:
    return "FUNCTION";
  case T_THIS:
    return "THIS";
  case N_VARG:
    return "VARG";

  case N_ADDASSIGN:
    return "ADDEQ";
  case N_SUBASSIGN:
    return "SUBEQ";
  case N_MULASSIGN:
    return "MULEQ";
  case N_DIVASSIGN:
    return "DIVEQ";
  case N_MODASSIGN:
    return "MODEQ";
  case N_ANDASSIGN:
    return "ANDEQ";
  case N_ORASSIGN:
    return "OREQ";
  case N_XORASSIGN:
    return "XOREQ";
  case N_LESSEQ:
    return "LESSEQ";
  case N_GREATEQ:
    return "GREATEQ";
  case N_NOTEQ:
    return "NOTEQ";
  case N_EQEQ:
    return "EQEQ";
  }

  return "<UNKNOWN>";
}
