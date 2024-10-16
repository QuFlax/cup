#include "main.h"
#include "parser.h"

const char* tokentext[] = {
    "","","","","","","","","","",
    "T_NL",
    "","","","","","","","","","","","","","","","","","","","","","",

    "T_NOT","\"","T_IMPORT","T_THIS","T_MOD","T_AND","'","T_ORB","T_CRB","T_MUL","T_ADD","T_COMMA","T_SUB","T_DOT","T_DIV",

	"T_IDENTIFIER","T_NUMBER","T_FLOAT","T_STRING","T_MSTRING","5","6","7","8","9",

    "T_COLON","T_SCOLON","T_LESS","T_EQ","T_GREAT","T_TERNARY","T_AT",

	"T_ADDEQ","B","C","T_DIVEQ","T_EQEQ","F","T_GREATEQ","H","I","J","K","T_LESSEQ","T_MULEQ","T_ANDEQ","T_MODEQ","P","Q","T_OREQ","T_SUBEQ","T_NOTEQ","U","V","W","T_XOREQ","Y","Z",

    "T_OSB","T_CATNL","T_CSB","T_XOR","_","`",

	"a","T_BREAK","T_CONTINUE","d","T_ELSE","T_FOR","g","h","T_IF","j","k","T_ELIF","m","n","o","p","q","T_RETURN","s","t","u","v","T_WHILE","x","y","z",

    "T_OCB","T_OR","T_CCB","T_EXTERNAL"
};

const TOKENTYPE types1[] = { T_ADD, T_SUB, T_MUL, T_DIV, T_MOD, T_AND, T_OR, T_LESS, T_GREAT, T_XOR, T_NOT, T_EQ };
const TOKENTYPE doubles[12] = { T_ADDEQ, T_SUBEQ, T_MULEQ, T_DIVEQ, T_MODEQ, T_ANDEQ, T_OREQ, T_LESSEQ, T_GREATEQ, T_XOREQ, T_NOTEQ, T_EQEQ };
const TOKENTYPE types2[] = { T_ORB, T_CRB, T_OCB, T_CCB, T_OSB, T_CSB, T_DOT, T_COMMA, T_COLON, T_SCOLON, T_IMPORT, T_TERNARY, T_AT, T_THIS, T_CATNL, T_EXTERNAL };

bool isID(const char c) {
    if (c >= '\0' && c <= '/')
        return false;
    if (c >= ':' && c <= '@')
		return false;
	if (c >= '[' && c <= '^')
		return false;
    if (c >= '{' && c <= 0x7F)
        return false;
    return true;
}

void pushtoken(linked_list<token_list>& list, token_list* item) {
    if (list.begin == nullptr) {
        list.begin = list.end = item;
    } else {
        list.end->next = item;
        list.end = item;
    }
}

void reverse() {
    /*
    ptr = tokens;
    tokens = nullptr;
    while (ptr) {
        token_list* next = ptr->next;
        ptr->next = tokens;
        tokens = ptr;
        ptr = next;
    }
    return tokens;
    */
}

token_list* next(char*& ptr, const char* end, const char*& line) {
    switch (*ptr) {
    case ' ': case '\t': case '\r': case '\0': return nullptr;
    case '\n': {
        token_list* t = new token_list;
        t->t = { T_NL, ptr, ptr + 1, line };
        t->next = nullptr;
        *ptr = '\0';
        line = ptr + 1;
        return t;
    }
    case '"': {
        ptr++; // skip quote
        const char* start = ptr;
        while (ptr < end && *ptr != '"')
            ptr++;
        if (ptr == end)
            ptrError(line, start, "reached end of file");
        //printf("size of multiline string: %i\n", ptr - start);
        token_list* t = new token_list;
        t->t = { T_MSTRING, start, ptr, line };
        t->next = nullptr;
        return t;
    }
    case '\'': {
        ptr++; // skip quote
        const char* start = ptr;
        while (ptr < end && *ptr != '\'' && *ptr != '\n')
            ptr++;
        if (ptr == end)
            ptrError(line, start, "reached end of file");
        if (*ptr != '\'')
            ptrError(line, start, "missing end of string");
        //printf("size of string: %i\n", ptr - start);
		token_list* t = new token_list;
		t->t = { T_STRING, start, ptr, line };
		t->next = nullptr;
		return t;
    }
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
        char* start = ptr++;
        bool isfloat = false;

        if (ptr < end) {
            const char second = *(ptr++);
            switch (second) {
            case 'b': case 'B':
                while (ptr < end && (*ptr == '_' || *ptr == '0' || *ptr == '1')) ptr++;
                break;
            case 'o': case 'O':
                while (ptr < end && (*ptr == '_' || (*ptr >= '0' && *ptr <= '7'))) ptr++;
                break;
            case 'x': case 'X':
                while (ptr < end && (*ptr == '_' || isxdigit(*ptr))) ptr++;
                break;
            default:
                for (ptr = start; ptr < end && (*ptr == '_' || *ptr == '.' || isdigit(*ptr)); ptr++) {
                    if (*ptr == '.') {
                        if (isfloat)
                            ptrError(line, start, "more than one decimal point in float");
                        isfloat = true;
                    }
                }
                break;
            }
        }
        token_list* t = new token_list;
        t->next = nullptr;

        if (isfloat) {
			t->t = { T_FLOAT, start, ptr, line };
        } else {
			t->t = { T_NUMBER, start, ptr, line };
        }

        ptr--;
        return t;
    }
    case '+': case '-': case '*': case '/': case '%': case '&':
    case '|': case '<': case '>': case '^': case '!': case '=': {
        const char* str = "+-*/%&|<>^!=";
        const int index = strchr(str, ptr[0]) - str;
        if ((ptr + 1) == end || ptr[1] != '=') {
            token_list* t = new token_list;
			t->t = { types1[index], ptr, ptr + 1, line };
            t->next = nullptr;
			return t;
        }
		token_list* t = new token_list;
		t->t = { doubles[index], ptr, ptr + 2, line };
		t->next = nullptr;
        ptr++;
        return t;
    }
    case '(': case ')':  case '{': case '}': case '[': case ']':
    case '.': case ',': case ':': case ';': case '#': case '?':
    case '@': case '$': case '\\': case '~': {
        const char* str = "(){}[].,:;#?@$\\~";
        const int index = strchr(str, ptr[0]) - str;
        //tpush(tl, result, types2[index], ptr, nullptr, line);
		token_list* t = new token_list;
		t->t = { types2[index], ptr, ptr + 1, line };
		t->next = nullptr;
		return t;
    }
    default: {
        const char* start = ptr;

        token_list* t = new token_list;
        t->next = nullptr;

#if 1
        for (; ptr < end && isID(*ptr); ptr++);
#else
        for (; ptr < end && (isdigit(*ptr) || isalpha(*ptr) || *ptr == '_' || *ptr == '`' || *ptr < 0); ptr++);
#endif
        {
            if (strpcmp("if", start, ptr)) {
                //tpush(tl, result, T_IF, start, nullptr, line);
				t->t = { T_IF, start, ptr, line };
            }
            else if (strpcmp("for", start, ptr)) {
                //tpush(tl, result, T_FOR, start, nullptr, line);
				t->t = { T_FOR, start, ptr, line };
            }
            else if (strpcmp("elif", start, ptr)) {
                //tpush(tl, result, T_ELIF, start, nullptr, line);
				t->t = { T_ELIF, start, ptr, line };
            }
            else if (strpcmp("else", start, ptr)) {
                //tpush(tl, result, T_ELSE, start, nullptr, line);
				t->t = { T_ELSE, start, ptr, line };
            }
            else if (strpcmp("break", start, ptr)) {
                //tpush(tl, result, T_BREAK, start, nullptr, line);
				t->t = { T_BREAK, start, ptr, line };
            }
            else if (strpcmp("while", start, ptr)) {
                //tpush(tl, result, T_WHILE, start, nullptr, line);
				t->t = { T_WHILE, start, ptr, line };
            }
            else if (strpcmp("return", start, ptr)) {
                //tpush(tl, result, T_RETURN, start, nullptr, line);
				t->t = { T_RETURN, start, ptr, line };
            }
            else if (strpcmp("continue", start, ptr)) {
                //tpush(tl, result, T_CONTINUE, start, nullptr, line);
				t->t = { T_CONTINUE, start, ptr, line };
            }
            else {
                //printf("size of identifier: %i\n", ptr - start);
                //tpush(tl, result, T_IDENTIFIER, start, ptr, line);
				t->t = { T_IDENTIFIER, start, ptr, line };
            }
        }
        ptr--;
        return t;
    }
    }
}

token_list* tokenize(SFile file) {
    linked_list<token_list> tokens = {};

	if (file.encoding != SFE_UTF8) {
        fprintf(stderr, "ERROR: compiler do not work with BOM\n");
        exit(1);
		return nullptr;
	}

    char* ptr = file.data;
    const char* line = file.data;
    const char* end = file.data + file.size;

    for (; ptr < end; ptr++) {
        auto t = next(ptr, end, line);
        if (t != nullptr) {
            pushtoken(tokens, t);
        }
	}
    return tokens.begin;
}