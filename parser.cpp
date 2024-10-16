#include "parser.h"

const char* asttext[] = {
    "AST_UNKNOWN",
    "AST_STATEMENT",
    "AST_BSTATEMENT", // b for block
    "AST_JSTATEMENT", // j for jump
    "AST_CSTATEMENT", // c for conditional
    "AST_ISTATEMENT", // i for iteration
    "AST_EXPRESSION",
    "AST_BEXPRESSION", // b for block
    "AST_AEXPRESSION", // a for assignment
    "AST_FUNCTION",

    "AST_IDENTIFIER",
    "AST_ARRAY",
    "AST_OBJECT"
};

void pushnode(linked_list<node_list>& list, node_list* item) {
    if (list.begin == nullptr) {
        list.begin = list.end = item;
    } else {
        list.end->next = item;
        list.end = item;
    }
}

inline const token postnext(token_list*& tokens) {
    const token t = tokens->t;
    tokens = tokens->next;
    return t;
}

inline variable& addvariable(variable_list*& variables, char* name, VARIABLETYPE type) {
    variable_list* temp = variables;

    for (auto it = temp; it; it = it->next) {
        if (strcmp(it->v.name, name) == 0) {
            if (it->v.type != type) {
                ptrError(0, 0, "variable already defined with different type");
            }
            printf("variable already defined\n");
            return it->v;
        }
    }


    printf("ASSIGNMENT: %s\n", name);


    variables = new variable_list();
    variables->next = temp;

    variables->v.name = name;
	variables->v.type = type;

    return variables->v;
}

bool skip(token_list*& tokens) {
    if (tokens == nullptr)
        return false;

    if (tokens->t.type == T_NL) {
        for (; tokens; tokens = tokens->next) {
            if (tokens->t.type != T_NL)
                break;
        }
    }
    return tokens != nullptr;
}

//#define pushlparse(nodes, tokens) push(nodes, lparse(tokens));
//#define pushrparse(nodes, tokens) push(nodes, rparse(tokens));

// TODO: ADD THIS FILE TO OBJECTS OR SOMETHING
void parse(Parser& parser, token_list* tokens) {
    if (parser.tokens != nullptr)
        printf("parse: parser.tokens != nullptr\n");

    if (tokens == nullptr)
        return;

    if (parser.root == nullptr) {
        parser.root = new node;
        parser.root->type = AST_BSTATEMENT;
        parser.root->nodes = {};
        
        //parser.root->exprtype = nullptr;

		parser.root->scope = new Scope();
        parser.root->scope->parent = nullptr;
        parser.root->scope->variables = nullptr;
    }

    parser.tokens = tokens;
    parser.scope = parser.root->scope;
    auto& nodes = parser.root->nodes;

    while (skip(parser.tokens)) {
        auto n = lparse(parser);
        pushnode(nodes, n);
        const char* type = asttext[n->n.type];
        printf("%s\n", type);
    }

    printf("\nend parse\n");
    // optimize

    for (auto it = nodes.begin; it != nullptr; it = it->next) {
        //const char* type = asttext[it->n.type];
        //printf("%s\n", type);
    }

}

/*
void hasImport(Parser parser) {
    for (auto it = parser.imports.cbegin(); it != parser.imports.cend(); it++) {
        if (strpcmp(*it, parser.tokens->t.begin, parser.tokens->t.end))
            return; //duplicate
    }
    const size_t len = parser.tokens->t.end - parser.tokens->t.begin;
    char* name = new char[len + 1];
    name[len] = '\0';
    memcpy(name, parser.tokens->t.begin, len);
    parser.imports.push_back(name);
}

void addImport(Parser& parser) {
    auto t = postnext(parser.tokens); // skip 'import'

    auto& tokens = parser.tokens;

    if (tokens == nullptr || tokens->t.type == T_NL)
        ptrError(t.line, t.begin, "unexpected end of import");

    while (skip(tokens)) {
        if (tokens->t.type != T_STRING && tokens->t.type != T_IDENTIFIER)
            ptrError(tokens->t.line, tokens->t.begin, "not a string or identifier");

        hasImport(parser);

        tokens = tokens->next; // T_STRING or T_IDENTIFIER

        if (tokens == nullptr || tokens->t.type == T_NL)
            return;

        if (tokens->t.type != T_COMMA)
            ptrError(tokens->t.line, tokens->t.begin, "missing comma"); // unexpected token

        tokens = tokens->next; // T_COMMA
    }

    ptrError(t.line, t.begin, "unexpected end of file");
}

void addExternal(Parser& parser, token_list*& tokens) {
    auto t = postnext(tokens); // skip 'external'

    if (tokens == nullptr || tokens->t.type == T_NL)
        return;

    ptrError(t.line, t.begin, "not implemented");
    //TODO add external function from dll

    /*while (skip(tokens)) {
        if (tokens->t.type != T_STRING && tokens->t.type != T_IDENTIFIER)
            ptrError(tokens->t.line, tokens->t.begin, "not a string or identifier");

        hasExternal(parser, tokens);
    }*
}
*/

RTYPEL lparse(Parser& parser) { // id()[] | id() | id[] = ... | id = ...

	auto& tokens = parser.tokens;

    switch (tokens->t.type) {
    case T_OCB: {
        auto t = postnext(tokens); // skip '{'

        if (!skip(tokens))
            ptrError(t.line, t.begin, "unexepted end of body");

        auto nl = new node_list();
        nl->n.type = AST_BSTATEMENT;
		auto& nodes = nl->n.nodes;

        while (tokens->t.type != T_CCB) {
            pushnode(nodes, lparse(parser));;

            if (!skip(tokens))
                ptrError(t.line, t.begin, "unexepted end of body");
        }

        tokens = tokens->next; // skip '}'

        return nl;
    }
    case T_RETURN: {
        token t = postnext(tokens); // skip 'return'

        auto nl = new node_list();
        nl->n.type = AST_JSTATEMENT;
        nl->n.exprtype = T_RETURN;
        auto& nodes = nl->n.nodes;

        auto right = rparse(parser);

        pushnode(nodes, right);

        VARIABLETYPE type = V_UNKNOWN;
        switch (right->n.type) {
        default:
            fprintf(stderr, "%s ", asttext[right->n.type]);
            ptrError(t.line, t.begin, "invalid return value");
            break;

        case AST_IDENTIFIER: {
            t = right->n.tptr->t;
            switch (t.type) {
            default:
                fprintf(stderr, "%s ", tokentext[t.type]);
                ptrError(t.line, t.begin, "invalid return value");
                break;

            case T_STRING: type = V_STRING; break;
            case T_NUMBER: type = V_NUMBER; break;
            case T_FLOAT: type = V_FLOAT; break;
            }

        } break;
        case AST_ARRAY: type = V_ARRAY; break;

        }

        auto& v = addvariable(parser.scope->variables, (char*)"return", type);

        return nl;
    }
    case T_BREAK: {
        auto t = postnext(tokens); // skip 'continue'
        auto nl = new node_list();
        nl->n.type = AST_JSTATEMENT;
        nl->n.exprtype = T_BREAK;
        return nl;
    }
	case T_CONTINUE: {
        auto t = postnext(tokens); // skip 'continue'
        auto nl = new node_list();
        nl->n.type = AST_JSTATEMENT;
        nl->n.exprtype = T_CONTINUE;
        return nl;
    }
    case T_IF: {
        auto t = postnext(tokens); // skip 'if'

        if (!skip(tokens))
            ptrError(t.line, t.begin, "unexepted end of statement");

        if (tokens->t.type != T_ORB)
            ptrError(t.line, t.begin, "exepted '(' after 'if'");

        auto orb = postnext(tokens); // skip '('

        auto nl = new node_list();
        nl->n.type = AST_CSTATEMENT;
        nl->n.exprtype = T_IF;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, rparse(parser));;

        if (!skip(tokens))
            ptrError(orb.line, orb.begin, "unexepted end of condition");

        if (tokens->t.type != T_CRB)
            ptrError(tokens->t.line, tokens->t.begin, "unexepted end of condition");

        auto crb = postnext(tokens); // skip ')'

        if (!skip(tokens))
            ptrError(crb.line, crb.begin, "unexepted end of body");

        pushnode(nodes, lparse(parser));;

        // TODO ADD ELSE / ELSE IF

        return nl;
    }
    case T_FOR: {
        auto t = postnext(tokens); // skip 'for'

		if (!skip(tokens))
            ptrError(t.line, t.begin, "unexepted end of statement");

		if (tokens->t.type != T_ORB)
			ptrError(t.line, t.begin, "exepted '(' after 'for'");

		auto orb = postnext(tokens); // skip '('

        if (tokens->t.type != T_IDENTIFIER)
			ptrError(tokens->t.line, tokens->t.begin, "exepted identifier after '('");

		auto id = postnext(tokens); // skip 'id'

        if (tokens->t.type != T_COLON)
			ptrError(tokens->t.line, tokens->t.begin, "exepted ':' after 'id'");
        
        tokens = tokens->next; // skip ':'

        auto nl = new node_list();
        nl->n.type = AST_ISTATEMENT;
        nl->n.exprtype = T_FOR;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, rparse(parser));

		if (tokens->t.type != T_CRB)
			ptrError(tokens->t.line, tokens->t.begin, "unexepted end of condition");

		auto crb = postnext(tokens); // skip ')'

		if (!skip(tokens))
			ptrError(crb.line, crb.begin, "unexepted end of body");

        pushnode(nodes, lparse(parser));

        return nl;
    }
    case T_WHILE: {
        ptrError(tokens->t.line, tokens->t.begin, "not implemented");
    }
    case T_IMPORT: {
        ptrError(tokens->t.line, tokens->t.begin, "not implemented");
    }
    case T_EXTERNAL: {
		ptrError(tokens->t.line, tokens->t.begin, "not implemented");
    }
    case T_IDENTIFIER: {
        Parser copy = parser; // copy
        auto& ptr = copy.tokens;
        auto id = idparse(copy);

        if (ptr && ptr->t.type == T_ORB) {
            auto t = postnext(ptr); // skip '('
            
            auto nl = new node_list();
            nl->n.type = AST_FUNCTION;
            nl->n.scope = new Scope();
            nl->n.scope->parent = copy.scope;
            copy.scope = nl->n.scope;
            auto& nodes = nl->n.nodes;

            auto args = rparse(copy);

			if (ptr->t.type != T_CRB)
				ptrError(t.line, t.begin, "unexepted end of args expect ')'");

			auto crb = postnext(ptr); // skip ')'

            if (skip(ptr) && ptr->t.type == T_OCB) {

                auto body = lparse(copy);

                pushnode(nodes, body);

                if (args != nullptr)
                    pushnode(nodes, args);

                copy.scope = copy.scope->parent;

                parser = copy;

                auto t = id->n.tptr->t;

                auto size = t.end - t.begin;
                char* name = new char[size + 1];
				memcpy(name, t.begin, size);
				name[size] = '\0';

                
                auto& v = addvariable(parser.scope->variables, name, V_FUNCTION);

                return nl;
            }
            printf("possiable memory leak!\n");
            auto cur = nl->n.scope->variables;
            while (cur != nullptr) {
                auto next = cur->next;
                delete cur;
				cur = next;
            }
            delete nl->n.scope; // delete scope
            delete nl; // delete node
        }
        printf("possiable memory leak!\n");

        const size_t len = id->n.tptr->t.end - id->n.tptr->t.begin;
        char* buff = new char[len + 1];
        buff[len] = '\0';
        memcpy(buff, id->n.tptr->t.begin, len);
        
        printf("lparse: T_IDENTIFIER: %s\n", id->n.tptr->t.begin);
        delete[] buff;
    } [[fallthrough]]
    default:
        return rparse(parser);
    }
}

RTYPEL rparse(Parser& parser) {
    auto left = assignmentparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("rparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_COMMA) {
        tokens = tokens->next; // skip ','

        auto nl = new node_list();
        nl->n.type = AST_BEXPRESSION;
        nl->n.exprtype = T_COMMA;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        while (skip(tokens)) {
            pushnode(nodes, rparse(parser));;

            if (tokens->t.type != T_COMMA)
				break;
        }

        return nl;
    }

    return left;
}

RTYPEL assignmentparse(Parser& parser) {
    auto left = comparisonparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("assignmentparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_EQ ||
        tokens->t.type == T_ADDEQ || tokens->t.type == T_SUBEQ ||
        tokens->t.type == T_MULEQ || tokens->t.type == T_DIVEQ ||
        tokens->t.type == T_MODEQ || tokens->t.type == T_ANDEQ ||
        tokens->t.type == T_OREQ || tokens->t.type == T_XOREQ)
    {
        token t = postnext(tokens);

        auto nl = new node_list();
        nl->n.type = AST_AEXPRESSION;
        nl->n.exprtype = t.type;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        auto right = assignmentparse(parser);

        pushnode(nodes, right);

        if (left->n.type != AST_IDENTIFIER) {
            ptrError(t.line, t.begin, "that assignment not available yet");
        }

        VARIABLETYPE type = V_UNKNOWN;
        char* name = nullptr;

        auto node = left->n;

        switch (left->n.type) {
        case AST_IDENTIFIER: {
            auto t = node.tptr->t;

            auto size = t.end - t.begin;
            name = new char[size + 1];
            memcpy(name, t.begin, size);
            name[size] = '\0';
            
            switch (right->n.type) {
            case AST_ARRAY: type = V_ARRAY; break;
            case AST_OBJECT: type = V_OBJECT; break;
            case AST_IDENTIFIER: {
                switch (right->n.tptr->t.type) {
                case T_NUMBER: type = V_NUMBER; break;
                default:
                    fprintf(stderr, "%s\n", tokentext[right->n.tptr->t.type]);
                    ptrError(0, 0, "assignmentparse not implemented yet");
                    break;
                }
            } break;
            default:
                fprintf(stderr, "%s\n", asttext[right->n.type]);
                ptrError(0, 0, "assignmentparse not implemented yet");
                break;
            }
        } break;
        /*case AST_EXPRESSION: {
            switch (left->n.exprtype) {
                case T_DOT: {
                    if (left->n.nodes.end && left->n.nodes.end->n.type == AST_IDENTIFIER) {
                        auto t = left->n.nodes.end->n.tptr->t;
                        auto size = t.end - t.begin;
                        char* name = new char[t.end - t.begin + 1];
                        memcpy(name, t.begin, size);
                        name[size] = '\0';
                        printf("ASSIGNMENT: %s\n", name);
                        parser.scope->variables->v.name = name;
                        //parser.scope->variables->v.function = false;
                    }
                } break;
                default: break;
            }
        } break;*/
        default: break;
        }


        auto& v = addvariable(parser.scope->variables, name, type);

        return nl;
    }

    return left;
}

RTYPEL comparisonparse(Parser& parser) {
    auto left = notparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("comparisonparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_EQEQ ||
        tokens->t.type == T_NOTEQ ||
        tokens->t.type == T_LESSEQ ||
        tokens->t.type == T_GREATEQ ||
        tokens->t.type == T_LESS ||
        tokens->t.type == T_GREAT)
    {
        auto nl = new node_list();
        nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = postnext(tokens).type;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        pushnode(nodes, rparse(parser));

        return nl;
    }

    return left;
}

RTYPEL notparse(Parser& parser) {
    auto& tokens = parser.tokens;

    if (tokens->t.type != T_NOT)
		return addparse(parser);

    auto nl = new node_list();
	nl->n.type = AST_EXPRESSION;
    nl->n.exprtype = postnext(tokens).type;
	auto& nodes = nl->n.nodes;

    pushnode(nodes, rparse(parser));

    return nl;
}

RTYPEL addparse(Parser& parser) {
    node_list* left = mulparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("addparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_ADD || tokens->t.type == T_SUB) { // .. +/- ..
        node_list* nl = new node_list();
		nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = postnext(tokens).type;
        auto& nodes = nl->n.nodes;

		pushnode(nodes, left);

        node_list* r = rparse(parser);

        pushnode(nodes, r);

        return nl;
    }

    return left;
}

RTYPEL mulparse(Parser& parser) {
    auto left = bracketsparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("mulparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_MUL || tokens->t.type == T_DIV || tokens->t.type == T_MOD) {
        auto nl = new node_list();
		nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = postnext(tokens).type;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        pushnode(nodes, rparse(parser));

        return nl;
    }
    return left;
}

RTYPEL bracketsparse(Parser& parser) {
	auto left = dotparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("bracketsparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_ORB) {
        auto t = postnext(tokens); // skip '('

        auto nl = new node_list();
        nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = T_ORB;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        pushnode(nodes, rparse(parser));

        if (tokens->t.type != T_CRB)
            ptrError(t.line, t.begin, "round brackets not closed");
        tokens = tokens->next; // skip ')'
        return nl;
    }
    if (tokens->t.type == T_OSB) {
        auto t = postnext(tokens); // skip '['

        auto nl = new node_list();
        nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = T_OSB;
        auto& nodes = nl->n.nodes;

        pushnode(nodes, left);

        pushnode(nodes, rparse(parser));

        if (tokens->t.type != T_CSB)
            ptrError(t.line, t.begin, "square brackets not closed");
        tokens = tokens->next; // skip ']'
        return nl;
    }

	return left;
}

RTYPEL dotparse(Parser& parser) {
    auto left = idparse(parser);

    auto& tokens = parser.tokens;

    if (tokens == nullptr || left == nullptr) {
#ifdef TKNULL
        printf("dotparse: tokens == nullptr || left == nullptr\n");
#endif // TKNULL
        return left;
    }

    if (tokens->t.type == T_DOT) {
        auto nl = new node_list();
        nl->n.type = AST_EXPRESSION;
        nl->n.exprtype = T_DOT;
        auto& nodes = nl->n.nodes;

        tokens = tokens->next;
        
        pushnode(nodes, left);

        pushnode(nodes, bracketsparse(parser));

        return nl;
    }

    return left;
}

RTYPEL idparse(Parser& parser) {
    auto& tokens = parser.tokens;
    auto type = tokens->t.type;

    if (!skip(tokens))
        ptrError(0, 0, "idparse unexpected end to token");

    switch (type) {
    case T_IDENTIFIER: case T_NUMBER: case T_FLOAT: case T_STRING: case T_MSTRING: {
        auto nl = new node_list();
		nl->n.type = AST_IDENTIFIER;
        nl->n.tptr = tokens;

		postnext(tokens);

        return nl;
    }
    case T_OSB: { // array
        auto t = postnext(tokens); // skip '['

        if (!skip(tokens))
            ptrError(t.line, t.begin, "unexepted end of array");

        auto nl = new node_list();
        nl->n.type = AST_ARRAY;
        auto& nodes = nl->n.nodes;

        while (tokens->t.type != T_CSB) {
            // TODO: do not rparse in array
            auto right = rparse(parser);

            pushnode(nodes, right);

#if 0
            if (!skip(tokens))
                ptrError(tokens->t.line, tokens->t.begin, "unexepted end of array");
#else
            if (!skip(tokens))
                ptrError(t.line, t.begin, "unexepted end of array");
#endif
        }

        postnext(tokens); // skip ']'

        return nl;
    }
    case T_OCB: { // object
		auto t = postnext(tokens); // skip '{'

		if (!skip(tokens))
			ptrError(t.line, t.begin, "unexepted end of object");

		auto nl = new node_list();
		nl->n.type = AST_OBJECT;
		auto& nodes = nl->n.nodes;

        while (tokens->t.type != T_CCB) {
            auto right = rparse(parser);

            pushnode(nodes, right);

            if (!skip(tokens))
                ptrError(t.line, t.begin, "unexepted end of object");
        }

        postnext(tokens); // skip '}'

        return nl;
    }
    default:
        break;
    }
    if (tokens->t.type == T_ORB) { // multiple expressions
        ptrError(tokens->t.line, tokens->t.begin, "not implemented");
		//const token orb = tokens->t;
		//tokens = tokens->next; // skip '('

        return nullptr;
    }
    if (tokens->t.type == T_CRB) {
        return nullptr;
    }
    else {
        const char* errmsg = "idparse unexpected token(%s)";
        const char* type = tokentext[tokens->t.type];
        char* buff = new char[strlen(errmsg) + strlen(type)]();
        sprintf(buff, errmsg, type);
        ptrError(tokens->t.line, tokens->t.begin, buff);
    }
    return nullptr;
}