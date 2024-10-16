//#define _CRT_SECURE_NO_WARNINGS

#include "main.h"
#include "files.h"
#include "parser.h"

const size_t line_tab(const char* line, const char* start) {
    // TODO rename to ...
    size_t count = 1;
    for (; line < start; line++, count++) {
        if (*line == '\t')
            count += 7; // tab width
    }
    return count;
}

void ptrError(const char* line, const char* index, const char* err) {
    fprintf(stderr, "ERROR: %s\n", err);
    if (line) {
        char* end = (char*)line;
        while (*end && *end != '\n' && *end != '\r') end++;
        end[0] = '\0';
        fprintf(stderr, "%s\n", line);
        fprintf(stderr, "%*s\n", (int)line_tab(line, index), "^");
    }
    exit(1);
}

bool strpcmp(const char* a, const char* b, const char* b_end) {
    for (; b < b_end && *a == *b; a++, b++);
    return b == b_end && *a == '\0';
}

struct Option {
    bool usage;
    bool version;
    bool nowarnings;
    bool x32;
    bool debug;
};

void printUsage() {
    printf("Usage:\tcup [options] <file>\nOptions:\n"
        "-? / -h\t\tShow this help\n"
        "-v\t\tShow the version\n"
        "-n\t\tNo warnings\n"
        "-x\t\tCompile x32\n"
        "-d\t\tAdd DebugInfo\n");
}

void argvOptions(Option& options, const char** argv) {
    //if (argv == nullptr) return false;
    const char* arg = *argv;
    switch (arg[1]) {
    case 'v': case 'V': options.version = true; break;
    case 'n': case 'N': options.nowarnings = true; break;
    case 'x': case 'X': options.x32 = true; break;
    case 'd': case 'D': options.debug = true; break;
    case '?': case 'H': case 'h': default:
        options.usage = true; break;
    }
}

void printScope(Scope* scope, FILE* fp) {
    if (scope->parent) {
        printScope(scope->parent, fp);
    }
    fwrite(" ", 1, 1, fp);
    if (scope->returns) {
        fwrite("return ", 1, 1, fp);
        auto r = scope->returns->v;
        fwrite(r.name, strlen(r.name), 1, fp);
        fwrite(" ", 1, 1, fp);
        switch (r.type) {
        case V_UNKNOWN: fwrite("V_UNKNOWN", 9, 1, fp); break;
        case V_OBJECT: fwrite("V_OBJECT", 8, 1, fp); break;
        case V_FUNCTION: fwrite("V_FUNCTION", 10, 1, fp); break;
        case V_NUMBER: fwrite("V_NUMBER", 8, 1, fp); break;
        case V_FLOAT: fwrite("V_FLOAT", 7, 1, fp); break;
        case V_STRING: fwrite("V_STRING", 7, 1, fp); break;
        case V_ARRAY: fwrite("V_ARRAY", 7, 1, fp); break;
        }
        fwrite(" ", 1, 1, fp);
    }
    fwrite("[ ", 2, 1, fp);
    for (auto it = scope->variables; it != nullptr; it = it->next) {
        fwrite(it->v.name, strlen(it->v.name), 1, fp);
        fwrite(" ", 1, 1, fp);
        switch (it->v.type) {
            case V_UNKNOWN: fwrite("V_UNKNOWN", 9, 1, fp); break;
			case V_OBJECT: fwrite("V_OBJECT", 8, 1, fp); break;
			case V_FUNCTION: fwrite("V_FUNCTION", 10, 1, fp); break;
			case V_NUMBER: fwrite("V_NUMBER", 8, 1, fp); break;
			case V_FLOAT: fwrite("V_FLOAT", 7, 1, fp); break;
			case V_STRING: fwrite("V_STRING", 7, 1, fp); break;
			case V_ARRAY: fwrite("V_ARRAY", 7, 1, fp); break;
        }
        fwrite(" ", 1, 1, fp);
    }
    fwrite("]", 1, 1, fp);
}

void printAst(node node, FILE* fp, int tabs) {
    const char* type = asttext[node.type];
    for (int i = 0; i < tabs; i++)
        fwrite("\t", 1, 1, fp);
    fwrite(type, strlen(type), 1, fp);

    if (node.scope) {
        printScope(node.scope, fp);
    }

    switch (node.type) {
        case AST_BSTATEMENT: {
            fwrite("\n", 1, 1, fp);
            for (size_t i = 0; i < tabs; i++)
                fwrite("\t", 1, 1, fp);
            fwrite("{\n", 2, 1, fp);
            for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
			    printAst(nl->n, fp, tabs + 1);
                fwrite("\n", 1, 1, fp);
            }
            for (size_t i = 0; i < tabs; i++)
                fwrite("\t", 1, 1, fp);
            fwrite("}\n", 2, 1, fp);
            return;
        }
        case AST_CSTATEMENT: {
            fwrite("\t", 1, 1, fp);
            const char* type = tokentext[node.exprtype];
		    fwrite(type, strlen(type), 1, fp);
		    fwrite("\t", 1, 1, fp);

            if (node.exprtype == T_IF) {
                fwrite("(", 1, 1, fp);
                node_list* nl = node.nodes.begin;
                printAst(nl->n, fp, 0);
			    fwrite(")", 1, 1, fp);
                for (nl = nl->next; nl != nullptr; nl = nl->next) {
                    printAst(nl->n, fp, tabs);
                }
            }
            return;
        }
        case AST_ISTATEMENT: {
            fwrite("\t", 1, 1, fp);
            const char* type = tokentext[node.exprtype];
            fwrite(type, strlen(type), 1, fp);
            fwrite("\t", 1, 1, fp);

            if (node.exprtype == T_FOR) {
				fwrite("(", 1, 1, fp);
				printAst(node.nodes.begin->n, fp, 0);
				fwrite(")", 1, 1, fp);
				for (node_list* nl = node.nodes.begin->next; nl != nullptr; nl = nl->next) {
					printAst(nl->n, fp, tabs);
				}
            }
            return;
        }
        case AST_JSTATEMENT: {
            fwrite("\t", 1, 1, fp);
            const char* type = tokentext[node.exprtype];
            fwrite(type, strlen(type), 1, fp);
		    fwrite("\t", 1, 1, fp);

            if (node.exprtype == T_RETURN) {
                if (node.nodes.begin != nullptr) {
                    printAst(node.nodes.begin->n, fp, 0);
                }
            }
            return;
        }
        case AST_FUNCTION: {
            if (node.nodes.end == nullptr) {
                fprintf(stderr, "ERROR: function without nodes\n");
                exit(1);
            }
            if (node.nodes.begin != node.nodes.end)
                printAst(node.nodes.begin->n, fp, tabs);
            printAst(node.nodes.end->n, fp, tabs);
            return;
        }
        case AST_ARRAY: {
            fwrite("\t[", 2, 1, fp);
            for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                printAst(nl->n, fp, 1);
            }
            fwrite("]", 1, 1, fp);
            return;
        }
        case AST_OBJECT: {
			fwrite("\t{", 2, 1, fp);
            for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                printAst(nl->n, fp, 1);
            }
            fwrite("}", 1, 1, fp);
            return;
        }
        case AST_AEXPRESSION: {
            if (node.nodes.begin == nullptr) {
                fprintf(stderr, "ERROR: AST_AEXPRESSION without node\n");
                exit(1);
            }
			fwrite("\t", 1, 1, fp);

			const char* type = tokentext[node.exprtype];
            fwrite(type, strlen(type), 1, fp);
            fwrite("\t", 1, 1, fp);

            for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                printAst(nl->n, fp, 1);
            }
            return;
        }
        case AST_BEXPRESSION: {
            if (node.nodes.begin == nullptr) {
                fprintf(stderr, "ERROR: AST_BEXPRESSION without node\n");
                exit(1);
            }

            if (node.exprtype != T_COMMA) {
                fprintf(stderr, "ERROR: AST_BEXPRESSION without T_COMMA\n");
                exit(1);
            }

            fwrite("\t", 1, 1, fp);

			for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
				printAst(nl->n, fp, 1);
			}
            return;
        }
        case AST_EXPRESSION: {
            fwrite("\t", 1, 1, fp);
            if (node.nodes.begin == nullptr) {
                fprintf(stderr, "ERROR: AST_EXPRESSION without node\n");
                exit(1);
            }
            const char* type = tokentext[node.exprtype];
            fwrite(type, strlen(type), 1, fp);
            //fwrite("\t", 1, 1, fp);
            switch (node.exprtype) {
                case T_DOT: {
                    for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                        printAst(nl->n, fp, 1);
                    }
                    return;
                }
                case T_EQEQ: {
                    for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                        printAst(nl->n, fp, 1);
                    }
                    return;
                }
                case T_ORB: {
                    for (node_list* nl = node.nodes.begin; nl != nullptr; nl = nl->next) {
                        printAst(nl->n, fp, 1);
                    }
                    return;
                }
            }
            return;
        }
        case AST_IDENTIFIER: {
            fwrite("\t", 1, 1, fp);
            const token t = node.tptr->t;
            auto type = tokentext[t.type];
            fwrite(type, strlen(type), 1, fp);
            fwrite("\t", 1, 1, fp);

            char* name = new char[t.end - t.begin + 1];
            memcpy(name, t.begin, t.end - t.begin);
            name[t.end - t.begin] = '\0';
            fwrite(name, strlen(name), 1, fp);
            return;
        }
    }
}

int test(int argc, const char** argv) {
    return 0;
}


int main(int argc, const char** argv) {
    if (argc == 1) {
        printUsage();
        return CUPERROR;
    }

    Option options = { 0 };
    SFile file = { 0 };

    for (size_t i = 1; i < argc; i++) {
        const char* arg = argv[i];

        if (arg[0] == '-') {
            argvOptions(options, argv + i);
            continue;
        }
        
        if (readFile(file, arg)) {
            fprintf(stderr, "ERROR: Not Found %s\n", arg);
            return CUPERROR;
        }
    }

    if (options.version)    printf("Cup 0.1\n");
    if (options.usage)      printUsage();

    if (!isFileValid(file)) {
        fprintf(stderr, "ERROR: No file provided\n");
        return CUPERROR;
    }

    token_list* tokens = tokenize(file);

    size_t len;
    char* filename;
    FILE* fp;

    if (options.debug) {
        len = strlen(file.name);
        filename = new char[len + 3]();
        strncpy(filename, file.name, len);
        strncpy(filename + len, ".t", 2);

        fp = fopen(filename, "wb");
        if (!fp) return false;
        for (token_list* ptr = tokens; ptr; ptr = ptr->next) {
            //const token t = ptr->t;
            const char* type = tokentext[ptr->t.type];
            fwrite(type, strlen(type), 1, fp);
            if (ptr->t.type != T_NL) {
                if (ptr->t.end) {
                    fwrite(" ", 1, 1, fp);
                    fwrite(ptr->t.begin, (ptr->t.end - ptr->t.begin), 1, fp);
                }
            }
            //const int tab = line_tab(ptr->t.line, ptr->t.begin); //printf("%.*s\n", (int)line_end(ptr->t.line), ptr->t.line); //printf("%*s", tab, "^"); //printf(" %s %i\n", ptr->t.type, tab);
            fwrite("\n", 1, 1, fp);
        }
        fclose(fp);
        delete[] filename;
    }

    Parser parser = { 0 };
    parse(parser, tokens);

    if (options.debug) {
        len = strlen(file.name);
        filename = new char[len + 5]();
        strncpy(filename, file.name, len);
        strncpy(filename + len, ".ast", 4);

        fp = fopen(filename, "wb");
        if (!fp) return false;
        /*for (auto import : parser.imports) {
			fwrite(import, strlen(import), 1, fp);
			fwrite("\n", 1, 1, fp);
        }*/
        if (parser.root) {
            printAst(*parser.root, fp, 0);
        }
        fclose(fp);
        delete[] filename;
    }

    return 0;
}