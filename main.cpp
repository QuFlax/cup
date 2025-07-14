#include "cup.h"

#define push_token(left, state, skipNL) \
  left.push_back({ state->token, 0 }); \
  getToken(state, skipNL);

static char buffer[1024];

void printUsage() {
  puts("Usage:\tcup [options] <file>\nOptions:\n"
    "-? / -h\t\tShow this help\n"
    "-v\t\tShow the version\n"
    "-n\t\tNo warnings\n"
    "-x\t\tCompile x32\n"
    "-d\t\tAdd DebugInfo\n");
}

static size_t strindex(const char* str, const char c) {
  const char* ptr = str;
  while (*ptr != c)
    ptr++;
  return ptr - str;
}

static bool streq(const char*& it, const char* b, size_t len) {
  do {
    if (*it != *b) {
      while (*it != '\0') it++;
      it++;
      return false;
    }
    else {
      it++;
      b++;
      len--;
    }
  } while (len > 0);
  return *it == '\0';
}

static bool streq(const char* a, size_t len, const char* b) {
  if (a && b) {
		if (strlen(b) != len)
			return false;
		for (; len > 0; len--, a++, b++)
      if (*a != *b)
        return false;
  }
  return true;
}

static inline bool isID(const char c) {
  return ((c < '\0' || c > '/') && (c < ':' || c > '@') &&
    (c < '[' || c > '^') && (c < '{' || c > 0x7F));
}

inline const char nextChar(CUPState* state) {
  const char c = *state->input_stream++;
  if (c == '\n') {
    state->token.loc.line++;
    state->token.loc.col = 1;
  }
  else if (c != '\r') {
    state->token.loc.col++;
  }
  return *state->input_stream;
}

inline bool zeroChar(CUPState* state, char input_char) {
  input_char = state->input_stream[1];
  if (input_char == 'b' || input_char == 'B') {
    nextChar(state);
    input_char = nextChar(state);
    while (input_char == '0' || input_char == '1' || input_char == '_') {
      if (input_char != '_')
        state->token_number = state->token_number * 2 + (input_char - '0');
      input_char = nextChar(state);
    }
    return true;
  }
  if (input_char == 'o' || input_char == 'O') {
    nextChar(state);
    input_char = nextChar(state);
    while ((input_char >= '0' && input_char <= '7') || input_char == '_') {
      if (input_char != '_')
        state->token_number = state->token_number * 8 + (input_char - '0');
      input_char = nextChar(state);
    }
    return true;
  }
  if (input_char == 'x' || input_char == 'X') {
    nextChar(state);
    input_char = nextChar(state);
    while ((input_char >= '0' && input_char <= '9') ||
      (input_char >= 'a' && input_char <= 'f') ||
      (input_char >= 'A' && input_char <= 'F') || input_char == '_') {
      size_t c_int = (input_char >= 'a' && input_char <= 'f') ? (input_char - 87)
        : (input_char >= 'A' && input_char <= 'F') ? (input_char - 55)
        : (input_char - '0');
      if (input_char != '_')
        state->token_number = state->token_number * 16 + c_int;
      input_char = nextChar(state);
    }
    return true;
  }
  return false;
}

static TokenType getToken(CUPState* state, bool skipNL = false) {
  static const TokenType types1[] = { T_ADD, T_SUB,  T_MUL,   T_DIV, T_MOD, T_AND,
                                 T_OR,  T_LESS, T_GREAT, T_XOR, T_NOT, T_EQ };
  static const TokenType doubles[] = { T_ADDEQ,   T_SUBEQ, T_MULEQ, T_DIVEQ,
                                  T_MODEQ,   T_ANDEQ, T_OREQ,  T_LESSEQ,
                                  T_GREATEQ, T_XOREQ, T_NOTEQ, T_EQEQ };
  static const TokenType types2[] = {
      T_ORB,   T_CRB,    T_OCB,    T_CCB, T_OSB, T_CSB,  T_DOT,   T_COMMA,
      T_COLON, T_SCOLON, T_IMPORT, T_ASK, T_AT,  T_THIS, T_CATNL, T_EXTERNAL };

  static Loc loc = { 0, 1, 1 };
  state->token.loc = loc;

  if (skipNL)
  {
    while (*state->input_stream == ' ' || *state->input_stream == '\t'
      || *state->input_stream == '\n' || *state->input_stream == '\r')
      nextChar(state);
  }
  else {
    while (*state->input_stream == ' ' || *state->input_stream == '\t')
      nextChar(state);
  }

  loc = state->token.loc;

  static const char* temp = nullptr;
  char input_char = *state->input_stream;
  state->token_number = 0;

  switch (input_char) {
  case '\0':
    return { T_EOF };
  case '\n':
  case '\r':
    nextChar(state);
    std::swap(loc, state->token.loc);
    return state->token.type = T_NL;
  case '"': {
    for (temp = ++state->input_stream; *state->input_stream != '\0'; nextChar(state)) {
      if (*state->input_stream == '"') {
        nextChar(state);
        break;
      }
    }
    size_t len = (state->input_stream - temp) - 1;
    std::swap(loc, state->token.loc);

    if (state->data) {
      const char* end = state->data + state->data_size;
      for (const char* ptr = state->data; ptr != end;) {
        if (streq(ptr, temp, len)) {
          return state->token.type = T_STRING;
        }
        state->token_number++;
      }
    }
    state->data = (char*)realloc(state->data, state->data_size + len + 1);
    memcpy(state->data + state->data_size, temp, len);
    state->data_size += len;
    state->data[state->data_size++] = '\0';
    return state->token.type = T_STRING;
  }
  case '0':
    if (zeroChar(state, input_char)) {
      std::swap(loc, state->token.loc);
      return state->token.type = T_NUMBER;
    }
    [[fallthrough]];
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    while ((input_char >= '0' && input_char <= '9') || input_char == '_') {
      if (input_char != '_')
        state->token_number = state->token_number * 10 + (input_char - '0');
      input_char = nextChar(state);
    }
    std::swap(loc, state->token.loc);
    return state->token.type = T_NUMBER;
  case '+': case '-': case '*': case '/': case '%': case '&': case '|': case '<': case '>': case '^': case '!': case '=':
    input_char = strindex("+-*/%&|<>^!=", input_char);
    if (nextChar(state) != '=') {
      std::swap(loc, state->token.loc);
      return state->token.type = types1[input_char];
    }
    nextChar(state);
    std::swap(loc, state->token.loc);
    return state->token.type = doubles[input_char];
  case '.': {
    if (nextChar(state) == '.') {
      if (state->input_stream[1] == '.') {
        nextChar(state); nextChar(state);
        std::swap(loc, state->token.loc);
        return state->token.type = T_VARG;
      }
    }
    std::swap(loc, state->token.loc);
    return state->token.type = T_DOT;
  }
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case ',':
  case ':':
  case ';':
  case '#':
  case '?':
  case '@':
  case '$':
  case '\\':
  case '~':
    input_char = strindex("(){}[].,:;#?@$\\~", input_char);
    nextChar(state);
    std::swap(loc, state->token.loc);
    return state->token.type = types2[input_char];
  default: {
    for (temp = state->input_stream; isID(*state->input_stream); nextChar(state));
    size_t len = state->input_stream - temp;
    std::swap(loc, state->token.loc);

    if (streq(temp, len, "if")) {
      return state->token.type = T_IF;
    }
    if (streq(temp, len, "for")) {
      return state->token.type = T_FOR;
    }
    if (streq(temp, len, "else")) {
      return state->token.type = T_ELSE;
    }
    if (streq(temp, len, "break")) {
      return state->token.type = T_BREAK;
    }
    if (streq(temp, len, "while")) {
      return state->token.type = T_WHILE;
    }
    if (streq(temp, len, "return")) {
      return state->token.type = T_RETURN;
    }
    if (streq(temp, len, "continue")) {
      return state->token.type = T_CONTINUE;
    }

    if (state->data) {
      const char* end = state->data + state->data_size;
      for (const char* ptr = state->data; ptr != end;) {
        if (streq(ptr, temp, len))
          return state->token.type = T_IDENTIFIER;
        state->token_number++;
      }
    }
    state->data = (char*)realloc(state->data, state->data_size + len + 1);
    memcpy(state->data + state->data_size, temp, len);
    state->data_size += len;
    state->data[state->data_size++] = '\0';
    return state->token.type = T_IDENTIFIER;
  }
  }
}

static void pushScope(CUPState* state) {
  Scope* old_scope = state->this_scope;
  state->this_scope = new Scope();
  state->this_scope->parent = old_scope;
}
static void popScope(CUPState* state) {
	Scope* old_scope = state->this_scope;
  state->this_scope = state->this_scope->parent;
  state->this_scope->children.push_back(old_scope);
}

static CVariable& getScopeVar(CUPState* state, size_t name) {
  for (auto& s : state->scopes) {
    if (s.variables.contains(name)) {
			return s.variables[name];
		}
	}

  Scope* save_scope = scope;
  do {
    if (scope->variables.contains(name)) {
      return scope->variables[name];
    }
    scope = scope->parent;
  } while (scope);
  save_scope->variables[name] = {};
  if (save_scope->parent == nullptr)
    save_scope->variables[name].flags = SVF_GLOBAL;
	return save_scope->variables[name];
}

static const char* getString(const char* data, size_t index) {
  while (index > 0) {
    data += strlen(data) + 1;
		index--;
  }
	return data;
}

const std::pair<uint8_t, uint8_t> getPower(TokenType t) {
  switch (t) {
  default: {
    fprintf(stderr, "unknown operator '%t'", t);
		exit(1);
  }

  case T_ORB: {
    fprintf(stderr, "unknown '%t'", t);
    exit(1);
  }

  case T_EOF:
  case T_CRB:
  case T_CCB:
  case T_CSB:
  case T_NL:
    return { 0, 0 };

  case T_OCB:
    return { 0, 0 };

  case T_COMMA:
    return { 1, 2 };
  case T_EQ:
  case T_ADDEQ:
  case T_SUBEQ:
  case T_MULEQ:
  case T_DIVEQ:
  case T_MODEQ:
  case T_ANDEQ:
  case T_OREQ:
  case T_XOREQ:
    return { 4, 3 };

  case T_OR:
  case T_AND:
  case T_XOR:
    return { 5, 6 };

  case T_EQEQ:
  case T_NOTEQ:
    return { 7, 8 };

  case T_LESSEQ:
  case T_GREATEQ:
  case T_LESS:
  case T_GREAT:
    return { 9, 10 };

  case T_ADD:
  case T_SUB:
    return { 11, 12 };

  case T_MUL:
  case T_DIV:
  case T_MOD:
    return { 13, 14 };

  case T_DOT:
    return { 15, 16 };
  }
}

inline void expectAndNext(CUPState* state, TokenType type) {
  if (state->token.type != type) {
		snprintf(buffer, sizeof(buffer), "'%c' expected, but got '%c'", state->token.type, state->token.type);
    state->error(buffer);
		exit(1);
  }
  getToken(state);
}

std::list<Node> primary(CUPState* state, uint8_t mpower = 1) {
  std::list<Node> left = {};

  switch (state->token.type) {
  case T_SUB: {
    Node temp = { state->token, 0 };
    temp.type = N_UNARY;
    left.push_back(temp);
    getToken(state);
    left.append_range(primary(state));
    break;
  }
  case T_NOT: {
    left.push_back({ state->token, 0 });
    getToken(state);
    left.append_range(primary(state));
    break;
  }
  case T_ORB: {
    Node temp = { state->token, 0 };
    temp.type = N_COMMA;
    left.push_back(temp);
    if (getToken(state, true) != T_CRB) {
      auto body = primary(state);
      left.front().value = body.size();
      left.append_range(body);
    }
    if (state->token.type == T_NL)
      getToken(state, true);
    expectAndNext(state, T_CRB);
    break;
  }
  case T_IDENTIFIER: {
    Node var = { state->token, state->token_number };
    auto& v = getScopeVar(state->this_scope, state->token_number, true);
    v.flags |= (v.flags & SVF_USED_ONCE ? SVF_USED : SVF_USED_ONCE);

    getToken(state);
    if (state->token.type == T_ORB) {
			Node temp = { state->token, 0 };
			temp.type = N_CALL;
      left.push_back(temp);
      left.push_back(var);
      left.append_range(primary(state));
    }
    else if (state->token.type == T_OSB) {
      Node temp = { state->token, 0 };
      temp.type = N_SUBSCRIPT;
      left.push_back(temp);
      left.push_back(var);
      left.append_range(primary(state));
    }
    else {
      left.push_back(var);
    }
    break;
  }
  case T_STRING: {
    left.push_back({ state->token, state->token_number });
    getToken(state);
    break;
  }
  case T_NUMBER: {
    Node n = { state->token, state->token_number };
    if (getToken(state) == T_DOT) {
      n.type = N_DOUBLE;
      double* dval = (double*)&n.value;
      *dval = (double)n.value;
      if (getToken(state) == T_NUMBER) {
        if (state->token_number != 0)
          *dval += (state->token_number / pow(10, floor(log10(state->token_number) + 1)));
      }
    }
    left.push_back(n);
    break;
  }
  case T_OSB: {
    Node temp = { state->token, 0 };
    temp.type = N_ARRAY;
    left.push_back(temp);
    if (getToken(state, true) != T_CSB) {
      auto body = primary(state);
      left.front().value = body.size();
      left.append_range(body);
    }
    if (state->token.type == T_NL)
      getToken(state, true);
    expectAndNext(state, T_CSB);
    break;
  }
  case T_OCB: {
    Node temp = { state->token, 0 };
    temp.type = N_OBJECT;
    left.push_back(temp);
    if (getToken(state, true) != T_CCB) {
      auto body = primary(state);
      left.front().value = body.size();
      left.append_range(body);
    }
    if (state->token.type == T_NL)
      getToken(state, true);
    expectAndNext(state, T_CCB);
    break;
  }
  case T_VARG: {
    left.push_back({ state->token, 0 });
    getToken(state);
    break;
  }
  default: {
		snprintf(buffer, sizeof(buffer), "Unexpected token '%t'", state->token);
		state->error(buffer);
    exit(1);
    break;
  }
  }

  while (true) {
    auto power = getPower(state->token.type);
    if (power.first < mpower) {
      return left;
    }
    Token t = state->token;
    if (t.type == T_ORB || t.type == T_OSB)
      state->error("T_ORB and T_OSB not implemented yet");
    if (t.type == T_COMMA)
      getToken(state, true);
    else
      getToken(state);
    left.splice(left.end(), primary(state, power.second));

    if (left.front().type == N_COMMA && t.type == T_COMMA) {
      left.front().value++;
    }
    else {
      if (t.type == T_EQ) {
        static size_t assign_count = 0;
        if (left.size() < 2)
          state->error("Cannot assign to nothing");
        auto node = left.front();
        if (node.type != N_VARIABLE) {
          snprintf(buffer, sizeof(buffer), "Cannot assign to { %c, %u:%hu, %zu }", node.type, node.loc.line, node.loc.col, node.value);
          state->error(buffer);
          exit(1);
        }
        auto& v = getScopeVar(state->this_scope, node.value, true);
        if (v.flags & SVF_USED_ONCE) {
          v.flags ^= SVF_USED_ONCE;
        }
        v.flags |= (v.flags & SVF_DECL_ONCE ? SVF_DECL : SVF_DECL_ONCE);
        left.push_front({ t, assign_count++ });
      }
      else if (t.type == T_ADD) {
        left.push_front({ t, 0 });
      }
      else if (t.type == T_COMMA) {
        left.push_front({ t, 0 });
      }
      else if (t.type == T_EQEQ) {
        left.push_front({ t, 0 });
      }
      else if (t.type == T_MUL) {
        left.push_front({ t, 0 });
      }
      else {
				snprintf(buffer, sizeof(buffer), "Unknown operator '%c'", t.type);
				state->error(buffer);
        exit(1);
      }
    }
  }
}

std::list<Node> statement(CUPState* state) { // statement
  std::list<Node> left;
  switch (state->token.type) {
  case T_IMPORT:
    state->error("T_IMPORT not implemented");
  case T_EXTERNAL: {
    if (getToken(state) != T_STRING) {
      state->error("Expected DLL name after '~'");
    }
    // lld_args.push_back(global_strings[token_num]);
    //const char* dllName = token_string;
    //std::string errMsg;
    // Load the DLL permanently
    //if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(dllName, &errMsg)) {
      //cupErrorf("Failed to load DLL '%s': %s", dllName, errMsg.c_str());
    //}
    getToken(state); // Move to next token
  } break;
  default: {
    left = primary(state);
  } break;
  case T_RETURN: {
		push_token(left, state, true);
    left.append_range(primary(state));
  } break;
  case T_BREAK: {
    push_token(left, state, false);
  } break;
  case T_CONTINUE: {
    push_token(left, state, false);
  } break;
  case T_AT: { // function
		push_token(left, state, false);

    if (state->token.type == T_AT) {
      getToken(state);
			sprintf(buffer, "function macro for '%c' not implemented yet", state->token.type);
      state->error(buffer);
      return {};
    }

    if (state->token.type != T_IDENTIFIER) {
      state->error("Expected function name in prototype");
    }

    Node name = { state->token, state->token_number };
    auto& v = getScopeVar(state->this_scope, state->token_number);
    v.flags |= (v.flags & SVF_DECL_ONCE ? SVF_DECL : SVF_DECL_ONCE);
    left.push_back(name);

    expectAndNext(state, T_IDENTIFIER);
    
    pushScope(state);

    left.splice(left.end(), primary(state));

    if (state->token.type == T_NL)
      getToken(state, true);

    left.splice(left.end(), statement(state));

		popScope(state);

    std::list<Node> copy = left;
    copy.pop_front(); // remove N_FUNCTION
    size_t code_size = state->getCodeSize(left.front(), copy);
		left.front().ival = (size_t)realloc(nullptr, code_size);
    state->functions.append_range(left);
    return {};
  }
  case T_OCB: { // block
    getToken(state, true); // skip T_OCB {

    if (state->this_scope->parent) {
      pushScope(state);

      left.push_back({ N_BLOCK });
      while (state->token.type != T_CCB) {
        auto block = statement(state);
        if (block.size()) {
          left.front().ival++;
        }
        left.splice(left.end(), block);
      }

      popScope(state);
    } else {
      while (state->token.type != T_CCB) {
        left.splice(left.end(), statement(state));
      }
    }
    getToken(state, true); // skip T_CCB }

    return left;
  }
  case T_IF: {
    getToken(state, true);

    left.push_back({ N_IF });
    left.splice(left.end(), primary(state));
    left.splice(left.end(), statement(state));
    if (state->token.type == T_ELSE) {
      getToken(state, true);
      left.push_front({ N_IFELSE });
      left.splice(left.end(), statement(state));
    }
    return left;
  }
  case T_WHILE: {
    getToken(state, true);

    left.push_back({ N_WHILE });
    left.splice(left.end(), primary(state));
    statement(state);

    return left;
  }
  }

  if (state->token.type == T_NL)
    getToken(state, true);
  else if (state->token.type != T_EOF) {
    // TODO: say something like "found two or more expression statements in a row"
    fputs("Expected newline\n", stderr);
  }
  return left;
}

void parse(CUPState* c) {
  if (c == nullptr)
    return;
  if (c->error == nullptr)
		c->error = [](const char* msg) { fputs(msg, stderr); };
  if (c->input_stream == nullptr || c->input_path == nullptr) {
    c->error("Input stream or input path not set");
    return;
  }
  getToken(c, true);

  pushScope(c);

  while (c->token.type != T_EOF) {
    c->globals.append_range(statement(c));
  }
  /*
  * 'def: loop {
        lexer::get_token(l)?;
        match (*l).token {
            Token::EOF => break 'def,
            _ => {
                expect_token(l, Token::ID)?;
                let name = arena::strdup(&mut (*c).arena, (*l).string);
                let name_loc = (*l).loc;
                declare_var(c, name, name_loc, Storage::External{name})?;

                let saved_point = (*l).parse_point;
                lexer::get_token(l)?;

                match (*l).token {
                    Token::OParen => { // Function definition
                        scope_push(&mut (*c).vars); // begin function scope
                        let mut params_count = 0;
                        let saved_point = (*l).parse_point;
                        lexer::get_token(l)?;
                        if (*l).token != Token::CParen {
                            (*l).parse_point = saved_point;
                            'params: loop {
                                get_and_expect_token(l, Token::ID)?;
                                let name = arena::strdup(&mut (*c).arena, (*l).string);
                                let name_loc = (*l).loc;
                                let index = allocate_auto_var(&mut (*c).auto_vars_ator);
                                declare_var(c, name, name_loc, Storage::Auto{index})?;
                                params_count += 1;
                                get_and_expect_tokens(l, &[Token::CParen, Token::Comma])?;
                                match (*l).token {
                                    Token::CParen => break 'params,
                                    Token::Comma => continue 'params,
                                    _ => unreachable!(),
                                }
                            }
                        }
                        compile_statement(l, c)?;
                        scope_pop(&mut (*c).vars); // end function scope

                        for i in 0..(*c).func_gotos.count {
                            let used_label = *(*c).func_gotos.items.add(i);
                            let existing_label = find_goto_label(&(*c).func_goto_labels, used_label.name);
                            if existing_label.is_null() {
                                diagf!(used_label.loc, c!("ERROR: label `%s` used but not defined\n"), used_label.name);
                                bump_error_count(c)?;
                                continue;
                            }
                            (*(*c).func_body.items.add(used_label.addr)).opcode = Op::JmpLabel {label: (*existing_label).label};
                        }

                        da_append(&mut (*c).funcs, Func {
                            name,
                            name_loc,
                            body: (*c).func_body,
                            params_count,
                            auto_vars_count: (*c).auto_vars_ator.max,
                        });
                        (*c).func_body = zeroed();
                        (*c).func_goto_labels.count = 0;
                        (*c).func_gotos.count = 0;
                        (*c).auto_vars_ator = zeroed();
                        (*c).op_label_count = 0;
                    }
                    Token::Asm => { // Assembly function definition
                        let mut body: Array<AsmStmt> = zeroed();
                        compile_asm_stmts(l, c, &mut body)?;
                        da_append(&mut (*c).asm_funcs, AsmFunc {name, name_loc, body});
                    }
                    _ => { // Variable definition
                        (*l).parse_point = saved_point;

                        let mut global = Global {
                            name,
                            values: zeroed(),
                            is_vec: false,
                            minimum_size: 0,
                        };

                        // TODO: This code is ugly
                        // couldn't find a better way to write it while keeping accurate error messages
                        get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID, Token::SemiColon, Token::OBracket])?;

                        if (*l).token == Token::OBracket {
                            global.is_vec = true;
                            get_and_expect_tokens(l, &[Token::IntLit, Token::CBracket])?;
                            if (*l).token == Token::IntLit {
                                global.minimum_size = (*l).int_number as usize;
                                get_and_expect_token_but_continue(l, c, Token::CBracket)?;
                            }
                            get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID, Token::SemiColon])?;
                        }

                        while (*l).token != Token::SemiColon {
                            let value = match (*l).token {
                                Token::Minus => {
                                    get_and_expect_token(l, Token::IntLit)?;
                                    ImmediateValue::Literal(!(*l).int_number + 1)
                                }
                                Token::IntLit | Token::CharLit => ImmediateValue::Literal((*l).int_number),
                                Token::String => ImmediateValue::DataOffset(compile_string((*l).string, c)),
                                Token::ID => {
                                    let name = arena::strdup(&mut (*c).arena, (*l).string);
                                    let scope = da_last_mut(&mut (*c).vars).expect("There should be always at least the global scope");
                                    let var = find_var_near(scope, name);
                                    if var.is_null() {
                                        diagf!((*l).loc, c!("ERROR: could not find name `%s`\n"), name);
                                        bump_error_count(c)?;
                                    }
                                    ImmediateValue::Name(name)
                                }
                                _ => unreachable!()
                            };
                            da_append(&mut global.values, value);

                            get_and_expect_tokens(l, &[Token::SemiColon, Token::Comma])?;
                            if (*l).token == Token::Comma {
                                get_and_expect_tokens(l, &[Token::Minus, Token::IntLit, Token::CharLit, Token::String, Token::ID])?;
                            } else {
                                break;
                            }
                        }

                        if !global.is_vec && global.values.count == 0 {
                            da_append(&mut global.values, ImmediateValue::Literal(0));
                        }
                        da_append(&mut (*c).globals, global)
                    }
                }
            }
        }
    }

    Some(())
  */

  while (c->token.type != T_EOF) {
    c->globals.append_range(statement(c));
  }
}
CVariable& codegen(CUPState* state, Node node, std::list<Node>& nodes) {
  bool isGlobal = state->this_scope->parent == nullptr;
  switch (node.type) {
  case N_VARIABLE: {
    auto& var = getScopeVar(state->this_scope, node.ival, true);
    if (var.flags & SVF_DECL) {
      if (isGlobal) {
        return var;
      }
			state->current_block->mov_stack_to_reg(node.ival, BasicBlock::Reg::RAX);
			state->error("codegen: variable not declared in local scope '%v'", var);
    }
    var = state->extental(getString(state->data, node.ival));
    return var;
  }
  case N_NUMBER: {
    CVariable var = {};
    var.value = state->getCValue(CType::getNumber(), node.ival);
    var.flags = SVF_CONSTANT;
    return var;
  }
  case N_DOUBLE: {
    CVariable var = {};
    var.value = state->getCValue(CType::getDouble(), node.ival);
    var.flags = SVF_CONSTANT;
    return var;
  }
  case N_STRING: {
    CVariable var = {};
    var.value = state->getCValue(CType::getString(), (size_t)getString(state->data, node.ival));
    var.flags = SVF_CONSTANT;
    return var;
  }
  case N_BLOCK: {
    CVariable ret;
    Scope* scope = state->this_scope;
    state->this_scope = pop<Scope*>(scope->children);
    for (size_t i = 0; i < node.ival; i++) {
      auto n = pop<Node>(nodes);
      if (n.type != N_RETURN)
        codegen(state, n, nodes);
      else
        ret = codegen(state, n, nodes);
    }
    state->this_scope = state->this_scope->parent;
    return ret;
  }
  case N_ASSIGN: {
    Node name = pop<Node>(nodes);
    if (name.type != N_VARIABLE)
      state->error("codegenassign: unhandled node type %n\n", node);

    fputs("codegenassign: ", stderr);
    fputs(getString(state->data, name.ival), stderr);
    fputc('\n', stderr);

    Node right = pop<Node>(nodes);
    auto val = codegen(state, right, nodes);

    auto& var = codegen(state, name, nodes);

    if (isGlobal) {
      var.getValue<uint64_t>() = val.getValue<uint64_t>();
      if (var.getType() == CType::getVoid()) {
				var.value->type = val.getType();
      }
      return var;
    }
    state->error("codegenassign: not implemented");
    return var;
  }
  case N_CALL: {
    auto BB = state->GetInsertBlock();
    if (BB == nullptr)
      cupError("Invalid function call");

    auto left = pop<Node>(nodes);

    if (left.type != N_VARIABLE) {
      cupError("Invalid function call");
    }

    auto left_val = codegen(state, left, nodes);
    if (left_val.value->type->type != CType::C_FUNCTION)
      cupError("Invalid function call");

    std::list<const CType*> args_types;
    size_t vars = -1;
    auto args = pop<Node>(nodes);
    if (args.type != N_COMMA)
      cupError("Invalid function call");
    for (size_t i = 0; i < args.ival; i++) {
			auto n = pop<Node>(nodes);
      if (n.type == N_VARG) 
				vars = i;
      else 
        args_types.push_back(codegen(state, n, nodes).getType());
    }

    const std::vector<BasicBlock::Reg> registers = { BasicBlock::Reg::RCX, BasicBlock::Reg::RDX, BasicBlock::Reg::R8, BasicBlock::Reg::R9 };
    if (args_types.size() > registers.size()) {
      cupError("Invalid function call");
    }
    for (size_t i = 0; i < args_types.size(); i++) {
      BB->mov_stack_to_reg(0, registers[i]);
    }

    state->CreateCall(left_val.value);
    return left_val;
  }
  case N_ADD: {
    CVariable var = {};
    auto left = codegen(state, pop<Node>(nodes), nodes);
    auto right = codegen(state, pop<Node>(nodes), nodes);
    if (isGlobal) {
      var.value = state->CreateConstAdd(left, right);
      var.flags = SVF_CONSTANT;
      return var;
    }
    state->CreateAdd(left, right);
		return left;
  }
  case N_FUNCTION: {
		std::list<Node> copy = nodes;
    size_t code_size = state->getCodeSize(node, copy);
		auto& var = codegen(state, pop<Node>(nodes), nodes);
    auto args = pop<Node>(nodes);

    if (!isGlobal)
      cupError("Local function in global scope");

    Scope* scope = state->this_scope;
    state->this_scope = pop<Scope*>(scope->children);

    std::list<const CType*> args_vals;
    size_t stack_size = 0;

    if (args.type == N_COMMA) {
      for (size_t i = 0; i < args.ival; i++) {
        auto n = pop<Node>(nodes);
        if (n.type == N_VARIABLE) {
          args_vals.push_back(CType::getNumber());
          continue;
        }
        cupErrorf("codegenfunction: typedefargs: unhandled node type %n\n", n);
      }
    }
    else {
      cupErrorf("codegenfunction: %v\n%n\n", var, args);
    }

    var.getValue<uint64_t>() = node.ival;
    if (var.getType() == CType::getVoid()) {
      var.value->type = CType::getFunction(CType::getVoid(), args_vals);
    }
    else {
      state->error("codegenfunction: type error");
    }
    stack_size = args_vals.size() * 8;

    auto block = pop<Node>(nodes);
    if (block.type == N_BLOCK) {
      stack_size += state->this_scope->children.front()->variables.size() * 8;
    }

    auto BB = state->CreateFunctionPrelude(var.value, code_size, stack_size);
    state->SetInsertPoint(BB);  

    auto ret = codegen(state, block, nodes);
    //var.value->type->types.front = ret->value->type;

    //if (!state->GetInsertBlock()->getTerminator()) {
     // Builder->CreateRet(ConstantInt::get(returnType, 0));
      //}

    state->this_scope = state->this_scope->parent;
    return var;
  }
  default:
    cupErrorf("Unknown node type %n", node);
    break;
  }
}

std::optional<CValue> compile(CUPState* c) {
  if (c == nullptr)
    return;
  if (c->error == nullptr)
    c->error = cupError;
  if (c->machine_type == CUPMachineType::CIRM_UNKNOWN) {
    c->error("Machine type not set");
  }

  c->data_variables_size = c->this_scope->variables.size() * sizeof(size_t);
  c->data = (char*)realloc(c->data, c->data_size + c->data_variables_size);
  memset(c->data + c->data_size, 0, c->data_variables_size);
  size_t* index = (size_t*)(c->data + c->data_size);

  for (auto& var : c->this_scope->variables) {
    var.second.value = c->getCValue(CType::getVoid(), (size_t)index++);
  }

  while (!c->globals.empty()) {
    codegen(c, pop<Node>(c->globals), c->globals);
  }
  while (!c->functions.empty()) {
    codegen(c, pop<Node>(c->functions), c->functions);
  }

  return std::nullopt;
}

std::list<const CType*> CType::all_types = {};

void set_error_function(CUPState* c, CUPState::errorFunction error) {
  c->error = error;
}
void set_extental_function(CUPState* c, CUPState::extentalFunction extental) {
  c->extental = extental;
}
void set_machine_type(CUPState* c, CUPMachineType type) {
  c->machine_type = type;
}

namespace {
  enum SFileEncoding {
    SFE_UTF8,    // UTF-8
    SFE_UTF8BOM, // UTF-8 with BOM
    SFE_UTF16BE, // UTF-16 big endian
    SFE_UTF16LE, // UTF-16 little endian
    SFE_UTF32BE, // UTF-32 big endian
    SFE_UTF32LE, // UTF-32 little endian
  };

  enum SFileTypes {
    SFT_UNKNOWN,
    SFT_CUP, // .cup
    SFT_CBC, // .cbc || binary data
  };

  struct SFile {
    const char* name;
    SFileTypes type;
    SFileEncoding encoding;
    size_t size;
    char* data;
    const char* ptr;
  };

  static const char* basename(const char* fullpath) {
    static char val[_MAX_PATH] = { 0 };
    const char* start = fullpath + strlen(fullpath);
    const char* end = start;
    while (start != fullpath && *start != '/' && *start != '\\') {
      if (*start == '.') {
        end = start;
      }
      start--;
    }
    strncpy(val, start, end - start);
    return val;
  }

  SFileEncoding fileEncoding(const char* data, const size_t size) {
    if (size < 2)
      return SFE_UTF8;
    if (data[0] == 0xFE && data[1] == 0xFF)
      return SFE_UTF16BE; // FE FF UTF-16, big-endian
    if (data[0] == 0xFF && data[1] == 0xFE)
      return SFE_UTF16LE; // FF FE UTF-16, little-endian
    if (size < 3)
      return SFE_UTF8;
    if (data[0] == 0xEF && data[1] == 0xBB && data[2] == 0xBF)
      return SFE_UTF8BOM;
    if (size < 4)
      return SFE_UTF8;
    if (data[0] == 0 && data[1] == 0 && data[2] == 0xFE && data[3] == 0xFF)
      return SFE_UTF32BE; // 00 00 FE FF UTF-32, big-endian
    if (data[0] == 0xFF && data[1] == 0xFE && data[2] == 0 && data[3] == 0)
      return SFE_UTF32LE; // FF FE 00 00 UTF-32, little-endian
    return SFE_UTF8;
  }

  SFileTypes extFile(const char* name) {
    // NEVER TODO: MAYBE READ FIRST 4 BYTES FOR TYPE
    const char* ext = strrchr(name, '.');
    if (ext == nullptr)
      return SFT_CBC;
    if (!strcmp(ext, ".cbc"))
      return SFT_CBC;
    if (!strcmp(ext, ".cup"))
      return SFT_CUP;
    return SFT_UNKNOWN;
  }
  bool readFile(SFile& file, const char* name) {
    FILE* fp = fopen(name, "rb");
    if (!fp)
      return false;

    file.name = name;
    file.type = extFile(name);

    fseek(fp, 0, SEEK_END);
    file.size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    file.data = new char[file.size + 1];
    file.data[file.size] = '\0';
    fread(file.data, file.size, 1, fp);
    fclose(fp);

    file.encoding = fileEncoding(file.data, file.size);
    file.ptr = file.data;

    return true;
  }
}