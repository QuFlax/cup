#pragma once
#include <unordered_map>
#include <iostream>
#include <memory>
#include <string>
#include <cstdarg>
#include <stdlib.h>

#include <list>
#include <optional>

template<typename T>
struct CUPList {
  T* head = nullptr;
	T* tail = nullptr;
  T* pop() {
		T* item = head;
		head = head->next;
		return item;
  }
	void pushFront(T* item) {
		item->next = head;
		head = item;
	}
  void pushBack(T* item) {
		item->next = nullptr;
		if (tail == nullptr) {
			head = item;
		} else {
			tail->next = item;
		}
		tail = item;
	}
};

struct Loc {
  uint8_t type;
  uint16_t col;
  uint32_t line;
};

enum TokenType : uint8_t {
  T_EOF = '\0',
  T_NL = '\n',
  T_CATNL = '\\',

  T_RETURN = 'r',
  T_CONTINUE = 'c',
  T_BREAK = 'b',
  T_IF = 'i',
  T_ELSE = 'e',
  T_FOR = 'f',
  T_WHILE = 'w',

  T_IDENTIFIER = '0',
  T_NUMBER = '1',
  T_DOUBLE = '2',
  T_STRING = '3',
  T_MSTRING = '4',
  T_RANGE = '5',

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

  T_ASK = '?',
  T_AT = '@',
  T_THIS = '$',
  T_VARG = 'V',

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

union Token {
  TokenType type;
  Loc loc;
};

enum NodeType : uint8_t {
  N_COMMA    = T_COMMA,    //T_EOF = '\0',
  //T_NL = '\n',
  //T_CATNL = '\\',
  N_RETURN   = T_RETURN,   //T_RETURN = 'r',
  N_CONTINUE = T_CONTINUE, //T_CONTINUE = 'c',
  N_BREAK    = T_BREAK,    //T_BREAK = 'b',
  N_IF       = T_IF,       //T_IF = 'i',
  N_IFELSE   = T_ELSE,   //T_ELSE = 'e',
  N_FOR      = T_FOR,      //T_FOR = 'f',
  N_WHILE    = T_WHILE,    //T_WHILE = 'w',
  N_VARIABLE = T_IDENTIFIER, //T_IDENTIFIER = '0',
  N_NUMBER   = T_NUMBER,   //T_NUMBER = '1',
  N_DOUBLE   = T_DOUBLE,   //T_DOUBLE = '2',
  N_STRING   = T_STRING,   //T_STRING = '3',
  //T_MSTRING = '4',
  //T_RANGE = '5',
  //
  N_ADD      = T_ADD,      //T_ADD = '+',
  N_SUB      = T_SUB,      //T_SUB = '-',
  N_MUL      = T_MUL,      //T_MUL = '*',
  N_DIV      = T_DIV,      //T_DIV = '/',
  N_MOD      = T_MOD,      //T_MOD = '%',
  N_AND      = T_AND,      //T_AND = '&',
  N_OR       = T_OR,       //T_OR = '|',
  N_XOR      = T_XOR,      //T_XOR = '^',
  //T_LESS = '<',
  //T_GREAT = '>',
  N_NOT      = T_NOT,      //T_NOT = '!',
  N_ASSIGN   = T_EQ,   //T_EQ = '=',
  //
  N_CALL      = T_ORB,     //T_ORB = '(',
  N_UNARY     = T_CRB,    //T_CRB = ')',
  N_BLOCK     = T_OCB,    //T_OCB = '{',
  N_OBJECT    = T_CCB,   //T_CCB = '}',
  N_SUBSCRIPT = T_OSB,//T_OSB = '[',
  N_ARRAY     = T_CSB,    //T_CSB = ']',
  N_MEMBER    = T_DOT,   //T_DOT = '.',
  //T_COMMA = ',',
  //T_COLON = ':',
  //T_SCOLON = ';',
  //
  //T_IMPORT = '#',
  //T_EXTERNAL = '~',
  //
  //T_ASK = '?',
  N_FUNCTION = T_AT,  //T_AT = '@',
  //T_THIS = '$',
  N_VARG = T_VARG,      //T_VARG = 'V',
  //
  N_ADDASSIGN = T_ADDEQ, //T_ADDEQ = 'A',
  N_SUBASSIGN = T_SUBEQ, //T_SUBEQ = 'S',
  N_MULASSIGN = T_MULEQ, //T_MULEQ = 'M',
  N_DIVASSIGN = T_DIVEQ, //T_DIVEQ = 'D',
  N_MODASSIGN = T_MODEQ, //T_MODEQ = 'O',
  N_ANDASSIGN = T_ANDEQ, //T_ANDEQ = 'N',
  N_ORASSIGN  = T_OREQ,  //T_OREQ = 'R',
  N_XORASSIGN = T_XOREQ, //T_XOREQ = 'X',
  N_LESSEQ    = T_LESSEQ,    //T_LESSEQ = 'L',
  N_GREATEQ   = T_GREATEQ,   //T_GREATEQ = 'G',
  N_NOTEQ     = T_NOTEQ,     //T_NOTEQ = 'T',
  N_EQEQ      = T_EQEQ,      //T_EQEQ = 'E',
};

struct Node {
  union {
    Token token;
    NodeType type;
    Loc loc;
  };
  uint64_t value;
};

template<typename T>
static const T pop(std::list<T>& list) {
  T item = list.front();
  list.pop_front();
  return item;
}

struct CType {
  static std::list<const CType*> all_types;

  static const CType* getVoid() {
    for (auto it : all_types) {
      if (it->type == Type::C_VOID) {
        return it;
      }
    }
		CType* item = new CType(Type::C_VOID);
		all_types.push_back(item);
		return item;
	}
  static const CType* getNumber() {
    for (auto it : all_types) {
      if (it->type == Type::C_NUMBER) {
        return it;
      }
    }
    CType* item = new CType(Type::C_NUMBER);
    all_types.push_back(item);
    return item;
	}
  static const CType* getDouble() {
    for (auto it : all_types) {
      if (it->type == Type::C_DOUBLE) {
        return it;
      }
    }
    CType* item = new CType(Type::C_DOUBLE);
    all_types.push_back(item);
    return item;
  }
  static const CType* getString() {
    for (auto it : all_types) {
      if (it->type == Type::C_STRING) {
        return it;
      }
    }
    CType* item = new CType(Type::C_STRING);
    all_types.push_back(item);
    return item;
	}
  static const CType* getFunction(const CType* ret, std::list<const CType*> args) {
		std::list<const CType*> func;
		func.push_back(ret);
		func.append_range(args);
		const size_t size = func.size();
    for (auto it : all_types) {
      if (it->type != Type::C_FUNCTION)
				continue;
      if (it->types == func)
				return it;
    }
    CType* item = new CType(Type::C_FUNCTION);
    item->types = func;
    all_types.push_back(item);
    return item;
	}

  enum Type : uint8_t {
    C_VOID,
    C_NUMBER,
    C_DOUBLE,
    C_STRING,
    C_POINTER,
    C_FUNCTION,
  };
  const Type type;
	std::list<const CType*> types;

  bool operator==(const CType& other) const {
    return type == other.type && types == other.types;
  }
  bool operator!=(const CType& other) const {
    return !(*this == other);
  }
};

struct CValue {
	const CType* type;
  uint64_t value;
};

enum CVariableFlag : uint8_t {
	SVF_CONSTANT  = 0x01,
  SVF_GLOBAL    = 0x02,
	SVF_USED_ONCE = 0x04,
  SVF_USED      = 0x0C,
	SVF_DECL_ONCE = 0x10,
	SVF_DECL      = 0x30,
};

struct CVariable {
  CValue value;
  uint8_t flags;
  const bool isConstant() const { return flags & CVariableFlag::SVF_CONSTANT; }
	const bool isGlobal() const { return flags & CVariableFlag::SVF_GLOBAL; }
	const bool isLocal() const { return !(isGlobal() || isConstant()); }
  template<typename T>
  T& getValue() const {
    if (isConstant()) {
      return (T&)value.value;
    }
    if (isGlobal())
    {
      return *(T*)value.value;
    }
    printf("getValue: local variable\n");
		exit(1);
  }
  const CType* getType() const { return value.type; }
};

struct Scope {
  Scope* parent;
  std::list<Scope*> children;
  std::unordered_map<size_t, CVariable> variables;
};

enum CUPMachineType : uint8_t {
  CIRM_UNKNOWN,
  CIRM_X64,
};

struct BasicBlock {
  CValue func;
  uint8_t* code;
  uint8_t* code_end;

  enum Reg {
    RAX, // 000
    RCX, // 001
    RDX, // 010
    RBX, // 011
    RSP, // 100
    RBP, // 101
    RSI, // 110
    RDI, // 111

    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
  };

  void mov_reg_to_reg(Reg from, Reg to) {
    if (from >= R8 || to >= R8) {
      return;
        // ("mov_reg_to_reg: unsupported register");
    }
    *(code++) = 0x48; // 64-bit
    *(code++) = 0x89; // mov reg1, reg2
    uint8_t modrm = 0xC0; // mod = 11, reg = reg1, r/m = reg2
    modrm |= from << 3;
    modrm |= to;
    *(code++) = modrm;
  }
  void mov_stack_to_reg(size_t offset, Reg to) {
		if (to >= R8) {
      return;
      //cupError("mov_stack_to_reg: unsupported register");
		}
		*(code++) = 0x48; // 64-bit
		*(code++) = 0x8B; // mov reg, [rbp - offset]
		uint8_t modrm = 0xC5; // mod = 11, reg = reg1, r/m = reg2
		modrm |= to;
		*(code++) = modrm;
		*(uint32_t*)(code) = offset; code += 4;
  }
  void mov_imm_to_reg(Reg reg, uint64_t value) {
    *(code++) = 0x48;
    switch (reg) {
    case RAX: *(code++) = 0xB8; break;
    case RCX: *(code++) = 0xB9; break;
    case RDX: *(code++) = 0xBA; break;
    case RBX: *(code++) = 0xBB; break;
    case RSP: *(code++) = 0xBC; break;
    case RBP: *(code++) = 0xBD; break;
    case RSI: *(code++) = 0xBE; break;
    case RDI: *(code++) = 0xBF; break;
    default:
      //cupError("unsupported register");
      break;
    }
    *(uint64_t*)(code) = value; code += 8;
  }
  void add_imm_to_reg(Reg reg, uint32_t value) {
    if (reg >= R8) {
      return;
      //cupError("add_imm_to_reg: unsupported register");
    }
    *(code++) = 0x48; // 64-bit
		if (reg == RAX) {
			*(code++) = 0x05; *(uint32_t*)(code) = value; code += 4;
			return;
		}
		*(code++) = 0x83; // add reg, imm32
		uint8_t modrm = 0xC0; // mod=11, reg=0 (ADD), r/m=reg
		modrm |= reg;
		*(code++) = modrm;
		*(uint32_t*)(code) = value; code += 4;
  }
  void sub_imm_to_reg(Reg reg, uint32_t value) {
		*(code++) = 0x48; // 64-bit
    if (reg >= R8) {
      return;
      //cupError("sub_imm_to_reg: unsupported register");
    }
    if (reg == RAX) {
      *(code++) = 0x2D; *(uint32_t*)(code) = value; code += 4;
      return;
    }
		*(code++) = 0x81; // sub reg, imm32
		uint8_t modrm = 0xE8; // mod=11, reg=5 (SUB), r/m=reg
		modrm |= reg;
		*(code++) = modrm;
		*(uint32_t*)(code) = value; code += 4;
  }
  void push_reg(Reg reg) {
    switch (reg) {
    case BasicBlock::RBP:
      *(code++) = 0x55; // push rbp
      break;
    default:
      // cupError("push_reg: unsupported register");
      break;
    }
  }
  void callRax() { *(code++) = 0xFF; *(code++) = 0xD0; }
};

static char buffer[1024];

struct CFunc {
  const char* name;
  Loc name_loc;
  std::list<Node> body;
  size_t params_count;
	size_t auto_vars_count;
};

struct CUPState {
  using extentalFunction = CVariable(*)(const char*);
	using errorFunction = void(*)(const char*);

  CUPMachineType machine_type;
  const char* input_path;
  const char* input_stream;

  errorFunction error;

  Token token;
  uint64_t token_number;

  Scope* this_scope;

	std::list<CFunc> funcs;
  std::list<Node> globals;

  size_t getCodeSize(Node node, std::list<Node>& this_nodes) {
    if (machine_type != CIRM_X64)
      error("getCodeSize: type not implemented");

    size_t size = 0;
    switch (node.type) {
    case N_FUNCTION: {
      auto name = pop<Node>(this_nodes);
      auto args = pop<Node>(this_nodes);
      size += 8; // push rbp
      size += 8; // mov rbp, rsp
      size += 8; // sub rsp, x (stack space for args)
      size += getCodeSize(args, this_nodes);
      auto body = pop<Node>(this_nodes);
      size += getCodeSize(body, this_nodes);
      size += 8; // mov rsp, rbp
      size += 8; // pop rbp
      size += 8; // ret
      break;
    }
    case N_BLOCK: {
      for (size_t i = 0; i < node.value; i++) {
        auto arg = pop<Node>(this_nodes);
        size += getCodeSize(arg, this_nodes);
      }
      break;
    }
    case N_COMMA: {
      for (size_t i = 0; i < node.value; i++) {
        auto arg = pop<Node>(this_nodes);
        size += getCodeSize(arg, this_nodes);
      }
      break;
    }
    case N_CALL: {
      auto name = pop<Node>(this_nodes);
      size += getCodeSize(name, this_nodes);
      auto args = pop<Node>(this_nodes);
      size += getCodeSize(args, this_nodes); // args
      size += 8; // call
      break;
    }
    case N_VARIABLE: {
      size += 8; // mov reg, name
      break;
    }
    case N_STRING: {
      size += 8; // mov reg, string
      break;
    }
    case N_NUMBER: {
      size += 8; // mov reg, number
      break;
    }
    case N_RETURN: {
      auto value = pop<Node>(this_nodes);
      size += getCodeSize(value, this_nodes);
      size += 8; // ret
      break;
    }
    case N_EQEQ: {
      auto left = pop<Node>(this_nodes);
      size += getCodeSize(left, this_nodes);
      auto right = pop<Node>(this_nodes);
      size += getCodeSize(right, this_nodes);
      size += 8; // cmp left, right
      break;
    }
    case T_IF: {
      auto condition = pop<Node>(this_nodes);
      size += getCodeSize(condition, this_nodes);
      auto body = pop<Node>(this_nodes);
      size += getCodeSize(body, this_nodes);
      break;
    }
    case N_ASSIGN: {
      auto left = pop<Node>(this_nodes);
      size += getCodeSize(left, this_nodes);
      auto right = pop<Node>(this_nodes);
      size += getCodeSize(right, this_nodes);
      size += 8; // mov name, value
      break;
    }
    case N_ADD: {
      auto left = pop<Node>(this_nodes);
      size += getCodeSize(left, this_nodes);
      auto right = pop<Node>(this_nodes);
      size += getCodeSize(right, this_nodes);
      size += 8; // add left, right
      break;
    }
    default: {
      snprintf(buffer, sizeof(buffer), "getCodeSize: unknown node type { %c, %u:%hu, %zu }", node.type, node.loc.line, node.loc.col, node.value);
			error(buffer);
      exit(1);
      break;
    }
    }
    return size;
  }

  char* data;
  size_t data_size;
  size_t data_variables_size;

  extentalFunction extental;
  std::list<BasicBlock*> blocks;
  BasicBlock* current_block;

  BasicBlock* CreateBasicBlock(std::optional<CValue> func, size_t size) {
    BasicBlock* block = new BasicBlock;
		block->func = func.value();
    block->code = (uint8_t*) (block->func.value);
    block->code_end = block->code + size;
    blocks.push_back(block);
    return block;
  }

  void SetInsertPoint(BasicBlock* block) { current_block = block; }
  BasicBlock* GetInsertBlock() { return current_block; }

  BasicBlock* CreateFunctionPrelude(std::optional<CValue> func, uint64_t size, uint32_t stack_size) {
		if (machine_type != CIRM_X64)
			error("CreateFunctionPrelude: type not implemented");
		auto block = CreateBasicBlock(func, size);
    block->push_reg(BasicBlock::RBP); // push rbp
    block->mov_reg_to_reg(BasicBlock::RBP, BasicBlock::RSP); // mov rbp, rsp
    //block->sub_imm_to_reg(BasicBlock::RSP, stack_size); // sub rsp, stack_size
		return block;
	}

  CValue CreateCall(CValue func) {
    if (machine_type == CIRM_X64) {
      auto block = GetInsertBlock();
      block->mov_imm_to_reg(BasicBlock::RAX, func.value);
      block->callRax();
      return { func.type->types.front(), 0 };
    }
    error("CreateCall: machine type not implemented");
  }

  CVariable CreateConstAdd(CVariable left, CVariable right) {
    if (left.getType() != right.getType()) {
      error("CreateConstAdd: types do not match");
    }
    if (left.getType() != CType::getNumber())
			error("CreateConstAdd: type not implemented");
    CValue val = { left.getType(), left.getValue<uint64_t>() + right.getValue<uint64_t>() };
    return { val, CVariableFlag::SVF_CONSTANT };
  }
  CVariable CreateAdd(CVariable left, CVariable right) {
    if (machine_type == CIRM_X64) {
      if (left.getType() != right.getType())
        error("CreateAdd: types do not match");
      if (left.getType() != CType::getNumber())
				error("CreateAdd: type not implemented");

      auto block = GetInsertBlock();
      block->mov_imm_to_reg(BasicBlock::RAX, left.getValue<uint64_t>());
      block->add_imm_to_reg(BasicBlock::RAX, right.getValue<uint64_t>());
      CValue val = { left.getType(), 0 };
      return { val, 0 };
    }
    error("CreateAdd: machine type not implemented");
  }
};

void parse(CUPState* c);
void set_error_function(CUPState* c, CUPState::errorFunction error);
void set_extental_function(CUPState* c, CUPState::extentalFunction extental);
void set_machine_type(CUPState* c, CUPMachineType type);
std::optional<CValue> compile(CUPState* c);