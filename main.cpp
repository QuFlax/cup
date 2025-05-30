#include <array>
#include <unordered_map>
#include <unordered_set>
#include <iostream>
#include <memory>
#include <string>
#include <cstdarg>
#include <stdlib.h>
#include <vector>
#include <array>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <llvm\TargetParser\Host.h>
#include <llvm\MC\TargetRegistry.h>
#include <llvm\IR\LegacyPassManager.h>

#include "Windows.h"

#include "lld/Common/Driver.h"
#include "lld/Common/ErrorHandler.h"
#include "lld/Common/Memory.h"

LLD_HAS_DRIVER(coff)
LLD_HAS_DRIVER(elf)
LLD_HAS_DRIVER(mingw)
LLD_HAS_DRIVER(macho)
LLD_HAS_DRIVER(wasm)

/*TEST(AsLib, AllDrivers) {
  EXPECT_TRUE(lldInvoke({ "ld.lld" }));
  EXPECT_TRUE(lldInvoke({ "ld64.lld" }));
  EXPECT_TRUE(lldInvoke({ "ld", "-m", "i386pe" })); // MinGW
  EXPECT_TRUE(lldInvoke({ "lld-link" }));
  EXPECT_TRUE(lldInvoke({ "wasm-ld" }));
}*/

using namespace llvm;

#define CUPSUCCESS false
#define CUPERROR true
#define TOKENNUMTYPE uint64_t

enum Token {
  T_EOF,
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
  T_VARG = 'V',
};

enum NTYPE {
  N_NUMBER,
  N_DOUBLE,
  N_STRING,
  N_ARRAY,
  N_OBJECT,
	N_VARG,

  N_VARIABLE,

  N_CALL,
  N_SUBSCRIPT,
  N_MEMBER,

  N_OPERATOR,
  N_COMMA,

  N_BLOCK,
  N_IF,
  N_FOR,
  N_FUNCTION,
  N_CONTROLFLOW
};

struct Node {
  NTYPE type;
  struct Scope* scope;
  union as {
    double dnumber;
    uint64_t inumber;
    //const char* str;
    Token token;
  } as;
  llvm::Type* ty;
  std::vector<struct Node> nodes;
};

void cupErrorf(const char* format...);
static void cupError(const char* err) { cupErrorf("%s\n", err); }

const Node statement();
const Node primary(uint8_t mpower = 1);

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static ExitOnError ExitOnErr;

class CUPJIT {
private:
  std::unique_ptr<orc::ExecutionSession> ES;
  
	DataLayout DL;

  orc::RTDyldObjectLinkingLayer ObjectLayer;
  orc::IRCompileLayer CompileLayer;

  orc::JITDylib& MainJD;

public:
  CUPJIT(
    std::unique_ptr<orc::ExecutionSession> ES,
    orc::JITTargetMachineBuilder JTMB,
    DataLayout DL)
    :
    ES(std::move(ES)),
    DL(std::move(DL)),
    ObjectLayer(*this->ES, []() { return std::make_unique<SectionMemoryManager>(); }),
    CompileLayer(
      *this->ES, ObjectLayer,
      std::make_unique<orc::ConcurrentIRCompiler>(std::move(JTMB))),
    MainJD(this->ES->createBareJITDylib("<main>"))
  {
    MainJD.addGenerator(
      cantFail(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        DL.getGlobalPrefix())
      )
    );
    if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
      ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
      ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
    }
  }
  ~CUPJIT() {
    if (auto Err = ES->endSession())
      ES->reportError(std::move(Err));
  }
  static Expected<std::unique_ptr<CUPJIT>> Create() {
    auto EPC = orc::SelfExecutorProcessControl::Create();
    if (!EPC)
      return EPC.takeError();
    auto ES = std::make_unique<orc::ExecutionSession>(std::move(*EPC));
    orc::JITTargetMachineBuilder JTMB(
      ES->getExecutorProcessControl().getTargetTriple());
    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError();
    return std::make_unique<CUPJIT>(
      std::move(ES), std::move(JTMB), std::move(*DL)
    );
  }
  const DataLayout& getDataLayout() const { return DL; }
  const Triple& getTargetTriple() const { return ES->getExecutorProcessControl().getTargetTriple(); }
	Expected<std::unique_ptr<MemoryBuffer>> getMemoryBufferForFile() {
		return CompileLayer.getCompiler().operator()(*TheModule);
	}
  orc::JITDylib& getMainJITDylib() { return MainJD; }

  Error addModule(orc::ThreadSafeModule TSM,
    orc::ResourceTrackerSP RT = nullptr) {
    if (!RT)
      RT = MainJD.getDefaultResourceTracker();
    return CompileLayer.add(RT, std::move(TSM));
  }

  Expected<orc::ExecutorSymbolDef> lookup(StringRef Name) {
		orc::MangleAndInterner Mangle(*this->ES, DL);
    return ES->lookup({ &MainJD }, Mangle(Name.str()));
  }
};

static std::unique_ptr<CUPJIT> TheJIT;

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

const char* basename(const char* fullpath) {
  char val[_MAX_PATH] = { 0 };
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
    return CUPERROR;

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

  return CUPSUCCESS;
}

static struct Scope {
  struct ScopeValue {
		Value* value;
    std::vector<ScopeValue*> links;
  };
  std::unordered_map<const char*, ScopeValue> variables;
  Scope* parent;
} *this_scope = nullptr;

static void InitializeModuleAndManagers(void) {
  // Open a new context and module.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("CUPJIT", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());
  TheModule->setTargetTriple(TheJIT->getTargetTriple().normalize());

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  // Create new pass and analysis managers.
  TheFPM = std::make_unique<FunctionPassManager>();
  TheLAM = std::make_unique<LoopAnalysisManager>();
  TheFAM = std::make_unique<FunctionAnalysisManager>();
  TheCGAM = std::make_unique<CGSCCAnalysisManager>();
  TheMAM = std::make_unique<ModuleAnalysisManager>();
  ThePIC = std::make_unique<PassInstrumentationCallbacks>();
  TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
    /*DebugLogging*/ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());

  // Add transform passes.
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->addPass(InstCombinePass());
  // Reassociate expressions.
  TheFPM->addPass(ReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->addPass(GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->addPass(SimplifyCFGPass());

  // Register analysis passes used in these transform passes.
  PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static SFile file = { 0 };

static std::vector<const char*> global_strings = {};

static TOKENNUMTYPE token_num = 0;

static Token CurTok;
static Node root;
static Node empty = { N_OPERATOR, nullptr, T_COMMA };

static const char* rstr = "return";
static const char* tstr = "type";
static const char* lstr = "length";
static const char* sstr = "size";
static const char* thisstr = "$";

static const char* inttype = "int";
static const char* doubletype = "double";
static const char* arraytype = "array";
static const char* pointertype = "pointer";

static std::vector<const char*> lld_args = {
     "lld-link",
     "/entry:main",
     "/subsystem:console",
     nullptr,
     nullptr,
};

static char* this_module = nullptr;
Value* codegen(Node node, Type* type);

Value* findvar(const char* name, Scope* scope) {
  if (scope == nullptr) {
    return nullptr;
  }
  auto it = scope->variables.find(name);
  if (it != scope->variables.end())
    return it->second.value;
  return findvar(name, scope->parent);
}

Type* getNodeType(Node node) {
  switch (node.type) {
  case N_NUMBER:
    return Type::getInt64Ty(*TheContext);
  case N_DOUBLE:
    return Type::getDoubleTy(*TheContext);
  case N_STRING:
    // With opaque pointers, we return a pointer to i8
    return PointerType::get(Type::getInt8Ty(*TheContext), 0);
  case N_VARIABLE: {
    Value* V = findvar(global_strings[node.as.inumber], node.scope);
    if (V == nullptr)
      return Type::getInt64Ty(*TheContext);

    if (auto G = dyn_cast<GlobalVariable>(V))
      return G->getValueType();

    if (auto A = dyn_cast<AllocaInst>(V))
      return A->getAllocatedType();

    // For other values, just return their type directly
    return V->getType();
  }
  default:
    cupErrorf("getNodeType: unhandled node type %n\n", node);
    return nullptr;
  }
}

void typedefargs(Node node, std::vector<Type*>& args) {
  if (node.type == N_OPERATOR && node.as.token == T_EQ) {
    typedefargs(node.nodes.back(), args);
    return;
  }
  if (node.type == N_OPERATOR && node.as.token == T_COMMA) {
    for (auto it : node.nodes) {
      typedefargs(it, args);
    }
    return;
  }
  if (node.type == N_NUMBER) {
    args.push_back(Type::getInt64Ty(*TheContext));
    return;
  }
  if (node.type == N_DOUBLE) {
    args.push_back(Type::getDoubleTy(*TheContext));
    return;
  }
  if (node.type == N_STRING) {
    args.push_back(PointerType::get(Type::getInt8Ty(*TheContext), 0));
    return;
  }
  cupErrorf("typedefargs: unhandled node type %n\n", node);
}

Value* codegenassign(Node node, Token token) {
  if (token == T_EQ) {
    auto front = node.nodes.front();
    if (front.type != N_VARIABLE)
      cupErrorf("codegenassign: unhandled node type %n\n", node);
    const char* name = global_strings[front.as.inumber];
    auto var = findvar(name, node.scope);

		Value* val = nullptr;
    if (var)
      val = codegen(node.nodes.back(), var->getType());
		else
			val = codegen(node.nodes.back(), nullptr);
    if (!val)
      return nullptr;
    
    if (Builder->GetInsertBlock()) {
        // Локальная переменная
        if (var == nullptr) {
          // Иначе создаем новую переменную
          Function* TheFunction = Builder->GetInsertBlock()->getParent();
          IRBuilder TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
          AllocaInst* Alloca = TmpB.CreateAlloca(val->getType(), nullptr, name);
          // Check if we're assigning an array
          if (val->getType()->isArrayTy()) {
            ArrayType* arrayType = cast<ArrayType>(val->getType());
            // Store each element individually
            for (unsigned i = 0; i < arrayType->getNumElements(); ++i) {
              // Extract the element from our array value
              Value* element = Builder->CreateExtractValue(val, i);

              // Create indices for GEP
              Value* idxs[] = {
                ConstantInt::get(Type::getInt64Ty(*TheContext), 0),
                ConstantInt::get(Type::getInt64Ty(*TheContext), i)
              };

              // GEP to get pointer to array element
              Value* elemPtr = Builder->CreateInBoundsGEP(arrayType, Alloca, idxs, "elemPtr");

              // Store element
              Builder->CreateStore(element, elemPtr);
            }

            node.scope->variables.insert({ name, { Alloca } });
            return val;
          }
          node.scope->variables.insert({ name, { Alloca } });
          var = Alloca;
        }
        Builder->CreateStore(val, var);
        return val;
      }
      // Глобальная переменная
      GlobalVariable* gvar = dyn_cast_or_null<GlobalVariable>(var);
      if (gvar == nullptr) {
        gvar = new GlobalVariable(
          *TheModule, val->getType(), false, GlobalValue::ExternalLinkage,
            Constant::getNullValue(val->getType()), name);
        node.scope->variables.insert({ name , { gvar } });
      }
      if (!isa<Constant>(val))
        cupError("Global initializer must be constant");
      gvar->setInitializer(cast<Constant>(val));
      return gvar;
    }
  cupError("codegenassign");
  return nullptr;
}

Function* codegenfunction(Node node) {
  Node proto = node.nodes.front();
  Node argsNode = proto.nodes.back();
  const char* name = global_strings[proto.nodes.front().as.inumber];
  std::vector<Type*> argTypes;
  FunctionType* funcType = nullptr;
  Type* returnType = Type::getInt64Ty(*TheContext);


  if (auto* F = TheModule->getFunction(name)) {
		cupError("Function already defined");
    return F;
  }
  if (argsNode.type != N_COMMA) {
    cupError("Invalid function argument list");
  }
  for (auto n : argsNode.nodes) {
    typedefargs(n, argTypes);
  }

  if (node.nodes.size() == 1) {
    // Function prototype
    funcType = FunctionType::get(returnType, argTypes, false);
    return Function::Create(funcType, Function::ExternalLinkage, name, TheModule.get());
  }
  //auto var = findvar(name, this_scope);
  if (this_module) {
    size_t nameLen = strlen(name);
    size_t moduleLen = strlen(this_module);
    size_t funcNameLen = nameLen + 1 + moduleLen;
    for (auto str : global_strings) {
      auto len = strlen(str);
      if (funcNameLen != len)
        continue;
      const char* it = str;
      if (strncmp(it, this_module, moduleLen) != 0)
        continue;
      it += moduleLen;
      if (strncmp(it, "@", 1) != 0)
        continue;
      it += 1;
      if (strncmp(it, name, nameLen) != 0)
        continue;
      name = str;
      break;
    }
    if (strlen(name) != funcNameLen)
    {
      char* funcName = new char[funcNameLen + 1]();
      strcpy(funcName, this_module);
      strcat(funcName, "@");
      strcat(funcName, name);
      global_strings.push_back(funcName);
      name = funcName;
    }
  }

  // Create function argument types
  std::vector<const char*> argNames;

  // Handle arguments
  if (argsNode.type == N_COMMA) {
    for (auto& argNode : argsNode.nodes) {
      // Default to 64-bit integers for parameters if type not specified
      argTypes.push_back(Type::getInt64Ty(*TheContext));
      argNames.push_back(global_strings[argNode.as.inumber]);
    }
  }

  // Create function type
  funcType = FunctionType::get(returnType, argTypes, false);

  // Create function
  Function* function = Function::Create(funcType, Function::ExternalLinkage,
    name, *TheModule);

  // Set parameter names
  unsigned idx = 0;
  for (auto& arg : function->args()) {
    if (idx < argNames.size()) {
      arg.setName(argNames[idx++]);
    }
  }

  // Create a new basic block to start insertion into
  BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", function);
  Builder->SetInsertPoint(BB);

  // Register arguments in the symbol table
  idx = 0;
  for (auto& arg : function->args()) {
    // Create an alloca for this variable
    AllocaInst* a =
      Builder->CreateAlloca(arg.getType(), nullptr, arg.getName());

    // Store the initial value into the alloca
    Builder->CreateStore(&arg, a);

    // Add arguments to variable symbol table
    const char* argName = argNames[idx++];
    // oldBindings[argName] = NamedValues[argName];
    // NamedValues[argName] = a;
  }

  // Generate code for function body
  Value* bodyVal = codegen(node.nodes.back(), nullptr);
  if (!bodyVal) {
    function->eraseFromParent();
    return nullptr;
  }

  // Check if the function body already has a terminator (like a return
  // statement)
  if (!Builder->GetInsertBlock()->getTerminator()) {
    // If not, add a return instruction with a default value
    Builder->CreateRet(ConstantInt::get(Type::getInt64Ty(*TheContext), 0));
  }

  // Validate the generated code, checking for consistency
  verifyFunction(*function);

  function->print(errs());

  return function;
}

void codegenargs(Node argsNode, std::vector<Value*>& args, size_t& varargs) {
  switch (argsNode.type) {
  case N_OPERATOR:
    if (argsNode.as.token == T_COMMA) {
			for (auto& arg : argsNode.nodes) {
				codegenargs(arg, args, varargs);
			}
			return;
    }
    cupError("Invalid function argument list 2");
	case N_VARG:
		varargs = args.size();
    break;
  case N_STRING:
  case N_NUMBER:
  case N_VARIABLE:
		args.push_back(codegen(argsNode, nullptr));
		break;
  default:
    cupErrorf("Invalid '%n'\n", argsNode);
    break;
  }
}

#define codegenop(left, right, a, b, c) \
  if (isGlobal) {               \
    auto clhs = dyn_cast<Constant>(L); \
    auto crhs = dyn_cast<Constant>(R); \
    if (clhs && crhs)                 \
      return a;\
    cupError("Global expressions must be constant");\
  }\
  return isFloat ? b(L, R) : c(L, R)

Value* codegen(Node node, Type* type) {
  static Constant* Zero = ConstantInt::get(Type::getInt64Ty(*TheContext), 0);
  bool isGlobal = (Builder->GetInsertBlock() == nullptr);
  switch (node.type) {
  case N_NUMBER:
    return ConstantInt::get(Type::getInt64Ty(*TheContext), node.as.inumber, true);
  case N_DOUBLE:
    return ConstantFP::get(Type::getDoubleTy(*TheContext), APFloat(node.as.dnumber));
  case N_STRING: {
    Constant* StrVal = ConstantDataArray::getString(*TheContext, global_strings[node.as.inumber], true); // true = null terminated
    ArrayType* StrType = cast<ArrayType>(StrVal->getType());
		static char strName[256] = { 0 };
    sprintf(strName, ".%d", node.as.inumber);
    Constant* var = TheModule->getOrInsertGlobal(strName, StrVal->getType());
    std::vector<Constant*> indices;
    indices.push_back(Zero);
		indices.push_back(Zero);
    return ConstantExpr::getInBoundsGetElementPtr(
      StrType,  // Source type (array type)
      var,     // Pointer to global string
      indices   // Indices
    );
  }
  case N_ARRAY: {
    size_t arraySize = node.nodes.size();
    if (arraySize == 0)
      cupError("Empty arrays are not supported yet");

    std::vector<Value*> elements;
    for (auto& child : node.nodes) {
      Value* val = codegen(child, nullptr);
      if (!val)
        return nullptr;
      elements.push_back(val);
    }

    Type* elemType = elements[0]->getType();

    // Ensure all elements are of the same type
    for (Value* v : elements) {
      if (v->getType() != elemType) {
        cupError("Array elements must have the same type");
      }
    }

    ArrayType* arrayType = ArrayType::get(elemType, arraySize);
    if (isGlobal) cupError("Global arrays are not supported yet");

    // Create an UndefValue as a placeholder for our array
    Value* arrayValue = UndefValue::get(arrayType);

    // Insert each element into the array
    for (unsigned i = 0; i < arraySize; ++i) {
      // Use InsertValue to build the array (no memory allocation yet)
      arrayValue = Builder->CreateInsertValue(arrayValue, elements[i], i);
    }

    // Now we have an array value that can be assigned using codegenassign
    // The caller (likely codegenassign) will handle the actual allocation
    return arrayValue;
  }
  case N_SUBSCRIPT: {
    if (node.nodes.size() != 2) {
      cupError("Invalid subscript operation");
    }
    Value* arrayPtr = codegen(node.nodes[0], nullptr);
    Value* index = codegen(node.nodes[1], nullptr);
    if (!arrayPtr || !index)
      cupError("Invalid subscript operation");

    // Prepare zero index
    Value* zero = ConstantInt::get(Type::getInt64Ty(*TheContext), 0);

    // With opaque pointers, we need to know the element type explicitly
    // We need to infer the element type from the context
    Type* elemTy = nullptr;
    Type* arrayTy = nullptr;

    // Try to determine the element type from the array pointer
    if (auto allocaInst = dyn_cast<AllocaInst>(arrayPtr)) {
      // For local arrays created with alloca
      arrayTy = allocaInst->getAllocatedType();
      if (arrayTy->isArrayTy()) {
        elemTy = cast<ArrayType>(arrayTy)->getElementType();
      }
    }

    if (!elemTy) {
      cupError("Cannot determine array element type");
      return nullptr;
    }

    // Normalize index to i64
    if (index->getType()->isIntegerTy() &&
      index->getType() != Type::getInt64Ty(*TheContext)) {
      index = Builder->CreateIntCast(
        index, Type::getInt64Ty(*TheContext), true);
    }

    // % arrayidx = getelementptr inbounds i64, ptr% ppp_val, i64 0
		auto gep = Builder->CreateInBoundsGEP(
			arrayTy, arrayPtr, { zero, index }, "arrayidx");
		// %first_elem_val = load i64, ptr %first_elem_ptr, align 8
		auto load = Builder->CreateLoad(elemTy, gep, "loadtmp");
    return load;
  }
  case N_IF: {
    if (node.nodes.size() < 2)
			cupError("Invalid if statement");

    Value* condVal = codegen(node.nodes[0], nullptr);
    if (!condVal)
      return nullptr;

    // Convert condition to a bool i1 if needed (assuming condVal is i64 or something else)
    if (condVal->getType()->isIntegerTy() && condVal->getType()->getIntegerBitWidth() != 1) {
      condVal = Builder->CreateICmpNE(condVal, Zero, "ifcond");
    }

    Function* TheFunction = Builder->GetInsertBlock()->getParent();

    // Create blocks for then, else, and merge
    BasicBlock* ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
    BasicBlock* MergeBB = BasicBlock::Create(*TheContext, "ifcont", TheFunction);
    BasicBlock* ElseBB = MergeBB;

    if (node.nodes.size() > 2) {
      ElseBB = BasicBlock::Create(*TheContext, "else", TheFunction);
    }
    
    Builder->CreateCondBr(condVal, ThenBB, ElseBB);

    Builder->SetInsertPoint(ThenBB);
    Value* ThenVal = codegen(node.nodes[1], nullptr);
    if (!ThenVal) return nullptr;

    if (!ThenBB->getTerminator()) {
      Builder->CreateBr(MergeBB);// Jump to merge after then
    }

    Value* ElseVal = nullptr;
    if (ElseBB != MergeBB) {
      Builder->SetInsertPoint(ElseBB);
      ElseVal = codegen(node.nodes[2], nullptr);
      if (!ElseVal) return nullptr;
      if (!ElseBB->getTerminator()) {
        Builder->CreateBr(MergeBB);// Jump to merge after else
      }
    }

    Builder->SetInsertPoint(MergeBB);
    if (ThenVal->getType()->isVoidTy())
      return Zero;
    PHINode* PN = Builder->CreatePHI(ThenVal->getType(), ElseVal ? 2 : 1, "iftmp");
    PN->addIncoming(ThenVal, ThenBB);
    if (ElseVal) PN->addIncoming(ElseVal, ElseBB);
    else PN->addIncoming(Constant::getNullValue(ThenVal->getType()), ThenBB);
    return PN;
  }
  case N_MEMBER:
    return nullptr;
  case N_VARIABLE: {
    const char* name = global_strings[node.as.inumber];
    Value* V = findvar(name, node.scope);
    if (V == nullptr) {
      cupErrorf("Unknown variable '%s'\n", name);
			return nullptr;
    }

    if (auto A = dyn_cast<AllocaInst>(V)) {
      Type* allocTy = A->getAllocatedType();
      if (allocTy->isArrayTy()) {
        return A;  // Return the array pointer directly
      }
      return Builder->CreateLoad(allocTy, A, name);
    }

    if (auto G = dyn_cast<GlobalVariable>(V)) {
      Type* gvTy = G->getValueType();
      if (gvTy->isArrayTy()) {
        // Create GEP for array pointer with explicit source type
        return Builder->CreateInBoundsGEP(
          gvTy,     // Source element type (array type)
          G,        // Global variable pointer
          { Zero, Zero },
          name
        );
      }
      if (isGlobal)
        return G->getInitializer();
      return Builder->CreateLoad(gvTy, G, "loadtmp");
    }

    cupErrorf("Unknown variable storage for \"%s\"", name);
    return nullptr;
  }
  case N_OPERATOR:
  {
    if (node.as.token == T_EQ) return codegenassign(node, T_EQ);

    Value* L = codegen(node.nodes.front(), nullptr);
    Value* R = codegen(node.nodes.back(), nullptr);
    if (!L || !R)
      return nullptr;

		Type* LTy = L->getType();
		Type* RTy = R->getType();
    // Make sure both operands are of the same type
    if (LTy != RTy) {
      if (LTy->isIntegerTy() && RTy->isDoubleTy()) {
        L = Builder->CreateSIToFP(L, RTy, "inttofp");
      }
      else if (LTy->isDoubleTy() && RTy->isIntegerTy()) {
        R = Builder->CreateSIToFP(R, LTy, "inttofp");
      }
      else if (LTy->isPointerTy()) {
				L = Builder->CreatePtrToInt(L, RTy, "ptrtoint");
      }
      else {
        cupError("Incompatible types for binary operator");
      }
    }

    bool isFloat = LTy->isFloatingPointTy();

    switch (node.as.token) {
    case T_ADD: {
      codegenop(L, R, ConstantExpr::getAdd(clhs, crhs), Builder->CreateFAdd, Builder->CreateAdd);
    }
    case T_SUB: {
      codegenop(L, R, ConstantExpr::getSub(clhs, crhs), Builder->CreateFSub, Builder->CreateSub);
    }
    case T_MUL: {
      codegenop(L, R, ConstantExpr::getMul(clhs, crhs), Builder->CreateFMul, Builder->CreateMul);
    }
    case T_DIV: {
      codegenop(L, R,
        ConstantFoldBinaryInstruction(Instruction::SDiv, clhs, crhs),
        Builder->CreateFDiv, Builder->CreateSDiv);
    }
    case T_MOD: {
      codegenop(L, R,
        ConstantFoldBinaryInstruction(Instruction::URem, clhs, crhs),
        Builder->CreateFRem, Builder->CreateSRem);
    }
    case T_AND:
      return Builder->CreateAnd(L, R, "andtmp");
    case T_OR:
      return Builder->CreateOr(L, R, "ortmp");
    case T_XOR:
      return Builder->CreateXor(L, R, "xortmp");
    case T_LESS:
      return isFloat ? Builder->CreateFCmpOLT(L, R, "lttmp") : Builder->CreateICmpSLT(L, R, "lttmp");
    case T_GREAT:
      return isFloat ? Builder->CreateFCmpOGT(L, R, "gttmp") : Builder->CreateICmpSGT(L, R, "gttmp");
    case T_LESSEQ:
      return isFloat ? Builder->CreateFCmpOLE(L, R, "letmp") : Builder->CreateICmpSLE(L, R, "letmp");
    case T_GREATEQ:
      return isFloat ? Builder->CreateFCmpOGE(L, R, "getmp") : Builder->CreateICmpSGE(L, R, "getmp");
    case T_EQEQ:
      return isFloat ? Builder->CreateFCmpOEQ(L, R, "eqtmp") : Builder->CreateICmpEQ(L, R, "eqtmp");
    case T_NOTEQ:
      return isFloat ? Builder->CreateFCmpONE(L, R, "netmp")
        : Builder->CreateICmpNE(L, R, "netmp");
    default:
      cupErrorf("Invalid binary operator");
      return nullptr;
    }
  }
  case N_CALL: {
    if (node.nodes.size() != 2) {
			cupError("Invalid function call");
    }
    if (node.nodes.front().type != N_VARIABLE) {
			cupError("Invalid function call");
    }
    const char* name = global_strings[node.nodes.front().as.inumber];
    Value* V = findvar(name, node.scope);
    if (V != nullptr) {
			cupErrorf("Function name conflicts with variable name");
    }

    Node argsNode = node.nodes.back();
    std::vector<Value*> args;
    size_t varargs = -1;

    codegenargs(argsNode, args, varargs);

    Function* F = TheModule->getFunction(name);
    if (!F) {
      // Infer function type from arguments
      std::vector<Type*> paramTypes;
      for (size_t i = 0; i < args.size(); i++) {
        if (i == varargs)
					break;
        paramTypes.push_back(args[i]->getType());
      }
			FunctionType* FT = nullptr;
      if (type)
      {
        FT = FunctionType::get(type, paramTypes, varargs != -1);
			}
			else
			{
				FT = FunctionType::get(Type::getInt64Ty(*TheContext), paramTypes, varargs != -1);
			}
      F = Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());
    }

    if (F->arg_size() != args.size() && varargs == -1)
      cupErrorf("Incorrect number of arguments passed: expected %z, got %z",
        F->arg_size(), args.size());

    if (isGlobal)
    {
      cupError("Cannot call function from global scope");
    }
    for (unsigned i = 0; i < F->arg_size(); i++) {
      Value* arg = args[i];
      Type* expectedType = F->getFunctionType()->getParamType(i);
      // Далее каст (если типы разные)
      if (arg->getType() != expectedType) {
        cupError("Invalid argument type passed to function");
        if (isa<Constant>(arg)) {
          // Только между pointer-типами можно кастовать
          if (arg->getType()->isPointerTy() && expectedType->isPointerTy()) {
            arg = ConstantExpr::getBitCast(cast<Constant>(arg), expectedType);
          }
          else {
            cupError("Invalid constant bitcast attempted!");
          }
        }
        else {
          arg = Builder->CreateBitCast(arg, expectedType);
        }
      }
      args[i] = arg;
    }

    return Builder->CreateCall(F, args, "calltmp");
  }
  case N_BLOCK: {
    Value* lastVal = nullptr;
    for (auto it : node.nodes) {
      lastVal = codegen(it, nullptr);
      if (!lastVal)
        return nullptr;
    }
    return lastVal;
  }
  case N_FUNCTION: {
		return codegenfunction(node);
  }
  case N_CONTROLFLOW: {
    if (node.as.token == T_RETURN) {
      return Builder->CreateRet(codegen(node.nodes.front(), nullptr));
    }
    // if (node.as.token == T_BREAK) {
      // Builder->CreateBr(nullptr);
    // }
    cupErrorf("Unknown control flow operator");
    return nullptr;
  }
  default:
    cupErrorf("codegen: unknown node type %n\n", node);
    return nullptr;
  }
}

Value* codegenModule(Node& node) {
  bool dump_strings = true;
  if (dump_strings)
  for (size_t i = 0; i < global_strings.size(); i++) {
    Constant* Val = ConstantDataArray::getString(*TheContext, global_strings[i], true);
    static char Name[256] = { 0 };
		sprintf(Name, ".%d", i);
		new GlobalVariable(
			*TheModule, Val->getType(), true, // isConstant
			GlobalValue::PrivateLinkage, Val, Name
		);
  }
	Value* val = nullptr;

  for (Node& it : node.nodes) {
    if (it.type == N_CONTROLFLOW && it.as.token == T_RETURN) {
      // Generate external variables to module
      cupError("return not implemented yet");
      return nullptr;
    }
    val = codegen(it, nullptr);
    if (!val) return nullptr;
  }

	return val;
}

std::ostream& operator<<(std::ostream& os, const Token t) {
  switch (t) {
  case T_EOF: return os << "eof";
  case T_NL: return os << "nl";
  case T_IDENTIFIER: return os << "identifier";
	case T_NUMBER: return os << "number";
	case T_DOUBLE: return os << "double";
	case T_STRING: return os << "string";
  case T_MSTRING: return os << "mstring";
  case T_RANGE: return os << "range";
	case T_ADDEQ: return os << "+=";
	case T_SUBEQ: return os << "-=";
	case T_MULEQ: return os << "*=";
	case T_DIVEQ: return os << "/=";
	case T_MODEQ: return os << "%=";
	case T_ANDEQ: return os << "&=";
	case T_OREQ: return os << "|=";
	case T_XOREQ: return os << "^=";
  case T_LESSEQ: return os << "<=";
  case T_GREATEQ: return os << ">=";
  case T_NOTEQ: return os << "!=";
  case T_EQEQ: return os << "==";
  default:
    return os << (char)t;
  }
}
std::ostream& operator<<(std::ostream& os, const Node n) {
  if (n.scope == nullptr) {
    os << " ### ";
  }
  switch (n.type) {
  default:          return os << "{ " << n.type << " }";
  case N_OBJECT:
    cupError("object not implemented yet");
  case N_NUMBER:    return os << "{ number " << n.as.inumber << " }";
  case N_DOUBLE:    return os << "{ float " << n.as.dnumber << " }";
  case N_STRING:    return os << "{ string \"" << global_strings[n.as.inumber] << "\" }";
  case N_VARIABLE:  return os << "{ variable " << global_strings[n.as.inumber] << " }";
  case N_ARRAY:
    os << "{ array [ ";
    for (auto it : n.nodes) {
      os << it << " ";
    }
    return os << "] }";
  case N_CALL:
    return os << "{ call " << n.nodes.front() << " ( " << n.nodes.back() << " ) }";
  case N_SUBSCRIPT:
    return os << "{ subscript " << n.nodes.front() << " [ " << n.nodes.back() << " ] }";
  case N_MEMBER:
    return os << "{ member " << n.nodes.front() << " -> " << n.nodes.back() << " }";
  case N_OPERATOR: {
		os << "{ operator ";
    auto it = n.nodes.begin();

    if (it != n.nodes.end()) {
      for (;;)
      {
        os << *(it++);
        if (it == n.nodes.end())
          break;
        os << " " << n.as.token << " ";
      }
    }
		return os << " }";
  }
  case N_FUNCTION:
    return os << "{ function " << n.nodes.front() << n.nodes.back() << " }";
  case N_COMMA:
    os << "{ comma ";
    for (auto it = n.nodes.cbegin(); it != n.nodes.cend(); it++) {
      if (it != n.nodes.cbegin())
        os << ", ";
      os << *it;
    }
    return os << " }";
  case N_BLOCK:
    os << "{ block ";
    for (auto it : n.nodes) {
      os << it << " ";
    }
    return os << "}";
  case N_IF:
    if (n.nodes.size() != 3) {
      return os << "{ if ( " << n.nodes.front() << " ) "
        << n.nodes.back() << " }";
    }
    else {
      return os << "{ if ( " << n.nodes.front() << " ) "
        << n.nodes[1] << " else " << n.nodes.back() << " }";
    }
  case N_FOR:
    return os << "{ for ( " << n.nodes.front() << " ) " << n.nodes.back() << " }";
  case N_CONTROLFLOW:
    if (n.as.token == T_RETURN) {
      return os << "{ return " << n.nodes.front() << " }";
    }
    else if (n.as.token == T_CONTINUE) {
      return os << "{ continue }";
    }
    else if (n.as.token == T_BREAK) {
      return os << "{ break }";
    }
    else {
      cupErrorf("Unknown control flow '%t'", n.as.token);
    }
  }
}

void cupErrorf(const char* format...) {
	static char buf[32] = {};
  fwrite("ERROR: ", 7, 1, stderr);
  va_list args;
  va_start(args, format);
  //_vfprintf_l(stderr, format, NULL, args);
  for (char c = *format; c = *format; format++) {
    if (c == '\0')
      break;
    if (c != '%') {
      fputc(c, stderr);
      continue;
    }
    c = *(++format);
    if (c == 'z') {
      size_t s = va_arg(args, size_t);
      sprintf(buf, "%zu", s);
      fwrite(buf, strlen(buf), 1, stderr);
      continue;
    }
    if (c == 'i')
    {
			int i = va_arg(args, int);
			sprintf(buf, "%i", i);
			fwrite(buf, strlen(buf), 1, stderr);
			continue;
    }
    if (c == 's') {
      const char* ptr = va_arg(args, char*);
      if (format[1] != '*')
        fwrite(ptr, strlen(ptr), 1, stderr);
      else {
        fwrite(ptr, va_arg(args, size_t), 1, stderr);
        format++;
      }
      continue;
    }
    if (c == 'c') {
      c = va_arg(args, char);
      fputc(c, stderr);
      continue;
    }
    if (c == 't') {
			std::cerr << va_arg(args, Token) << std::flush;
      continue;
    }
    if (c == 'v') {
      std::cerr << va_arg(args, llvm::Type*) << std::flush;
      continue;
    }
    if (c == 'n') {
      std::cerr << va_arg(args, Node) << std::flush;
      continue;
    }
    exit(3);
  }
  va_end(args);
  exit(1);
}

void printUsage() {
  puts("Usage:\tcup [options] <file>\nOptions:\n"
    "-? / -h\t\tShow this help\n"
    "-v\t\tShow the version\n"
    "-n\t\tNo warnings\n"
    "-x\t\tCompile x32\n"
    "-d\t\tAdd DebugInfo\n");
}

int strindex(const char* str) {
  const char* ptr = str;
  while (*ptr != *file.ptr)
    ptr++;
  return ptr - str;
}

bool streq(const char* a, const char* bnull, const size_t len) {
  if (a && bnull) {
    if (strlen(bnull) != len)
      return false;
    for (size_t i = 0; i < len; i++) {
      if (a[i] != bnull[i])
        return false;
    }
    return true;
  }
  cupErrorf("streq");
}

bool isID(const char c) {
  return ((c < '\0' || c > '/') && (c < ':' || c > '@') &&
    (c < '[' || c > '^') && (c < '{' || c > 0x7F));
}

Token getToken(bool newlines) {
  static const Token types1[] = { T_ADD, T_SUB,  T_MUL,   T_DIV, T_MOD, T_AND,
                                 T_OR,  T_LESS, T_GREAT, T_XOR, T_NOT, T_EQ };
  static const Token doubles[] = { T_ADDEQ,   T_SUBEQ, T_MULEQ, T_DIVEQ,
                                  T_MODEQ,   T_ANDEQ, T_OREQ,  T_LESSEQ,
                                  T_GREATEQ, T_XOREQ, T_NOTEQ, T_EQEQ };
  static const Token types2[] = {
      T_ORB,   T_CRB,    T_OCB,    T_CCB, T_OSB, T_CSB,  T_DOT,   T_COMMA,
      T_COLON, T_SCOLON, T_IMPORT, T_ASK, T_AT,  T_THIS, T_CATNL, T_EXTERNAL };

  while (*file.ptr == ' ' || *file.ptr == '\t' ||
    (newlines && (*file.ptr == '\n' || *file.ptr == '\r')))
    file.ptr++;

  static const char* temp = nullptr;

  switch (*file.ptr) {
  case '\0':
    return T_EOF;
  case '\n':
  case '\r':
    file.ptr++;
    return T_NL;
  case '"': {
    for (temp = ++file.ptr; *file.ptr != '\0'; file.ptr++) {
      if (*file.ptr == '"') {
        file.ptr++;
        break;
      }
    }
    size_t len = (file.ptr - temp) - 1;
    for (size_t i = 0; i < global_strings.size(); i++) {
      if (streq(temp, global_strings[i], len)) {
        token_num = i;
        return T_STRING;
      }
    }
    char* token_string = new char[len + 1];
    token_string[len] = '\0';
    memcpy((char*)token_string, temp, len);
    token_num = global_strings.size();
    global_strings.push_back(token_string);
    return T_STRING;
  }
  case '0':
    token_num = 0;
    if (file.ptr[1] == 'b' || file.ptr[1] == 'B') {
      for (file.ptr += 2;
        *file.ptr == '_' || *file.ptr == '0' || *file.ptr == '1';
        file.ptr++) {
        const char c = *file.ptr;
        if (c == '_')
          continue;
        token_num = token_num * 2 + (c - '0');
      }
      return T_NUMBER;
    }
    if (file.ptr[1] == 'o' || file.ptr[1] == 'O') {
      for (file.ptr += 2;
        *file.ptr == '_' || (*file.ptr >= '0' && *file.ptr < '8');
        file.ptr++) {
        const char c = *file.ptr;
        if (c == '_')
          continue;
        token_num = token_num * 8 + (c - '0');
      }
      return T_NUMBER;
    }
    if (file.ptr[1] == 'x' || file.ptr[1] == 'X') {
      for (file.ptr += 2;
        *file.ptr == '_' || (*file.ptr >= '0' && *file.ptr <= '9') ||
        (*file.ptr >= 'a' && *file.ptr <= 'f') ||
        (*file.ptr >= 'A' && *file.ptr <= 'F');
        file.ptr++) {
        const char c = *file.ptr;
        if (c == '_')
          continue;
        token_num = token_num * 16 + (c >= 'a' && c <= 'f') ? (c - 87)
          : (c >= 'A' && c <= 'F') ? (c - 55)
          : (c - '0');
      }
      return T_NUMBER;
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
    for (token_num = 0;
      *file.ptr == '_' || (*file.ptr >= '0' && *file.ptr <= '9');
      file.ptr++) {
      const char c = *file.ptr;
      if (c == '_')
        continue;
      token_num = token_num * 10 + (c - '0');
    }
    return T_NUMBER;
  case '+':
  case '-':
  case '*':
  case '/':
  case '%':
  case '&':
  case '|':
  case '<':
  case '>':
  case '^':
  case '!':
  case '=':
    token_num = strindex("+-*/%&|<>^!=");
    if (*(++file.ptr) != '=')
      return types1[token_num];
    file.ptr++;
    return doubles[token_num];
  case '.': {
    if (*(++file.ptr) == '.') {
      if (file.ptr[1] == '.') {
        file.ptr++; file.ptr++;
        return T_VARG;
      }
      return T_DOT;
    }
    return T_DOT;
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
    token_num = strindex("(){}[].,:;#?@$\\~");
    file.ptr++;
    return types2[token_num];
  default: {
    for (temp = file.ptr; isID(*file.ptr); file.ptr++);
    size_t lenght = file.ptr - temp;

    if (streq(temp, "if", lenght))
      return T_IF;
    if (streq(temp, "for", lenght))
      return T_FOR;
    if (streq(temp, "elif", lenght))
      return T_ELIF;
    if (streq(temp, "else", lenght))
      return T_ELSE;
    if (streq(temp, "break", lenght))
      return T_BREAK;
    if (streq(temp, "while", lenght))
      return T_WHILE;
    if (streq(temp, rstr, lenght))
      return T_RETURN;
    if (streq(temp, "continue", lenght))
      return T_CONTINUE;

    for (size_t i = 0; i < global_strings.size(); i++) {
      if (streq(temp, global_strings[i], lenght)) {
        token_num = i;
        return T_IDENTIFIER;
      }
    }
    char* token_string = new char[lenght + 1];
    token_string[lenght] = '\0';
    memcpy((char*)token_string, temp, lenght);
    token_num = global_strings.size();
    global_strings.push_back(token_string);
    return T_IDENTIFIER;
  }
  }
}

Token getNextToken(bool newlines) {
  return CurTok = getToken(newlines);
}

const Node parse() {
  getNextToken(true);
  root.scope = this_scope;

  while (CurTok != T_EOF) {
    root.nodes.push_back(statement());
  }

  std::cout << root << std::endl;

  return root;
}

const std::pair<uint8_t, uint8_t> getPower(Token t) {
  switch (t) {
  default:
    cupErrorf("unknown operator '%t'", t);

  case T_ORB:
		cupError("unknown T_ORB");

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

void expectandnext(Token t) {
  if (CurTok != t)
    cupErrorf("'%t' expected, but got '%t'", t, CurTok);
  getNextToken(false);
}

const Node primary(uint8_t mpower) {
  // TODO: unary T_ADD T_SUB T_NOT
  Node left = {};
	left.scope = this_scope;
  switch (CurTok) {
  case T_ORB: {
    getNextToken(true);
    left = primary();
    expectandnext(T_CRB);
    break;
  }
  case T_IDENTIFIER: {
    left.type = N_VARIABLE;
    left.as.inumber = token_num;
    getNextToken(false);
    if (CurTok == T_ORB) {
      Node temp = { N_CALL, this_scope };
      temp.nodes.push_back(left);
      left = temp;
			if (getNextToken(true) == T_CRB) {
        left.nodes.push_back(empty);
			}
			else {
        left.nodes.push_back(primary());
			}
      expectandnext(T_CRB);
    }
    else if (CurTok == T_OSB) {
      getNextToken(true);
      Node node = { N_SUBSCRIPT };
      node.nodes.push_back(left);
      node.nodes.push_back(primary());
      left = node;
      expectandnext(T_CSB);
    }
    break;
  }
  case T_STRING: {
    left.type = N_STRING;
    left.as.inumber = token_num;
    getNextToken(false);
    break;
  }
  case T_NUMBER: {
    left.type = N_NUMBER;
    left.as.inumber = token_num;

    if (getNextToken(false) != T_DOT)
      break;

    left.type = N_DOUBLE;
    left.as.dnumber = (double)left.as.inumber;
    if (getNextToken(false) != T_NUMBER)
      break;

    if (token_num != 0)
      left.as.dnumber += (token_num / pow(10, floor(log10(token_num) + 1)));
    break;
  }
  case T_OSB: {
    left.type = N_ARRAY;
    if (getNextToken(true) == T_CSB) {
			cupError("empty array not implemented yet");
      getNextToken(false);
      left.ty = ArrayType::get(Type::getInt64Ty(*TheContext), 0);
      break;
    }
    auto arr = primary();

    expectandnext(T_CSB);

    if (arr.type != N_COMMA) {
      left.nodes.push_back(arr);
    }
    else {
      left.nodes = arr.nodes;
    }

    break;
  }
  case T_OCB: {
    left.type = N_OBJECT;
    if (getNextToken(true) == T_CCB) {
      getNextToken(false);
			cupError("empty object not implemented yet");
    }

    while (CurTok != T_CCB) {
      left.nodes.push_back(statement());
    }
    getNextToken(false); // skip T_CCB }
    //left.ty = StructType::create(*TheContext, "object");
		break;
  }
  case T_THIS:
    cupErrorf("not implemented yet '%t'\n", CurTok);
		break;
  case T_VARG: {
    getNextToken(false);
		left.type = N_VARG;
		break;
  }
  default:
    cupErrorf("Unexpected token '%t'\n", CurTok);
    break;
  }

  while (true) {
    Token t = CurTok;
    auto power = getPower(t);
    if (power.first < mpower)
      return left;
    getNextToken(false);
    Node right = primary(power.second);

    if (left.type == N_OPERATOR && left.as.token == T_COMMA && t == T_COMMA)
    {
      left.nodes.push_back(right);
    }
    else {
      Node node = { N_OPERATOR, this_scope };
      node.as.token = t;

      node.nodes.push_back(left); // copy
      node.nodes.push_back(right);

      left = node;
    }
  }
}

void argsparse(Node& nodes) {
  while (true) {
    if (CurTok == T_ORB) {
      if (getNextToken(true) == T_CRB) {
        getNextToken(true);
        return;
      }
      argsparse(nodes);

      if (CurTok != T_CRB)
        cupErrorf("')' expected, but got '%t'", CurTok);

      getNextToken(false);
    }
    else {
      if (CurTok == T_OCB)
        return;
      if (CurTok != T_IDENTIFIER)
        cupErrorf("lvalue expected, but got '%t'", CurTok);
      Node out = { N_VARIABLE };
      out.as.inumber = token_num;
      if (getNextToken(false) == T_EQ) {
        Node left = { N_VARIABLE };
        left.as.inumber = out.as.inumber;
        out.as.token = CurTok;
        out.type = N_OPERATOR;
        getNextToken(true);
        out.nodes.push_back(left);
        out.nodes.push_back(primary());
      }
      nodes.nodes.push_back(out);
    }
    if (CurTok != T_COMMA)
      return;
    getNextToken(true);
  }
}

const Node statement() { // statement
  Node node = {};
  node.scope = this_scope;

  switch (CurTok) {
  case T_IMPORT:
    cupError("T_IMPORT not implemented");
  case T_EXTERNAL: {
    if (getNextToken(false) != T_STRING) {
      cupError("Expected DLL name after '~'");
    }
		lld_args.push_back(global_strings[token_num]);
    //const char* dllName = token_string;
    //std::string errMsg;
    // Load the DLL permanently
    //if (llvm::sys::DynamicLibrary::LoadLibraryPermanently(dllName, &errMsg)) {
      //cupErrorf("Failed to load DLL '%s': %s", dllName, errMsg.c_str());
    //}
    getNextToken(false); // Move to next token
  } break;
  default: {
    node = primary();
  } break;
  case T_RETURN: {
    getNextToken(true);
    node.type = N_CONTROLFLOW;
    node.as.token = T_RETURN;
    node.nodes.push_back(primary());
  } break;
  case T_BREAK: {
    getNextToken(false);
    node.type = N_CONTROLFLOW;
    node.as.token = T_BREAK;
  } break;
  case T_CONTINUE: {
    getNextToken(false);
    node.type = N_CONTROLFLOW;
    node.as.token = T_CONTINUE;
  } break;
  case T_AT: { // function
		getNextToken(false);
    if (CurTok == T_AT) {
			cupError("function prototype not implemented yet");
    }
    if (CurTok != T_IDENTIFIER)
      cupError("Expected function name in prototype");
    node.type = N_FUNCTION;

    Node proto = { N_CALL, this_scope };

    Node protocall = { N_VARIABLE, this_scope };
    protocall.as.inumber = token_num;

    expectandnext(T_IDENTIFIER);
    proto.nodes.push_back(protocall);
    
		Scope* oldScope = this_scope;
		this_scope = new Scope();
		this_scope->parent = oldScope;

    Node nodeargs = { N_COMMA, this_scope };
    argsparse(nodeargs);
    proto.nodes.push_back(nodeargs);

    node.nodes.push_back(proto);

    if (CurTok == T_NL)
      getNextToken(true);

    node.nodes.push_back(statement());
    return node;
  }
  case T_OCB: { // block
    getNextToken(true); // skip T_OCB {
    Scope* oldScope = this_scope;
    this_scope = new Scope();
    this_scope->parent = oldScope;

    node.type = N_BLOCK;
    while (CurTok != T_CCB) {
      node.nodes.push_back(statement());
    }

    this_scope = oldScope;

    getNextToken(true); // skip T_CCB }

    return node;
  }
  case T_IF: {
    getNextToken(true);

    node.type = N_IF;
    node.nodes.push_back(primary());
    node.nodes.push_back(statement());
    if (CurTok == T_ELSE) {
      getNextToken(true);
      node.nodes.push_back(statement());
    }
    return node;
  }
  case T_FOR: {
    getNextToken(true);

    node.type = N_FOR;
    node.nodes.push_back(primary());
    node.nodes.push_back(statement());
    return node;
  }
  }

  if (CurTok == T_NL)
    getNextToken(true);
  else if (CurTok != T_EOF) {
    // TODO: say something like "find two or more expression statements in a row"
    fputs("Expected newline111\n", stderr);
  }

  return node;
}

int main(int argc, const char** argv) {
  if (argc == 1) {
    printUsage();
    return CUPERROR;
  }
  struct Option {
    bool usage;
    bool version;
    bool nowarnings;
    bool x32;
    bool debug;
  } options = { 0 };
  
  //"kernel32.lib",
      //"user32.lib",
      //"/libpath:C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.42.34433\\lib\\x64",
      //"legacy_stdio_definitions.lib",
      //"msvcrt.lib",
      //"/libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\ucrt\\x64",
      //Debug Build : Use ucrtbased.lib
      //Release Build : Use ucrt.lib
      //"ucrt.lib"

  for (size_t i = 1; i < argc; i++) {
    const char* arg = argv[i];
    if (arg[0] == '-') {
      switch (arg[1]) {
      case 'v':
      case 'V':
        options.version = true;
        continue;
      case 'n':
      case 'N':
        options.nowarnings = true;
        continue;
      case 'x':
      case 'X':
        options.x32 = true;
        continue;
      case 'd':
      case 'D':
        options.debug = true;
        continue;
      case 'l':
      case 'L':
        if (i + 1 < argc)
					lld_args.push_back(argv[++i]);
				else
					cupError("Missing argument for -l option");
        continue;
      case 'H':
      case 'h':
      case '?':
      default:
        options.usage = true;
        continue;
      }
    }
    if (readFile(file, arg)) {
      cupErrorf("Failed to read file %s\n", arg);
    }
    // TODO add args to run args
  }
  if (options.version)
    printf("Cup 0.1\n");
  if (options.usage)
    printUsage();
  if (!file.data) {
    cupError("No file provided");
  }
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();
  TheJIT = ExitOnErr(CUPJIT::Create());
  InitializeModuleAndManagers();
  root = { N_BLOCK };
  global_strings.push_back(rstr);
  global_strings.push_back(lstr);
  global_strings.push_back(sstr);
  global_strings.push_back(thisstr);
  this_scope = new Scope();
  auto node = parse();
  // auto result = codegen(node);
  auto result = codegenModule(node);
  if (!result) {
    cupError("codegen failed");
  }

  // Dump LLVM IR if requested
  {
    std::string ir_filename = std::string(file.name) + ".ll";
    std::error_code EC;
    llvm::raw_fd_ostream ir_file(ir_filename, EC, llvm::sys::fs::OF_None);

    if (EC) {
      fprintf(stderr, "Error opening file %s for writing IR: %s\n",
        ir_filename.c_str(), EC.message().c_str());
    }
    else {
      TheModule->print(ir_file, nullptr);
      ir_file.close();
      fprintf(stderr, "LLVM IR dumped to %s\n", ir_filename.c_str());
    }
  }

  // Emit object file if requested
  if (true) {
    std::string obj_filename = std::string(file.name) + ".o";
    std::error_code EC;
    raw_fd_ostream dest(obj_filename, EC, sys::fs::OF_None);
    if (EC) {
			cupErrorf("Error opening file %s for writing object: %s\n", obj_filename.c_str(), EC.message().c_str());
    }

    Expected<std::unique_ptr<MemoryBuffer>> ObjBufOrErr = TheJIT->getMemoryBufferForFile();
		if (!ObjBufOrErr) {
			cupErrorf("Error getting memory buffer for file: %s\n", toString(ObjBufOrErr.takeError()).c_str());
		}
    std::unique_ptr<MemoryBuffer> ObjBuf = std::move(*ObjBufOrErr);
    dest.write(ObjBuf->getBufferStart(), ObjBuf->getBufferSize());
    dest.close();
		printf("Object file emitted to %s\n", obj_filename.c_str());
    
		std::string exe_filename = "/out:" + std::string(file.name) + ".exe";
		lld_args[3] = obj_filename.c_str();
		lld_args[4] = exe_filename.c_str();
    auto r = lld::lldMain(lld_args, llvm::outs(), llvm::errs(), LLD_ALL_DRIVERS);
    //printf("lld64 canRunAgain: %d\n", r.canRunAgain);
    if (r.retCode != 0) {
      cupErrorf("Linking failed with error code: %i\n", r.retCode);
    }
    printf("Linked to %s\n", exe_filename.substr(5).c_str());
  }

  // Continue with JIT execution
  if (true) {
    auto TSM = llvm::orc::ThreadSafeModule::ThreadSafeModule(std::move(TheModule), std::move(TheContext));
    ExitOnErr(TheJIT->addModule(std::move(TSM)));
    auto ExprSymbol = ExitOnErr(TheJIT->lookup("main"));
    uint64_t(*mainF)() = ExprSymbol.getAddress().toPtr<uint64_t(*)()>();
    printf("main() = %llu\n", mainF());
  }

  return 0;
}
