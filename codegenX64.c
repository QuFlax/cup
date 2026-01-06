typedef uint8_t X64Reg;
enum {
  RAX = 0,
  RCX = 1,
  RDX = 2,
  RBX = 3,
  RSP = 4,
  RBP = 5,
  RSI = 6,
  RDI = 7,
  R8 = 8,
  R9 = 9,
  R10 = 10,
  R11 = 11,
  R12 = 12,
  R13 = 13,
  R14 = 14,
  R15 = 15
};

const char *X64Reg_names[16] = {"RAX", "RCX", "RDX", "RBX", "RSP", "RBP",
                                "RSI", "RDI", "R8",  "R9",  "R10", "R11",
                                "R12", "R13", "R14", "R15"};

// Emit REX prefix for 64-bit operations
static void emit_rex(CBuffer *buf, bool w, X64Reg reg, X64Reg rm) {
  uint8_t rex = 0x40;
  if (w)
    rex |= 0x08; // REX.W
  if (reg & 8)
    rex |= 0x04; // REX.R
  if (rm & 8)
    rex |= 0x01; // REX.B
  if (rex != 0x40)
    emit8(buf, rex);
}

// ModRM byte: [mod(2) | reg(3) | rm(3)]
static void emit_modrm(CBuffer *buf, uint8_t mod, X64Reg reg, X64Reg rm) {
  emit8(buf, (mod << 6) | ((reg & 7) << 3) | (rm & 7));
}

// PUSH reg
static void emit_push_reg(CBuffer *buf, X64Reg reg) {
  if (buf)
    printf("OP: push %s\n", X64Reg_names[reg]);
  if (reg >= 8)
    emit8(buf, 0x41);
  emit8(buf, 0x50 + (reg & 7));
}

//                                           // POP reg
static void emit_pop_reg(CBuffer *buf, X64Reg reg) {
  if (buf)
    printf("OP: pop %s\n", X64Reg_names[reg]);
  if (reg >= 8)
    emit8(buf, 0x41);
  emit8(buf, 0x58 + (reg & 7));
}

// MOV reg, imm64
static void emit_mov_reg_imm64(CBuffer *buf, X64Reg reg, uint64_t imm) {
  if (buf)
    printf("OP: mov %s, 0x%016llx\n", X64Reg_names[reg],
           (unsigned long long)imm);
  emit_rex(buf, true, 0, reg);
  emit8(buf, 0xB8 + (reg & 7));
  emit64(buf, imm);
}

// MOV reg, reg
static void emit_mov_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  if (buf)
    printf("OP: mov %s, %s\n", X64Reg_names[dst], X64Reg_names[src]);
  // Opcode 0x89: MOV r/m64, r64 (ModR/M: reg = src, rm = dst)
  emit_rex(buf, true, src, dst); // REX.R = src, REX.B = dst
  emit8(buf, 0x89);
  emit_modrm(buf, 3, src, dst);
}

static void emit_mov_reg_deref(CBuffer *buf, X64Reg dst, X64Reg src) {
  if (buf)
    printf("OP: mov %s, [%s]\n", X64Reg_names[dst], X64Reg_names[src]);
  // MOV r64, r/m64 → opcode 0x8B, ModR/M: mod=0 (indirect), reg=dst,
  // rm=src
  emit_rex(buf, true, dst, src); // REX.W = 1, REX.R = dst, REX.B = src
  emit8(buf, 0x8B);              // opcode for MOV r64, r/m64
  emit_modrm(buf, 0, dst, src);  // mod=0 → [rm], reg=dst, rm=src
}

static void emit_mov_deref_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  if (buf)
    printf("OP: mov [%s], %s\n", X64Reg_names[dst], X64Reg_names[src]);
  // Store RAX into [dst]
  emit_rex(buf, true, dst, src);
  emit8(buf, 0x89); // MOV r/m64, r64
  emit_modrm(buf, 0, dst, src);
}

// ADD reg, reg
static void emit_add_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  if (buf)
    printf("OP: add %s, %s\n", X64Reg_names[dst], X64Reg_names[src]);
  emit_rex(buf, true, src, dst);
  emit8(buf, 0x01);
  emit_modrm(buf, 3, src, dst);
}

// SUB reg, reg
static void emit_sub_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  emit_rex(buf, true, src, dst);
  emit8(buf, 0x29);
  emit_modrm(buf, 3, src, dst);
}

// IMUL reg, reg
static void emit_imul_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  emit_rex(buf, true, dst, src);
  emit8(buf, 0x0F);
  emit8(buf, 0xAF);
  emit_modrm(buf, 3, dst, src);
}

// XOR reg, reg
static void emit_xor_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  if (buf)
    printf("OP: xor %s, %s\n", X64Reg_names[dst], X64Reg_names[src]);
  // opcode 0x31 : XOR r/m64, r64  (ModR/M.reg = src, r/m = dst)
  emit_rex(buf, true, src, dst);
  emit8(buf, 0x31);
  emit_modrm(buf, 3, src, dst);
}

// CMP reg, reg
static void emit_cmp_reg_reg(CBuffer *buf, X64Reg dst, X64Reg src) {
  emit_rex(buf, true, src, dst);
  emit8(buf, 0x39);
  emit_modrm(buf, 3, src, dst);
}

// SETE reg (set if equal)
static void emit_sete_reg(CBuffer *buf, X64Reg reg) {
  emit_rex(buf, false, 0, reg);
  emit8(buf, 0x0F);
  emit8(buf, 0x94);
  emit_modrm(buf, 3, 0, reg);
}

// SETLE reg(set if less or equal)
static void emit_setle_reg(CBuffer *buf, X64Reg reg) {
  emit_rex(buf, false, 0, reg);
  emit8(buf, 0x0F);
  emit8(buf, 0x9E);
  emit_modrm(buf, 3, 0, reg);
}

// RET
static void emit_ret(CBuffer *buf) {
  if (buf)
    printf("OP: ret\n");
  emit8(buf, 0xC3);
}

// CALL reg
static void emit_call_reg(CBuffer *buf, X64Reg reg) {
  if (buf)
    printf("OP: call %s\n", X64Reg_names[reg]);
  if (reg >= 8)
    emit8(buf, 0x41);
  emit8(buf, 0xFF);
  emit_modrm(buf, 3, 2, reg);
}

// JMP rel32
static void emit_jmp_rel32(CBuffer *buf, int32_t offset) {
  emit8(buf, 0xE9);
  emit32(buf, offset);
}

// JE rel32 (jump if equal)
static void emit_je_rel32(CBuffer *buf, int32_t offset, CUPReallocFunc *) {
  emit8(buf, 0x0F);
  emit8(buf, 0x84);
  emit32(buf, offset);
}

// JNE rel32 (jump if not equal)
static void emit_jne_rel32(CBuffer *buf, int32_t offset, CUPReallocFunc *) {
  emit8(buf, 0x0F);
  emit8(buf, 0x85);
  emit32(buf, offset);
}

// TEST reg, reg
static void emit_test_reg_reg(CBuffer *buf, X64Reg reg1, X64Reg reg2) {
  emit_rex(buf, true, reg1, reg2);
  emit8(buf, 0x85);
  emit_modrm(buf, 3, reg1, reg2);
}

// SETE/SETNE/SETL/SETLE/SETG/SETGE
static void emit_set_condition(CBuffer *buf, CTType cond, X64Reg dst) {
  // Clear destination register first
  emit_rex(buf, false, dst, dst);
  emit8(buf, 0x31);
  emit8(buf, 0xC0 | ((dst & 7) << 3) | (dst & 7));

  // Set byte based on condition
  if (dst >= R8)
    emit8(buf, 0x41);
  emit8(buf, 0x0F);
  switch (cond) {
  case T_EQEQ:
    emit8(buf, 0x94);
    break; // SETE
  case T_NOTEQ:
    emit8(buf, 0x95);
    break; // SETNE
  case T_LESS:
    emit8(buf, 0x9C);
    break; // SETL
  case T_LESSEQ:
    emit8(buf, 0x9E);
    break; // SETLE
  case T_GREAT:
    emit8(buf, 0x9F);
    break; // SETG
  case T_GREATEQ:
    emit8(buf, 0x9D);
    break; // SETGE
  default:
    emit8(buf, 0x94);
    break;
  }
  emit8(buf, 0xC0 | (dst & 7));
}

// MOV [rbp+offset], reg (store variable)
static void emit_mov_mem_reg(CBuffer *buf, int32_t offset, X64Reg src) {
  emit_rex(buf, true, src, RBP);
  emit8(buf, 0x89);
  if (offset >= -128 && offset <= 127) {
    // Use 8-bit displacement
    emit_modrm(buf, 1, src, 5); // [rbp + disp8]
    emit8(buf, offset & 0xFF);
  } else {
    // Use 32-bit displacement
    emit_modrm(buf, 2, src, 5); // [rbp + disp32]
    emit32(buf, offset);
  }
}

// MOV reg, [rbp+offset] (load variable)
static void emit_mov_reg_mem(CBuffer *buf, X64Reg dst, int32_t offset) {
  emit_rex(buf, true, dst, RBP);
  emit8(buf, 0x8B);

  if (offset >= -128 && offset <= 127) {
    emit_modrm(buf, 1, dst, 5); // [rbp+disp8]
    emit8(buf, offset & 0xFF);
  } else {
    emit_modrm(buf, 2, dst, 5); // [rbp+disp32]
    emit32(buf, offset);
  }
}
