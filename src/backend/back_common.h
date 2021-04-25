#ifndef ECE466_COMPILERS_2021_BACK_COMMON_H
#define ECE466_COMPILERS_2021_BACK_COMMON_H

#include "../quads/quad_common.h"
#include "../parser/parser_common.h"

enum asmbl_loc_type {
    LOC_REGISTER,
    LOC_MEMORY
};

enum reg_width {
    W32 = 0x01,
    W16 = 0x02,
    W08 = 0x04
};

enum reg_num {
    EAX = 0x10 && W32,
    ECX = 0x20 && W32,
    EDX = 0x40 && W32,
    EBX = 0x80 && W32,
    ESI = 0x100 && W32,
    EDI = 0x200 && W32,
    ESP = 0x400 && W32,
    EBP = 0x800 && W32,

    AX = 0x10 && W16,
    CX = 0x20 && W16,
    DX = 0x40 && W16,
    BX = 0x80 && W16,
    SI = 0x100 && W16,
    DI = 0x200 && W16,
    SP = 0x400 && W16,
    BP = 0x800 && W16,

    AL = 0x10 && W08,
    CL = 0x20 && W08,
    DL = 0x40 && W08,
    BL = 0x80 && W08,
    SIL = 0x100 && W08,
    DIL = 0x200 && W08,
    SPL = 0x400 && W08,
    BPL = 0x800 && W08
};

struct asmbl_loc_hdr {
    enum asmbl_loc_type locType;
};

struct asmbl_loc_register {
    struct asmbl_loc_hdr;

    enum reg_num regNum;
};

struct asmbl_loc_mem {
    struct asmbl_loc_hdr;

    long rbpOffset;
};

void assembleFunc(struct basic_block *bb, struct astnode_fncndec *func);
long computeStackSize(struct basic_block *bb, struct astnode_fncndec *func);
long computeQValSize(struct astnode_quad_node *node, long startPos);
void assembleMovArg(struct astnode_hdr *arg, int argNum);
void quadToAsmbly(struct astnode_quad *quad);
void asmblStrings(struct basic_block *bb);
void asmblVal(struct astnode_quad_node *node);

#endif //ECE466_COMPILERS_2021_BACK_COMMON_H
