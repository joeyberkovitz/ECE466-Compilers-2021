#ifndef QUAD_COMMON_H
#define QUAD_COMMON_H

#include "../parser/parser_common.h"

enum quad_opcode {
    QOP_LOAD,
    QOP_STORE,
    QOP_ADD,
    QOP_SUB,
    QOP_DIV,
    QOP_MUL,
    QOP_MOD,
    QOP_MOVE,
    QOP_LEA,
    QOP_ARG,
    QOP_CALL,
    QOP_NOP,
    QOP_CMP,
    QOP_RETURN,

    QOP_BR_EQ,
    QOP_BR_NEQ,
    QOP_BR_GT,
    QOP_BR_LT,
    QOP_BR_GTEQ,
    QOP_BR_LTEQ,

    //Unsigned version
    QOP_BR_EQU,
    QOP_BR_NEQU,
    QOP_BR_GTU,
    QOP_BR_LTU,
    QOP_BR_GTEQU,
    QOP_BR_LTEQU,
    QOP_BR_UNCOND,

    QOP_CC_EQ,
    QOP_CC_NEQ,
    QOP_CC_GT,
    QOP_CC_LT,
    QOP_CC_GTEQ,
    QOP_CC_LTEQ,

    //Unsigned version
    QOP_CC_EQU,
    QOP_CC_NEQU,
    QOP_CC_GTU,
    QOP_CC_LTU,
    QOP_CC_GTEQU,
    QOP_CC_LTEQU
};

enum quad_node_type {
    QUADNODE_LOCAL,
    QUADNODE_PARAM,
    QUADNODE_GLOBAL,
    QUADNODE_CONST,
    QUADNODE_REGISTER,
    QUADNODE_BASICBLOCK
};

struct basic_block;

struct astnode_quad_node {
    struct astnode_hdr; //Node type == NODE_QUAD_VAL - for use in lval/rvals
    enum quad_node_type quadType;

    struct astnode_spec_inter *dataType;
};

struct astnode_quad_var {
    struct astnode_quad_node;

    struct astnode_hdr *varNode;
};

struct astnode_quad_const {
    struct astnode_quad_node;

    bool string;
    LexVals value;
    size_t stringNum;
};

struct astnode_quad_register {
    struct astnode_quad_node;

    int registerNum;
    long stackPos;
};

struct astnode_quad_bb {
    struct astnode_quad_node;

    struct basic_block *bb;
};

struct astnode_quad {
    struct astnode_hdr; //Node type == NODE_QUAD
    enum quad_opcode opcode;
    struct basic_block *parentBlock;
    struct astnode_quad *next, *prev;

    struct astnode_quad_node *lval, *rval1, *rval2;
};

struct basic_block {
    struct astnode_hdr; //Node type == NODE_BASICBLOCK
    struct astnode_quad *quads;

    int funcIdx, blockIdx; //Index of function, index of block in function

    struct basic_block *next,*prev, *brPt, *ctPt;
};

void *mallocSafeQuad(size_t size);
//Create a new basic block struct
struct basic_block* genBasicBlock(struct basic_block *prevBlock, int funcIdx, struct basic_block *existingPtr);
//Given a list of statement AST nodes, convert to quads, returning first basic block in linked list
struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx, struct astnode_fncndec *func);

//Convert individual statement to quad node, returning last generated quad node in linked list
//If firstQuad provided, stores first generated quad in this address
//Init_block references first basic block, which could be changed based on returned astnode_quad* BB
struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad, struct astnode_quad **firstQuad,
        struct basic_block *init_block, bool dontLEA, bool dontEmit, struct astnode_fncndec *func);
struct astnode_quad* argToQuad(struct astnode_hdr *arg, struct astnode_hdr *param, struct astnode_quad *lastQuad,
        struct astnode_quad **firstQuad, char *fname, int numArg, bool varArg, bool dontEmit, struct astnode_fncndec *func);
enum quad_opcode unopToQop(int op);
enum quad_opcode binopToQop(int op, bool isUnsigned);
struct astnode_quad_register *genRegister(struct astnode_spec_inter *type);
struct astnode_quad_node *genLval(struct astnode_hdr *node, struct astnode_quad **lastQuad, struct astnode_quad **firstQuad,
        struct basic_block *init_block, bool dontEmit, bool mod, bool funcDes, struct astnode_fncndec *func);

enum type_flag getType(struct astnode_spec_inter *node);
bool isInteger(struct astnode_spec_inter *node);
bool isFloat(struct astnode_spec_inter *node);
bool isPtr(struct astnode_spec_inter *node);
bool isIncomplete(struct astnode_spec_inter *node);
void typeCheck(struct astnode_quad_node *lval, struct astnode_quad_node *rval1, struct astnode_quad_node *rval2, enum quad_opcode opcode, int op, bool unop);
void checkFloat(struct astnode_spec_inter *node);
void assignConvCheck(struct astnode_spec_inter *left, struct astnode_spec_inter *right);
bool argConvCheck(struct astnode_quad_node *arg, struct astnode_hdr *param);
void unaryConvCheck(struct astnode_quad_node *node);
void binaryConvCheck(struct astnode_quad_node *left, struct astnode_quad_node *right);

struct astnode_quad* allocQuad(enum quad_opcode opcode, struct astnode_quad_node *left, struct astnode_quad_node *right, struct astnode_quad *lastQuad);
struct astnode_quad* allocLEAQuad(struct astnode_quad *quad, struct astnode_quad **lastQuad, bool dontLEA);
struct astnode_quad* setLEAQuad(struct astnode_quad *quad, bool ary);
struct astnode_quad_node* allocQuadConst(enum type_flag type, LexVals value, bool string);
struct astnode_hdr* allocFncndecCopy(struct astnode_fncndec *fncn);
struct astnode_spec_inter* allocTypespec(enum type_flag type);

struct astnode_quad_bb* allocBBQuadNode(struct basic_block *bb);
struct astnode_quad* allocCmpQuad(struct astnode_quad_node *left, struct astnode_quad_node *right, struct astnode_quad *lastQuad);
struct astnode_quad* allocMoveQuad(struct astnode_quad_node *lval, struct astnode_quad_node *rval, struct astnode_quad *lastQuad);
struct astnode_quad* allocQuadBR(enum quad_opcode branchType, struct basic_block *trueBB, struct basic_block *falseBB, struct astnode_quad *lastQuad);
struct astnode_quad* allocCCQuad(struct astnode_quad_node *lval, enum quad_opcode condCode, struct astnode_quad *lastQuad);

void printQuads(struct basic_block *basicBlock, char *fname);
void printQuadNode(struct astnode_quad_node *node);

#endif //QUAD_COMMON_H