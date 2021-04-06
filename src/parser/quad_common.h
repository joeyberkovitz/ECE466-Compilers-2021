#ifndef QUAD_COMMON_H
#define QUAD_COMMON_H

#include "parser_common.h"

enum quad_opcode {
    QOP_ADD,
    QOP_SUB,
    QOP_DIV,
    QOP_MUL,
    QOP_MOD,
    QOP_SHL,
    QOP_SHR,
    QOP_BITAND,
    QOP_BITXOR,
    QOP_BITOR,
    QOP_LOGOR,
    QOP_LOGAND,
    QOP_LOGEQ,
    QOP_LOGNEQ,
    QOP_LT,
    QOP_GT,
    QOP_LTEQ,
    QOP_GTEQ,
    QOP_MOVE,
};

enum quad_node_type {
    QUADNODE_LOCAL,
    QUADNODE_PARAM,
    QUADNODE_GLOBAL,
    QUADNODE_CONST,
    QUADNODE_REGISTER
};

struct astnode_quad_node {
    struct astnode_hdr; //Node type == NODE_QUAD_VAL - for use in lval/rvals
    enum quad_node_type quadType;
};

struct astnode_quad_var {
    struct astnode_quad_node;

    struct astnode_hdr *varNode;
};

struct astnode_quad_register {
    struct astnode_quad_node;

    int registerNum;
};

struct astnode_quad {
    struct astnode_hdr; //Node type == NODE_QUAD
    enum quad_opcode opcode;
    struct basic_block *parentBlock;
    struct astnode_quad *next, *prev;

    struct astnode_quad *lval, *rval1, *rval2;
    struct astnode_spec_inter *dataType;
};

struct basic_block {
    struct astnode_hdr; //Node type == NODE_BASICBLOCK
    struct astnode_quad *quads;

    int funcIdx, blockIdx; //Index of function, index of block in function

    struct basic_block *next,*prev;
};

//Create a new basic block struct
struct basic_block* genBasicBlock(struct basic_block *prevBlock, int funcIdx);
//Given a list of statement AST nodes, convert to quads, returning first basic block in linked list
struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx);
//Convert individual statement to quad node, returning last generated quad node in linked list
//If firstQuad provided, stores first generated quad in this address
struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad, struct astnode_quad **firstQuad);
enum quad_opcode binopToQop(int op);
struct astnode_quad_register *genRegister();
struct astnode_quad_node *genLval(struct astnode_hdr *node);


#endif //QUAD_COMMON_H
