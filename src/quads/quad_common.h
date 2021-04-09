#ifndef QUAD_COMMON_H
#define QUAD_COMMON_H

#include "../parser/parser_common.h"

enum quad_opcode {
    QOP_LOAD,
    QOP_ADD,
    QOP_SUB,
    QOP_DIV,
    QOP_MUL,
    QOP_MOD,
    QOP_LOGOR,
    QOP_LOGAND,
    QOP_LOGEQ,
    QOP_LOGNEQ,
    QOP_LT,
    QOP_GT,
    QOP_LTEQ,
    QOP_GTEQ,
    QOP_MOVE,
    QOP_LAE
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

    struct astnode_spec_inter *dataType;
};

struct astnode_quad_base {
    struct astnode_quad_node;

    struct astnode_hdr *node;
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

    struct astnode_quad_node *lval, *rval1, *rval2;
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
struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx, char *fname);
//Convert individual statement to quad node, returning last generated quad node in linked list
//If firstQuad provided, stores first generated quad in this address
struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad,
        struct astnode_quad **firstQuad, bool dontLAE);
enum quad_opcode unopToQop(int op);
enum quad_opcode binopToQop(int op);
struct astnode_quad_register *genRegister();
struct astnode_quad_node *genLval(struct astnode_hdr *node, struct astnode_quad **lastQuad);
void typeCheck(struct astnode_quad *quad);
void unaryConvCheck(struct astnode_quad_node *node);

void printQuads(struct basic_block *basicBlock, char *fname);
void printQuadNode(struct astnode_quad_node *node);


#endif //QUAD_COMMON_H
