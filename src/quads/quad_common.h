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
    QOP_LOGOR,
    QOP_LOGAND,
    QOP_LOGEQ,
    QOP_LOGNEQ,
    QOP_LT,
    QOP_GT,
    QOP_LTEQ,
    QOP_GTEQ,
    QOP_MOVE,
    QOP_LEA,
    QOP_ARG,
    QOP_CALL
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

struct astnode_quad_var {
    struct astnode_quad_node;

    struct astnode_hdr *varNode;
};

struct astnode_quad_const {
    struct astnode_quad_node;

    bool string;
    LexVals value;
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
        struct astnode_quad **firstQuad, bool dontLEA, bool dontEmit);
struct astnode_quad* argToQuad(struct astnode_hdr *arg, struct astnode_hdr *param, struct astnode_quad *lastQuad,
                               struct astnode_quad **firstQuad, char *fname, int numArg, bool varArg, bool dontEmit);
enum quad_opcode unopToQop(int op);
enum quad_opcode binopToQop(int op);
struct astnode_quad_register *genRegister(struct astnode_spec_inter *type);
struct astnode_quad_node *genLval(struct astnode_hdr *node, struct astnode_quad **lastQuad, struct astnode_quad **firstQuad, bool dontEmit, bool mod, bool funcDes);

bool isInteger(struct astnode_spec_inter *node);
bool isPtr(struct astnode_spec_inter *node);
void typeCheck(struct astnode_quad *quad, int op, bool unop);
bool assignConvCheck(struct astnode_quad_node *arg, struct astnode_hdr *param);
void unaryConvCheck(struct astnode_quad_node *node);
void binaryConvCheck(struct astnode_quad_node *left, struct astnode_quad_node *right);

struct astnode_quad* setLEAQuad(struct astnode_quad *quad, bool ary);
struct astnode_quad_node* allocQuadConst(enum type_flag type, LexVals value, bool string);
struct astnode_hdr* allocFncndecCopy(struct astnode_fncndec *fncn);
struct astnode_spec_inter* allocTypespec(enum type_flag type);

void printQuads(struct basic_block *basicBlock, char *fname);
void printQuadNode(struct astnode_quad_node *node);


#endif //QUAD_COMMON_H
