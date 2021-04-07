#include <stdlib.h>
#include <stdio.h>
#include "quad_common.h"
#include "../lexer/lexer_common.h"

struct basic_block* genBasicBlock(struct basic_block *prevBlock, int funcIdx){
    struct basic_block *newBlock = mallocSafe(sizeof(struct basic_block));
    newBlock->type = NODE_BASICBLOCK;
    newBlock->funcIdx = funcIdx;
    newBlock->quads = NULL;
    if(prevBlock != NULL){
        prevBlock->next = newBlock;
        newBlock->blockIdx = prevBlock->blockIdx + 1;
    }
    else{
        newBlock->blockIdx = 0;
    }

    newBlock->prev = prevBlock;
    newBlock->next = NULL;

    return newBlock;
}

struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx){
    struct astnode_quad *lastQuad = NULL;
    struct basic_block *currBlock = genBasicBlock(prevBlock, funcIdx);
    struct astnode_quad **firstQuad = &currBlock->quads;

    for(int i = 0; i < stmtList->numVals; i++){
        if(stmtList->els[i]->type == NODE_LST){
            //This is a compound statement, recurse
            currBlock = genQuads(stmtList, currBlock, funcIdx);
        }
        else{
            //TODO: may need to pass current block in here, and return (in struct/param ptr) in case of branching through IF/...
            lastQuad = stmtToQuad(stmtList->els[i], lastQuad, firstQuad);
            if(firstQuad != NULL && *firstQuad != NULL) //Once first quad is set into BB, no need to adjust it further
                firstQuad = NULL;
        }
    }
    return currBlock;
}

struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad, struct astnode_quad **firstQuad){
    union astnode stmtUnion = (union astnode)stmt;
    struct astnode_quad *newQuad = mallocSafe(sizeof(struct astnode_quad));

    switch (stmt->type) {
        case NODE_BINOP: {
            switch (stmtUnion.binNode->op) {
                case '=':
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    newQuad->lval = (struct astnode_quad*)genLval(stmtUnion.binNode->left);
                    lastQuad = newQuad->rval1 = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad);
                    break;
                case '.': //TODO: do we want to support this?
                    fprintf(stderr, "Error: struct member access unimplemented\n");
                    exit(EXIT_FAILURE);
                    break;
                case ',': //Probably best to break each comma into its own quad
                default:
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    newQuad->lval = (struct astnode_quad*)genRegister();
                    lastQuad = newQuad->rval1 = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad);
                    lastQuad = newQuad->rval2 = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad);
            }
            break;
        }
    }

    //This should occur at end because of depth-first recursion
    //Should be assigned to first generated quad
    if(firstQuad != NULL)
        *firstQuad = newQuad;
    if(lastQuad != NULL){
        newQuad->next = NULL;
        newQuad->prev = lastQuad;
        lastQuad->next = newQuad;
    }
    return newQuad;
}

enum quad_opcode binopToQop(int op){
    switch (op) {
        case '+': return QOP_ADD;
        case '-': return QOP_SUB;
        case '%': return QOP_MOD;
        case '*': return QOP_MUL;
        case '/': return QOP_DIV;
        case SHL: return QOP_SHL;
        case SHR: return QOP_SHR;
        case '&': return QOP_BITAND;
        case '^': return QOP_BITXOR;
        case '|': return QOP_BITOR;
        case LOGOR: return QOP_LOGOR;
        case LOGAND: return QOP_LOGAND;
        case EQEQ: return QOP_LOGEQ;
        case NOTEQ: return QOP_LOGNEQ;
        case '<': return QOP_LT;
        case '>': return QOP_GT;
        case LTEQ: return QOP_LTEQ;
        case GTEQ: return QOP_GTEQ;
        case '=': return QOP_MOVE;
    }
    fprintf(stderr, "Error: unknown binop %d\n", op);
    exit(EXIT_FAILURE);
}

struct astnode_quad_register *genRegister(){
    static int registerCounter = 0;
    struct astnode_quad_register *newReg = mallocSafe(sizeof(struct astnode_quad_register));
    newReg->type = NODE_QUAD_VAL;
    newReg->quadType = QUADNODE_REGISTER;
    newReg->registerNum = registerCounter;
    registerCounter++;
    return newReg;
}

struct astnode_quad_node *genLval(struct astnode_hdr *node){
    //TODO: if var - assign to var node
    //TODO: if array - compute array offset as ptr, assign there
    //TODO: else - problem
    union astnode nodeUnion = (union astnode)node;
    switch (node->type) {
        case NODE_SYMTAB: {
            switch (nodeUnion.symEntry->st_type) {
                case ENTRY_VAR: {
                    struct astnode_quad_var *varNode = mallocSafe(sizeof(struct astnode_quad_var));
                    varNode->type = NODE_QUAD_VAL;
                    varNode->quadType = nodeUnion.symEntry->parentTab->scope == SCOPE_FILE ? QUADNODE_GLOBAL : (
                        nodeUnion.symEntry->inProto ? QUADNODE_PARAM : QUADNODE_LOCAL );
                    varNode->varNode = node;
                    return (struct astnode_quad_node *) varNode;
                }
            }
            break;
        }

    }
}