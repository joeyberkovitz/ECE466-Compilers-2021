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
        newBlock->blockIdx = 1;
    }

    newBlock->prev = prevBlock;
    newBlock->next = NULL;

    return newBlock;
}

struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx, char *fname){
    struct astnode_quad *lastQuad = NULL;
    struct basic_block *currBlock = genBasicBlock(prevBlock, funcIdx);
    struct astnode_quad **firstQuad = &currBlock->quads;

    for(int i = 0; i < stmtList->numVals; i++){
        if(stmtList->els[i]->type == NODE_LST){
            //This is a compound statement, recurse
            currBlock = genQuads(stmtList, currBlock, funcIdx, fname);
        }
        else{
            //TODO: may need to pass current block in here, and return (in struct/param ptr) in case of branching through IF/...
            lastQuad = stmtToQuad(stmtList->els[i], lastQuad, firstQuad, false);
            if(firstQuad != NULL && *firstQuad != NULL) //Once first quad is set into BB, no need to adjust it further
                firstQuad = NULL;
        }
    }

    printQuads(currBlock, fname);
    return currBlock;
}

struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad,
        struct astnode_quad **firstQuad, bool dontLAE){
    union astnode stmtUnion = (union astnode)stmt;
    struct astnode_quad *newQuad = mallocSafe(sizeof(struct astnode_quad));
    newQuad->lval = newQuad->rval1 = newQuad->rval2 = NULL;

    switch (stmt->type) {
        case NODE_SYMTAB:
        case NODE_LEX:
            // TODO: function entries
            newQuad->lval = (struct astnode_quad_node*)genRegister();
            newQuad->rval1 = (struct astnode_quad_node*)mallocSafe(sizeof(struct astnode_quad_base));
            newQuad->rval1->quadType = stmt->type == NODE_LEX ? QUADNODE_CONST :
                    stmtUnion.varMem->parentTab->scope == SCOPE_FILE ?
                    QUADNODE_GLOBAL : stmtUnion.varMem->inProto ?
                    QUADNODE_PARAM : QUADNODE_LOCAL;
            ((struct astnode_quad_base*)newQuad->rval1)->node = stmt;
            if (stmt->type == NODE_LEX){
                if (hasFlag(stmtUnion.lexNode->tflags,float_type) || hasFlag(stmtUnion.lexNode->tflags,double_type)){
                    fprintf(stderr, "Error: float types are not implemented\n");
                    exit(EXIT_FAILURE);
                }

                struct astnode_typespec *newTypeNode = (struct astnode_typespec*)mallocSafe(sizeof(struct astnode_typespec));
                newTypeNode->stype = stmtUnion.lexNode->tflags;
                newTypeNode->qtype = 0;
                newQuad->lval->dataType = newQuad->rval1->dataType = (struct astnode_spec_inter*)newTypeNode;
            }
            else
                newQuad->lval->dataType = newQuad->rval1->dataType = stmtUnion.varMem->child;

            if (((stmt->type == NODE_SYMTAB && stmtUnion.varMem->child->type == NODE_ARY) ||
                    (stmt->type == NODE_LEX && stmtUnion.lexNode->sym == STRING)) && !dontLAE) {
                newQuad->opcode = QOP_LAE;
                struct astnode_ptr *ptr = (struct astnode_ptr*)mallocSafe(sizeof(struct astnode_ptr));
                ptr->type = NODE_PTR;
                ptr->qtype = 0;
                ptr->child = newQuad->lval->dataType;
                newQuad->lval->dataType = (struct astnode_spec_inter*)ptr;
            }
            else
                newQuad->opcode = QOP_MOVE;

            break;
        case NODE_UNOP: {
            switch (stmtUnion.unNode->op) {
                case '*':
                    newQuad->opcode = unopToQop(stmtUnion.unNode->op);
                    newQuad->lval = (struct astnode_quad_node*)genRegister();
                    lastQuad = stmtToQuad(stmtUnion.unNode->opand, lastQuad, firstQuad, false);
                    newQuad->rval1 = lastQuad->lval;
                    typeCheck(newQuad);
            }
        }
        case NODE_BINOP: {
            switch (stmtUnion.binNode->op) {
                case '=':
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, false);
                    newQuad->rval1 = lastQuad->lval;
                    newQuad->lval = genLval(stmtUnion.binNode->left, &lastQuad);
                    break;
                case '.': //TODO: do we want to support this?
                    fprintf(stderr, "Error: struct member access unimplemented\n");
                    exit(EXIT_FAILURE);
                    break;
                case ',': //Probably best to break each comma into its own quad
                default:
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    lastQuad = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad, false);
                    newQuad->rval1 = lastQuad->lval;
                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, false);
                    newQuad->rval2 = lastQuad->lval;
                    newQuad->lval = (struct astnode_quad_node*)genRegister();
                    //TODO: type checking
            }
            break;
        }
    }

    //This should occur at end because of depth-first recursion
    //Should be assigned to first generated quad
    if(*firstQuad == NULL) {
        printf("f%d", newQuad->opcode);
        *firstQuad = newQuad;
    }
    if(lastQuad != NULL){
        printf("n%d", newQuad->opcode);
        newQuad->next = NULL;
        newQuad->prev = lastQuad;
        lastQuad->next = newQuad;
    }
    return newQuad;
}

enum quad_opcode unopToQop(int op){
    switch (op) {
        case '*': return QOP_LOAD;
    }
    fprintf(stderr, "Error: unknown unop %d\n", op);
    exit(EXIT_FAILURE);
}

enum quad_opcode binopToQop(int op){
    switch (op) {
        case '+': return QOP_ADD;
        case '-': return QOP_SUB;
        case '%': return QOP_MOD;
        case '*': return QOP_MUL;
        case '/': return QOP_DIV;
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
    static int registerCounter = 1;
    struct astnode_quad_register *newReg = mallocSafe(sizeof(struct astnode_quad_register));
    newReg->type = NODE_QUAD_VAL;
    newReg->quadType = QUADNODE_REGISTER;
    newReg->registerNum = registerCounter;
    registerCounter++;
    return newReg;
}

struct astnode_quad_node *genLval(struct astnode_hdr *node, struct astnode_quad **lastQuad){
    //TODO: if var - assign to var node
    //TODO: if array - compute array offset as ptr, assign there
    //TODO: else - problem
    union astnode nodeUnion = (union astnode)node;
    switch (node->type) {
        case NODE_SYMTAB: {
            switch (nodeUnion.symEntry->st_type) {
                case ENTRY_VAR: {
                    struct astnode_quad_base *varNode = mallocSafe(sizeof(struct astnode_quad_base));
                    varNode->type = NODE_QUAD_VAL;
                    varNode->quadType = nodeUnion.symEntry->parentTab->scope == SCOPE_FILE ? QUADNODE_GLOBAL : (
                        nodeUnion.symEntry->inProto ? QUADNODE_PARAM : QUADNODE_LOCAL );
                    varNode->node = node;
                    return (struct astnode_quad_node*) varNode;
                }
            }
            break;
        }
        case NODE_UNOP: {
            //If determine that new quads need to be emitted, call stmtToQuad using lastQuad, adjust lastQuad accordingly
            break;
        }
        default:
            //error
            break;
    }
}

void typeCheck(struct astnode_quad *quad){
    switch (quad->opcode) {
        case QOP_LOAD:
            unaryConvCheck(quad->rval1); // TODO: might not be necessary
            if (quad->rval1->dataType->type != NODE_PTR){
                fprintf(stderr, "Error: invalid type argument of unary '*'\n");
                exit(EXIT_FAILURE);
            }
            break;

    }
}

void unaryConvCheck(struct astnode_quad_node *node){
    if (node->dataType->type == NODE_TYPESPEC){
        enum type_flag type = ((struct astnode_typespec*)node->dataType)->stype;
        if (hasFlag(type,sint_type) || hasFlag(type,char_type)){
            fprintf(stderr, "Error: unary conversions specify an implicit cast and casting is not implemented\n");
            exit(EXIT_FAILURE);
        }
    }
}

void printQuads(struct basic_block *basicBlock, char *fname){
    printf("%s:\n", fname);
    while (basicBlock != NULL){
        printf(".BB%d.%d\n", basicBlock->funcIdx, basicBlock->blockIdx);
        struct astnode_quad *quad = basicBlock->quads;
        while (quad != NULL){
            printf("\t");
            if (quad->lval != NULL)
                printQuadNode(quad->lval);

            printf(" = ");
            switch (quad->opcode) {
                case QOP_LOAD: printf("LOAD"); break;
                case QOP_ADD: printf("ADD"); break;
                case QOP_SUB: printf("SUB"); break;
                case QOP_DIV: printf("DIV"); break;
                case QOP_MUL: printf("MUL"); break;
                case QOP_MOD: printf("MOD"); break;
                case QOP_MOVE: printf("MOV"); break;
                case QOP_LAE: printf("LAE"); break;
            }

            printf(" ");
            if (quad->rval1 != NULL)
                printQuadNode(quad->rval1);
            if (quad->rval2 != NULL) {
                printf(",");
                printQuadNode(quad->rval2);
            }

            printf("\n");
            quad = quad->next;
        }

        printf("\n");
        basicBlock = basicBlock->next;
    }
}

void printQuadNode(struct astnode_quad_node *node){
    switch (node->quadType) {
        case QUADNODE_LOCAL:
        case QUADNODE_PARAM:
        case QUADNODE_GLOBAL: ;
            char *type = node->quadType == QUADNODE_LOCAL ? "lvar" : node->quadType == QUADNODE_PARAM ? "param" : "global";
            printf("%s{%s}", ((struct symtab_entry_generic*)((struct astnode_quad_base*)node)->node)->ident->value.string_val, type);
            break;
        case QUADNODE_CONST: ;
            struct LexVal *lex = (struct LexVal*)((struct astnode_quad_base*)node)->node;
            if (lex->sym == STRING)
                printf("%s", lex->value.string_val);
            else
                printf("%lld", lex->sym == CHARLIT ? lex->value.string_val[0] : lex->value.num_val.integer_val);

            break;
        case QUADNODE_REGISTER:
            printf("%%T%05d", ((struct astnode_quad_register*)node)->registerNum);
            break;
    }
}