#include <stdlib.h>
#include <stdio.h>
#include "quad_common.h"
#include "../lexer/lexer_common.h"

#define LASTBB(lq, init) ((lq) == NULL ? (init) : (lq)->parentBlock)

struct basic_block* genBasicBlock(struct basic_block *prevBlock, int funcIdx, struct basic_block *existingPtr){
    struct basic_block *newBlock = existingPtr ? existingPtr : mallocSafe(sizeof(struct basic_block));
    newBlock->type = NODE_BASICBLOCK;
    newBlock->funcIdx = funcIdx;
    newBlock->quads = NULL;
    if(prevBlock != NULL){
        newBlock->blockIdx = prevBlock->blockIdx + 1;
        newBlock->brPt = prevBlock->brPt;
        newBlock->ctPt = prevBlock->ctPt;
        newBlock->prev = prevBlock;
        if(prevBlock->next != NULL){
            newBlock->next = prevBlock->next;
            newBlock->next->prev = newBlock;

            struct basic_block *currBlock = newBlock->next;
            while (currBlock != NULL){
                currBlock->blockIdx++;
                currBlock = currBlock->next;
            }

        }
        prevBlock->next = newBlock;
    }
    else{
        newBlock->brPt = newBlock->ctPt = NULL;
        newBlock->blockIdx = 1;
        newBlock->prev = NULL;
        newBlock->next = NULL;
    }
    return newBlock;
}

struct basic_block* genQuads(struct astnode_lst *stmtList, struct basic_block *prevBlock, int funcIdx, char *fname){
    struct astnode_quad *lastQuad = NULL;
    struct basic_block *initBlock = genBasicBlock(prevBlock, funcIdx, NULL);
    struct basic_block *currBlock = initBlock;
    struct astnode_quad **firstQuad = &currBlock->quads;

    for(int i = 0; i < stmtList->numVals; i++){
        if(stmtList->els[i]->type == NODE_LST){
            //This is a compound statement, recurse
            currBlock = genQuads(stmtList, currBlock, funcIdx, fname);
        }
        else{
            //Need to pass current block in here, and return in lastQuad in case of branching through IF/...
            lastQuad = stmtToQuad(stmtList->els[i], lastQuad, firstQuad, currBlock, false, false);
            if(firstQuad != NULL && *firstQuad != NULL) //Once first quad is set into BB, no need to adjust it further
                firstQuad = NULL;
            if(lastQuad->parentBlock != currBlock)
                currBlock = lastQuad->parentBlock;
        }
    }

    if(prevBlock == NULL)
        printQuads(initBlock, fname);
    return initBlock;
}

struct astnode_quad* stmtToQuad(struct astnode_hdr *stmt, struct astnode_quad *lastQuad,
        struct astnode_quad **firstQuad, struct basic_block *init_block, bool dontLEA, bool dontEmit){
    union astnode stmtUnion = (union astnode)stmt;
    if(stmt->type == NODE_LST){
        //List doesn't generate a new quad other than its children
        for(int i = 0; i < stmtUnion.lst->numVals; i++){
            lastQuad = stmtToQuad(stmtUnion.lst->els[i], lastQuad, firstQuad, init_block, false, false);
        }
        return lastQuad;
    }


    struct astnode_quad *newQuad = mallocSafe(sizeof(struct astnode_quad));
    newQuad->type = NODE_QUAD;
    newQuad->parentBlock = init_block;
    newQuad->lval = newQuad->rval1 = newQuad->rval2 = NULL;
    struct astnode_quad *lastStmtQuad;

    switch (stmt->type) {
        case NODE_SYMTAB:
        case NODE_LEX:
            // TODO: function entries
            if (stmt->type == NODE_LEX){
                if (hasFlag(stmtUnion.lexNode->tflags,float_type) || hasFlag(stmtUnion.lexNode->tflags,double_type)){
                    fprintf(stderr, "Error: float types are not implemented\n");
                    exit(EXIT_FAILURE);
                }

                newQuad->rval1 = (struct astnode_quad_node*)mallocSafe(sizeof(struct astnode_quad_const));
                newQuad->rval1->quadType = QUADNODE_CONST;
                ((struct astnode_quad_const*)newQuad->rval1)->string = stmtUnion.lexNode->sym == STRING;
                ((struct astnode_quad_const*)newQuad->rval1)->value = stmtUnion.lexNode->value;
                if (stmtUnion.lexNode->sym == CHARLIT)
                    ((struct astnode_quad_const*)newQuad->rval1)->value.num_val.integer_val = stmtUnion.lexNode->value.string_val[0];

                newQuad->rval1->dataType = allocTypespec(stmtUnion.lexNode->tflags);
            }
            else {
                newQuad->rval1 = (struct astnode_quad_node*)mallocSafe(sizeof(struct astnode_quad_var));
                newQuad->rval1->quadType = stmtUnion.symEntry->parentTab->scope == SCOPE_FILE ?
                                           QUADNODE_GLOBAL : stmtUnion.symEntry->inProto ? QUADNODE_PARAM : QUADNODE_LOCAL;
                ((struct astnode_quad_var*)newQuad->rval1)->varNode = stmt;
                if (stmtUnion.symEntry->st_type == ENTRY_FNCN){
                    struct astnode_spec_inter *newFunc = (struct astnode_spec_inter*)allocFncndecCopy(stmtUnion.fncndec);
                    newQuad->rval1->dataType = newFunc;
                }
                else
                    newQuad->rval1->dataType = stmtUnion.symEntry->child;
            }

            newQuad->rval1->type = NODE_QUAD_VAL;
            newQuad->lval = (struct astnode_quad_node*)genRegister(newQuad->rval1->dataType);
            if (((stmt->type == NODE_SYMTAB && (stmtUnion.symEntry->st_type == ENTRY_FNCN || stmtUnion.symEntry->child->type == NODE_ARY))
                    || (stmt->type == NODE_LEX && stmtUnion.lexNode->sym == STRING)) && !dontLEA) {
                bool ary = stmt->type == NODE_SYMTAB && stmtUnion.symEntry->child->type == NODE_ARY;
                newQuad->opcode = QOP_LEA;
                struct astnode_spec_inter *ptr = (struct astnode_spec_inter*)allocPtr();
                ptr->child = ary ? newQuad->lval->dataType->child : newQuad->lval->dataType;
                newQuad->lval->dataType = (struct astnode_spec_inter*)ptr;
            }
            else
                newQuad->opcode = QOP_MOVE;

            break;
        case NODE_UNOP:
            // TODO: Logical negation
            newQuad->opcode = unopToQop(stmtUnion.unNode->op);
            if (stmtUnion.unNode->op == PLUSPLUS || stmtUnion.unNode->op == MINUSMINUS || stmtUnion.unNode->op == '&') {
                newQuad->rval1 = genLval(stmtUnion.binNode->left, &lastQuad, firstQuad, stmtUnion.unNode->op != '&', stmtUnion.unNode->op == '&', dontEmit);
                if (newQuad->rval1 == NULL){
                    fprintf(stderr, "Error: %slvalue required as %s operand\n", stmtUnion.unNode->op == '&' ? "" : "modifiable ",
                            stmtUnion.unNode->op == '&' ? "unary '&'" : stmtUnion.unNode->op == PLUSPLUS ? "increment" : "decrement");
                    exit(EXIT_FAILURE);
                }
            }
            else if (stmtUnion.unNode->op == SIZEOF){
                struct astnode_quad *temp = stmtToQuad(stmtUnion.unNode->opand, lastQuad, firstQuad, LASTBB(lastQuad, init_block), true, true);
                // int_type to allow greater variety of expressions, may really be other type
                unsigned long long size = (unsigned long long)computeSizeof((struct astnode_hdr*)temp->lval->dataType, true);
                struct astnode_quad_node *node = allocQuadConst(int_type, (LexVals)(NUMTYPE)size, false);
                newQuad->rval1 = node;
            }
            else {
                lastQuad = stmtToQuad(stmtUnion.unNode->opand, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, false);
                switch (stmtUnion.unNode->op) {
                    case '*':
                        newQuad->rval1 = lastQuad->lval;
                        break;
                    case '+':
                    case '-': {
                        struct astnode_quad_node *node = allocQuadConst(int_type, (LexVals) (NUMTYPE) 0ull, false);
                        newQuad->rval1 = node;
                        newQuad->rval2 = lastQuad->lval;
                        unaryConvCheck(newQuad->rval2);
                        break;
                    }
                    case '~':
                        fprintf(stderr, "Error: bitwise operations are not implemented\n");
                        exit(EXIT_FAILURE);
                    case '!':
                        lastQuad = allocCmpQuad(lastQuad->lval, allocQuadConst(int_type, (LexVals)(NUMTYPE)0ull,false), lastQuad);
                        break;
                }
            }

            typeCheck(newQuad, stmtUnion.unNode->op, true);
            switch (stmtUnion.unNode->op) {
                case PLUSPLUS:
                case MINUSMINUS:
                case '+':
                case '-':
                case SIZEOF:
                    newQuad->lval = (struct astnode_quad_node*)genRegister(newQuad->rval1->dataType);
                    break;
                case '&': ;
                    struct astnode_spec_inter *ptr = (struct astnode_spec_inter*)allocPtr();
                    ptr->child = newQuad->rval1->dataType;
                    newQuad->lval = (struct astnode_quad_node*)genRegister(ptr);
                    break;
                case '*':
                    newQuad->lval = (struct astnode_quad_node*)genRegister(newQuad->rval1->dataType->child);
                    newQuad = allocLEAQuad(newQuad, &lastQuad, dontLEA);
                    break;
                case '!':
                    newQuad->lval = (struct astnode_quad_node*) genRegister(allocTypespec(int_type));
            }

            break;
        case NODE_BINOP: {
            switch (stmtUnion.binNode->op) {
                case SHL:
                case SHR:
                case '&':
                case '|':
                case '^':
                    fprintf(stderr, "Error: bitwise operators are not implemented\n");
                    exit(EXIT_FAILURE);
                case '=':
                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    newQuad->rval1 = lastQuad->lval;
                    struct astnode_quad_node *lnode = genLval(stmtUnion.binNode->left, &lastQuad, firstQuad, dontEmit, true, false);
                    if (lnode == NULL){
                        fprintf(stderr, "Error: modifiable lvalue required as left operand of assignment\n");
                        exit(EXIT_FAILURE);
                    }

                    if (stmtUnion.binNode->left->type == NODE_UNOP) {
                        struct astnode_quad *storeQuad = newQuad;
                        storeQuad->opcode = QOP_STORE;
                        storeQuad->rval2 = lnode;
                        typeCheck(storeQuad, '=', false);
                        if (!dontEmit) {
                            lastQuad->next = storeQuad;
                            storeQuad->prev = lastQuad;
                            lastQuad = storeQuad;
                        }
                        newQuad = mallocSafe(sizeof(struct astnode_quad));
                        newQuad->opcode = QOP_LOAD;
                        newQuad->rval1 = lnode;
                        newQuad->lval = (struct astnode_quad_node*) genRegister(lnode->dataType->child);
                        newQuad = allocLEAQuad(newQuad, &lastQuad, dontLEA);
                    }
                    else {
                        newQuad->opcode = QOP_MOVE;
                        newQuad->lval = lnode;
                        typeCheck(newQuad, '=', false);
                    }

                    if (newQuad->lval->dataType->type == NODE_PTR)
                        ((struct astnode_ptr*)newQuad->lval->dataType)->qtype = 0;
                    if (newQuad->lval->dataType->type == NODE_TYPESPEC)
                        ((struct astnode_typespec*)newQuad->lval->dataType)->qtype = 0;

                    break;
                case '.':
                    fprintf(stderr, "Error: struct member access unimplemented\n");
                    exit(EXIT_FAILURE);
                case ',':
                    lastQuad = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    lastQuad->lval->dataType = allocTypespec(void_type);
                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    return lastQuad;
                case LOGAND:
                case LOGOR: {
                    //TODO: type checking - ensure left/right are scalar after evaluation
                    //Process left stmt
                    lastQuad = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    lastQuad = allocCmpQuad(
                        lastQuad->lval,
                        allocQuadConst(int_type, (LexVals) (NUMTYPE) 0ull, false),
                        lastQuad
                    );

                    //TODO: make sure that nested &&/|| isn't a problem
                    struct astnode_quad_node *logLval = (struct astnode_quad_node*)genRegister(allocTypespec(int_type));
                    //If left stmt == true for LOGAND || left stmt == false for LOGOR, keep going
                        //Put result of right into logLval
                    //Else - put 0 into logLval
                    struct basic_block *leftBB = genBasicBlock(lastQuad->parentBlock, lastQuad->parentBlock->funcIdx, NULL);
                    struct basic_block *rightBB = genBasicBlock(leftBB, lastQuad->parentBlock->funcIdx, NULL);
                    struct basic_block *newBB = genBasicBlock(rightBB, lastQuad->parentBlock->funcIdx, NULL);
                    lastQuad = allocQuadBR(stmtUnion.binNode->op == LOGAND ? QOP_BR_EQ : QOP_BR_NEQ, leftBB, rightBB, lastQuad);

                    //Left side - if we got here, then return 1 for LOGOR, 0 for LOGAND
                    lastQuad = allocMoveQuad(logLval, allocQuadConst(int_type,
                                             (LexVals) (NUMTYPE) (stmtUnion.binNode->op == LOGAND ? 0ull : 1ull), false), NULL);
                    leftBB->quads = lastQuad;
                    lastQuad = allocQuadBR(QOP_BR_UNCOND, newBB, NULL, lastQuad);

                    //Right side - evaluate stmt, CMP to 0, store in logLval, return to newBB
                    lastQuad = stmtToQuad(stmtUnion.binNode->right, NULL, &rightBB->quads, rightBB, false, dontEmit);
                    lastQuad = allocCmpQuad(lastQuad->lval, allocQuadConst(int_type, (LexVals) (NUMTYPE) 0ull, false), lastQuad);
                    lastQuad = allocCCQuad(logLval, QOP_CC_NEQ, lastQuad);
                    lastQuad = allocQuadBR(QOP_BR_UNCOND, newBB, NULL, lastQuad);

                    //Create NO-OP QUAD to carry new basic block
                    firstQuad = &newBB->quads; //Want to assign new quad to head of newBB at end of this function
                    lastQuad = NULL; //Reset last quad for new BB
                    newQuad->opcode = QOP_NOP;
                    newQuad->parentBlock = newBB;
                    newQuad->lval = (struct astnode_quad_node*)logLval; //set lval so that future ops can find it
                    break;
                }
                case '>':
                case '<':
                case GTEQ:
                case LTEQ:
                case EQEQ:
                case NOTEQ: {
                    struct astnode_quad_node *tmpLnode, *tmpRnode;
                    lastQuad = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    tmpLnode = lastQuad->lval;

                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    tmpRnode = lastQuad->lval;

                    lastQuad = allocCmpQuad(tmpLnode, tmpRnode, lastQuad);
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    newQuad->lval = (struct astnode_quad_node *) genRegister(allocTypespec(int_type));
                    //TODO: type checking as per 6.5.8,6.5.9
                    break;
                }
                default:
                    newQuad->opcode = binopToQop(stmtUnion.binNode->op);
                    unsigned long long size = 0;
                    bool ptr1 = false, ptrDiff = false;
                    lastQuad = stmtToQuad(stmtUnion.binNode->left, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    newQuad->rval1 = lastQuad->lval;
                    if ((stmtUnion.binNode->op == '+' || stmtUnion.binNode->op == '-') && lastQuad->lval->dataType->type == NODE_PTR){
                        size = (unsigned long long)computeSizeof((struct astnode_hdr *) lastQuad->lval->dataType->child, true);
                        ptr1 = true;
                    }

                    lastQuad = stmtToQuad(stmtUnion.binNode->right, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    newQuad->rval2 = lastQuad->lval;
                    if ((stmtUnion.binNode->op == '+' || stmtUnion.binNode->op == '-') && (lastQuad->lval->dataType->type == NODE_PTR || ptr1)) {
                        if (!ptr1)
                            size = (unsigned long long) computeSizeof(
                                    (struct astnode_hdr *) lastQuad->lval->dataType->child, true);
                        else if(stmtUnion.binNode->op == '-' && lastQuad->lval->dataType->type == NODE_PTR)
                            ptrDiff = true;

                        struct astnode_quad_node *sizeNode = allocQuadConst(int_type, (LexVals) (NUMTYPE) size, false);
                        struct astnode_quad *mulDivQuad = mallocSafe(sizeof(struct astnode_quad));
                        if (ptrDiff){
                            newQuad->lval = (struct astnode_quad_node*)genRegister(newQuad->rval1->dataType);
                            mulDivQuad->opcode = QOP_DIV;
                            mulDivQuad->rval1 = newQuad->lval;
                            mulDivQuad->rval2 = sizeNode;
                            // int_type to allow greater variety of expressions, may really be other type
                            mulDivQuad->lval = (struct astnode_quad_node *) genRegister(mulDivQuad->rval2->dataType);
                            typeCheck(newQuad, stmtUnion.binNode->op, false);
                            if (!dontEmit) {
                                lastQuad->next = newQuad;
                                newQuad->prev = lastQuad;
                                lastQuad = newQuad;
                            }
                            newQuad = mulDivQuad;
                        }
                        else {
                            mulDivQuad->opcode = QOP_MUL;
                            mulDivQuad->rval1 = ptr1 ? sizeNode : newQuad->rval1;
                            mulDivQuad->rval2 = ptr1 ? newQuad->rval2 : sizeNode;
                            mulDivQuad->lval = (struct astnode_quad_node *) genRegister(mulDivQuad->rval2->dataType);
                            if (!dontEmit) {
                                lastQuad->next = mulDivQuad;
                                mulDivQuad->prev = lastQuad;
                                lastQuad = mulDivQuad;
                            }

                            newQuad->rval1 = ptr1 ? newQuad->rval1 : mulDivQuad->lval;
                            newQuad->rval2 = ptr1 ? mulDivQuad->lval : newQuad->rval2;
                        }
                    }

                    if (!ptrDiff) {
                        typeCheck(newQuad, stmtUnion.binNode->op, false);
                        newQuad->lval = (struct astnode_quad_node *) genRegister(
                                ptr1 ? newQuad->rval1->dataType : newQuad->rval2->dataType);
                    }
            }
            break;
        }
        case NODE_TEROP:
            fprintf(stderr, "Error: conditional operators are not implemented\n");
            exit(EXIT_FAILURE);
        case NODE_CAST:
            fprintf(stderr, "Error: casts are not implemented\n");
            exit(EXIT_FAILURE);
        case NODE_FNCN:
            if (stmtUnion.fncn->name->type != NODE_SYMTAB || ((struct symtab_entry_generic*)stmtUnion.fncn->name)->st_type != ENTRY_FNCN){
                fprintf(stderr, "Error: only simple calls to identifiers are implemented\n");
                exit(EXIT_FAILURE);
            }

            newQuad->opcode = QOP_CALL;
            lastQuad = stmtToQuad(stmtUnion.fncn->name, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
            newQuad->rval1 = lastQuad->lval;
            struct astnode_fncndec *fncn = (struct astnode_fncndec*)stmtUnion.fncn->name;
            for (int i = 0; i < stmtUnion.fncn->lst->numVals; i++){
                if (i < fncn->args->numVals)
                    lastQuad = argToQuad(stmtUnion.fncn->lst->els[i], fncn->args->els[i], lastQuad, firstQuad,
                                         fncn->ident->value.string_val, i, false, dontEmit);
                else{
                    if(fncn->varArgs)
                        lastQuad = argToQuad(stmtUnion.fncn->lst->els[i], NULL, lastQuad, firstQuad,
                                             fncn->ident->value.string_val, i, true, dontEmit);
                    else{
                        fprintf(stderr, "Error: too many arguments to function '%s'\n", fncn->ident->value.string_val);
                        exit(EXIT_FAILURE);
                    }
                }
            }

            newQuad->rval2 = mallocSafe(sizeof(struct astnode_quad_const));
            newQuad->rval2->type = NODE_QUAD_VAL;
            newQuad->rval2->quadType = QUADNODE_CONST;
            ((struct astnode_quad_const*)newQuad->rval2)->value.num_val.integer_val = stmtUnion.fncn->lst->numVals;
            newQuad->lval = (struct astnode_quad_node*)genRegister(fncn->child);
            break;
        case NODE_CTRL: {
            switch (stmtUnion.ctrl->ctrlType) {
                case CTRL_WHILE:
                case CTRL_IF: {
                    struct astnode_quad *lastTrueQuad, *lastFalseQuad;
                    struct basic_block *brPt = NULL, *ctPt = NULL,
                            *lastBrPt = LASTBB(lastQuad,init_block)->brPt,
                            *lastCtPt = LASTBB(lastQuad,init_block)->ctPt;
                    struct basic_block *newBB = mallocSafe(sizeof(struct basic_block));
                    if(stmtUnion.ctrl->ctrlType == CTRL_WHILE){
                        ctPt = genBasicBlock(LASTBB(lastQuad, init_block), LASTBB(lastQuad, init_block)->funcIdx, NULL);
                        ctPt->brPt = newBB;
                        ctPt->ctPt = ctPt;
                        bool wasNull = lastQuad == NULL;
                        //Insert branch in previous block to continue point in case they weren't in order
                        lastQuad = allocQuadBR(QOP_BR_UNCOND, ctPt, NULL, lastQuad);
                        if(wasNull) {
                            lastQuad->parentBlock = init_block;
                            init_block->quads = lastQuad;
                        }
                        lastQuad = NULL;
                        init_block = ctPt;
                        firstQuad = &ctPt->quads;
                    }

                    //This will frequently start a new block due to hidden logic/short circuit ops
                    lastQuad = stmtToQuad(stmtUnion.ctrl->ctrlExpr, lastQuad, firstQuad, LASTBB(lastQuad, init_block),
                                          false, dontEmit);
                    lastQuad = allocCmpQuad(lastQuad->lval, allocQuadConst(int_type, (LexVals)(NUMTYPE)0ull, false), lastQuad);


                    struct basic_block *trueBB = genBasicBlock(lastQuad->parentBlock, lastQuad->parentBlock->funcIdx, NULL);
                    lastTrueQuad = stmtToQuad(stmtUnion.ctrl->stmt, NULL, &trueBB->quads, trueBB, false, dontEmit);

                    struct basic_block *falseBB = NULL;
                    if (stmtUnion.ctrl->stmtSecondary != NULL)
                        falseBB = genBasicBlock(trueBB, lastQuad->parentBlock->funcIdx, NULL);

                    if (stmtUnion.ctrl->stmtSecondary != NULL)
                        lastFalseQuad = stmtToQuad(stmtUnion.ctrl->stmtSecondary, NULL, &falseBB->quads, falseBB,
                                                   false, dontEmit);

                    genBasicBlock(stmtUnion.ctrl->stmtSecondary ? falseBB : trueBB, lastQuad->parentBlock->funcIdx, newBB);
                    newBB->brPt = lastBrPt;
                    newBB->ctPt = lastCtPt;
                    //TODO: properly handle break/continue points
                    //This should remain in the original BB
                    lastQuad = allocQuadBR(QOP_BR_NEQ, trueBB, falseBB ? falseBB : newBB, lastQuad);

                    if(stmtUnion.ctrl->ctrlType == CTRL_WHILE)
                        allocQuadBR(QOP_BR_UNCOND, ctPt, NULL, lastTrueQuad);
                    else
                        allocQuadBR(QOP_BR_UNCOND, newBB, NULL, lastTrueQuad);

                    if (stmtUnion.ctrl->stmtSecondary != NULL)
                        allocQuadBR(QOP_BR_UNCOND, newBB, NULL, lastFalseQuad);

                    newQuad->opcode = QOP_NOP;
                    newQuad->lval = NULL;
                    newQuad->parentBlock = newBB;
                    firstQuad = &newBB->quads;
                    lastQuad = NULL;
                    break;
                }
                default:
                    fprintf(stderr, "Unsupported CTRL type: %d\n", stmtUnion.ctrl->ctrlType);
                    exit(EXIT_FAILURE);
            }
            break;
        }
        case NODE_JMP: {
            switch (stmtUnion.jump->jumpType) {
                case JUMP_BREAK:
                    if(newQuad->parentBlock != NULL && newQuad->parentBlock->brPt != NULL){
                        newQuad->opcode = QOP_BR_UNCOND;
                        newQuad->rval1 = (struct astnode_quad_node *) allocBBQuadNode(newQuad->parentBlock->brPt);
                    } else{
                        fprintf(stderr, "Error: attempt to break without a break point specified\n");
                        exit(EXIT_FAILURE);
                    }
                    break;
                case JUMP_CONT:
                    if(newQuad->parentBlock != NULL && newQuad->parentBlock->ctPt != NULL){
                        newQuad->opcode = QOP_BR_UNCOND;
                        newQuad->rval1 = (struct astnode_quad_node *) allocBBQuadNode(newQuad->parentBlock->ctPt);
                    } else{
                        fprintf(stderr, "Error: attempt to continue without a continue point specified\n");
                        exit(EXIT_FAILURE);
                    }
                    break;
                case JUMP_RET:
                    newQuad->opcode = QOP_RETURN;
                    lastQuad = stmtToQuad(stmtUnion.jump->val, lastQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
                    newQuad->rval1 = lastQuad->lval;
                    break;
            }
            break;
        }
    }

    //This should occur at end because of depth-first recursion
    //Should be assigned to first generated quad
    if(firstQuad != NULL && *firstQuad == NULL && !dontEmit)
        *firstQuad = newQuad;

    if(lastQuad != NULL && !dontEmit){
        newQuad->next = NULL;
        newQuad->prev = lastQuad;
        lastQuad->next = newQuad;
    }

    if (stmt->type == NODE_UNOP && (stmtUnion.unNode->op == PLUSPLUS || stmtUnion.unNode->op == MINUSMINUS)){
        struct astnode_hdr *binop = allocBinop(stmtUnion.unNode->opand, allocBinop(stmtUnion.unNode->opand, allocConst(NULL, 1), stmtUnion.unNode->op == PLUSPLUS ? '+' : '-'), '=');
        lastQuad = stmtToQuad(binop, newQuad, firstQuad, LASTBB(lastQuad, init_block), false, dontEmit);
    }

    return newQuad;
}

struct astnode_quad* argToQuad(struct astnode_hdr *arg, struct astnode_hdr *param, struct astnode_quad *lastQuad,
                               struct astnode_quad **firstQuad, char *fname, int numArg, bool varArg, bool dontEmit){
    struct astnode_quad *newQuad = mallocSafe(sizeof(struct astnode_quad));
    newQuad->lval = newQuad->rval1 = newQuad->rval2 = NULL;
    newQuad->type = NODE_QUAD;
    newQuad->opcode = QOP_ARG;
    newQuad->rval1 = mallocSafe(sizeof(struct astnode_quad_const));
    newQuad->rval1->type = NODE_QUAD_VAL;
    newQuad->rval1->quadType = QUADNODE_CONST;
    ((struct astnode_quad_const*)newQuad->rval1)->value.num_val.integer_val = numArg;
    //If this function is ever called with lastQuad NULL, this is problematic
    lastQuad = stmtToQuad(arg, lastQuad, firstQuad, lastQuad->parentBlock, false, dontEmit);
    newQuad->rval2 = lastQuad->lval;

    if (!varArg) {
        if (!argConvCheck(newQuad->rval2, param)){
            fprintf(stderr, "Error: incompatible type for argument %d of '%s' (note: implicit conversions are not implemented)\n",
                    numArg, fname);
            exit(EXIT_FAILURE);
        }
    }
    else{
        if (newQuad->rval2->dataType->type == NODE_TYPESPEC && ((struct astnode_typespec*)newQuad->rval2->dataType)->stype == float_type){
            fprintf(stderr, "Error: argument conversions specify an implicit cast and casting is not implemented\n");
            exit(EXIT_FAILURE);
        }
        else
            unaryConvCheck(newQuad->rval2);
    }

    if(*firstQuad == NULL && !dontEmit)
        *firstQuad = newQuad;

    if (!dontEmit) {
        newQuad->next = NULL;
        newQuad->prev = lastQuad;
        lastQuad->next = newQuad;
    }
    return newQuad;
}

enum quad_opcode unopToQop(int op){
    switch (op) {
        case PLUSPLUS: case MINUSMINUS: case SIZEOF: return QOP_MOVE;
        case '&': return QOP_LEA;
        case '*': return QOP_LOAD;
        case '+': return QOP_ADD;
        case '-': return QOP_SUB;
        case '!': return QOP_CC_EQ;
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
        case EQEQ: return QOP_CC_EQ;
        case NOTEQ: return QOP_CC_NEQ;
        case '<': return QOP_CC_LT;
        case '>': return QOP_CC_GT;
        case LTEQ: return QOP_CC_LTEQ;
        case GTEQ: return QOP_CC_GTEQ;
        case '=': return QOP_MOVE;
    }
    fprintf(stderr, "Error: unknown binop %d\n", op);
    exit(EXIT_FAILURE);
}

struct astnode_quad_register *genRegister(struct astnode_spec_inter *type){
    static int registerCounter = 1;
    struct astnode_quad_register *newReg = mallocSafe(sizeof(struct astnode_quad_register));
    newReg->type = NODE_QUAD_VAL;
    newReg->quadType = QUADNODE_REGISTER;
    newReg->registerNum = registerCounter;
    newReg->dataType = type;
    registerCounter++;
    return newReg;
}

struct astnode_quad_node *genLval(struct astnode_hdr *node, struct astnode_quad **lastQuad, struct astnode_quad **firstQuad, bool dontEmit, bool mod, bool funcDes){
    //TODO: if var - assign to var node
    //TODO: if array - compute array offset as ptr, assign there
    //TODO: else - problem
    union astnode nodeUnion = (union astnode)node;
    struct astnode_quad_node *lnode;
    switch (node->type) {
        case NODE_LEX: {
            if (nodeUnion.lexNode->sym == STRING && !mod){
                struct astnode_quad_const *constNode = (struct astnode_quad_const*)allocQuadConst(nodeUnion.lexNode->tflags, nodeUnion.lexNode->value, true);
                struct astnode_spec_inter *ptr = (struct astnode_spec_inter*)allocPtr();
                ptr->child = constNode->dataType;
                constNode->dataType->parent = ptr;
                constNode->dataType = ptr;
                lnode = (struct astnode_quad_node*)constNode;
                break;
            }
            else
                return NULL;
        }
        case NODE_SYMTAB: {
            switch (nodeUnion.symEntry->st_type) {
                case ENTRY_VAR:
                case ENTRY_FNCN: {
                    struct astnode_quad_var *varNode = mallocSafe(sizeof(struct astnode_quad_var));
                    if (nodeUnion.symEntry->st_type == ENTRY_FNCN) {
                        if (!funcDes)
                            return NULL;

                        varNode->varNode = allocFncndecCopy(nodeUnion.fncndec);
                    }
                    else {
                        if (nodeUnion.symEntry->st_type == ENTRY_VAR && nodeUnion.symEntry->child->type == NODE_ARY &&
                            mod)
                            return NULL;

                        varNode->varNode = node;
                    }

                    varNode->type = NODE_QUAD_VAL;
                    varNode->quadType = nodeUnion.symEntry->parentTab->scope == SCOPE_FILE ? QUADNODE_GLOBAL : (
                        nodeUnion.symEntry->inProto ? QUADNODE_PARAM : QUADNODE_LOCAL );
                    varNode->dataType = ((struct symtab_entry_generic*)varNode->varNode)->child;
                    lnode = (struct astnode_quad_node*) varNode;
                    break;
                }
            }
            break;
        }
        case NODE_UNOP: {
            switch (nodeUnion.unNode->op) {
                case '*':
                    *lastQuad = stmtToQuad(mod ? nodeUnion.unNode->opand : node, *lastQuad, firstQuad, (*lastQuad)->parentBlock, true, dontEmit);
                    lnode = (*lastQuad)->lval;
                    break;
            }
            //If determine that new quads need to be emitted, call stmtToQuad using lastQuad, adjust lastQuad accordingly
            break;
        }
        default:
            return NULL;
    }

    if (lnode->dataType->type == NODE_ARY && mod)
        return NULL;

    return lnode;
}

bool isInteger(struct astnode_spec_inter *node){
    if (node->type == NODE_TYPESPEC){
        enum type_flag type = ((struct astnode_typespec*)node)->stype;
        return (hasFlag(type,char_type) || hasFlag(type,sint_type) || hasFlag(type,int_type) || hasFlag(type,lint_type) ||
                hasFlag(type,llint_type) || hasFlag(type,bool_type)) && !hasFlag(type,double_type);
    }

    return false;
}

bool isFloat(struct astnode_spec_inter *node){
    if (node->type == NODE_TYPESPEC){
        enum type_flag type = ((struct astnode_typespec*)node)->stype;
        return hasFlag(type,float_type) || hasFlag(type,double_type);
    }

    return false;
}

bool isPtr(struct astnode_spec_inter *node){
    return node->type == NODE_PTR;
}

// Incomplete checks: incomplete array, void type and incomplete struct/union type
bool isIncomplete(struct astnode_spec_inter *node){
    return (node->type == NODE_ARY && !((struct astnode_ary*)node)->complete) ||
            (node->type == NODE_TYPESPEC && (((struct astnode_typespec*)node)->stype == void_type) ||
            (((struct astnode_typespec*)node)->stype == struct_type && !((struct astnode_tag*)((struct astnode_typespec*)node)->type_specs->els[0])->complete));
}

void typeCheck(struct astnode_quad *quad, int op, bool unop){
    bool unErr = false, binErr = false;
    switch (quad->opcode) {
        case QOP_LOAD:
            if (!isPtr(quad->rval1->dataType)){
                fprintf(stderr, "Error: invalid type argument of unary '*'\n");
                exit(EXIT_FAILURE);
            }
            break;
        case QOP_STORE:
            assignConvCheck(quad->rval2->dataType->child, quad->rval1->dataType);
            break;
        case QOP_SUB:
            if (isPtr(quad->rval1->dataType) && isPtr(quad->rval2->dataType)){
                if (!checkCompatibility(quad->rval1->dataType->child, quad->rval2->dataType->child, false, false) ||
                        isIncomplete(quad->rval1->dataType->child))
                    binErr = true;

                break;
            }
        case QOP_ADD:
            if (unop && !isInteger(quad->rval2->dataType))
                unErr = true;

            binaryConvCheck(quad->rval1, quad->rval2);
            if ((!isInteger(quad->rval1->dataType) || !isInteger(quad->rval2->dataType)) &&
                    (!isPtr(quad->rval1->dataType) || !isInteger(quad->rval2->dataType)) &&
                    (!isInteger(quad->rval1->dataType) || !isPtr(quad->rval2->dataType)))
                binErr = true;
            else if(isPtr(quad->rval1->dataType) || isPtr(quad->rval2->dataType)) {
                struct astnode_spec_inter *check = isPtr(quad->rval1->dataType) ? quad->rval1->dataType
                                                                                : quad->rval2->dataType;
                if (check->child->type == NODE_FNCNDEC || isIncomplete(check->child))
                    binErr = true;
            }
            break;
        case QOP_MUL:
        case QOP_DIV:
        case QOP_MOD:
            binaryConvCheck(quad->rval1, quad->rval2);
            if (!isInteger(quad->rval1->dataType) || !isInteger(quad->rval2->dataType))
                binErr = true;
            break;
        case QOP_MOVE:
            if (op == '=')
                assignConvCheck(quad->lval->dataType, quad->rval1->dataType);
            break;
    }

    if (unErr){
        fprintf(stderr, "Error: wrong type argument to unary %s\n", op == '+' ? "plus" : "minus");
        exit(EXIT_FAILURE);
    }
    if (binErr){
        fprintf(stderr, "Error: invalid operands to binary %c\n", op);
        exit(EXIT_FAILURE);
    }
}

void checkFloat(struct astnode_spec_inter *node){
    if (isFloat(node)){
        fprintf(stderr, "Error: float types are not implemented\n");
        exit(EXIT_FAILURE);
    }
}

void assignConvCheck(struct astnode_spec_inter *left, struct astnode_spec_inter *right){
    checkFloat(left);
    if (!checkCompatibility(left, right, true, false)){
        fprintf(stderr, "Error: assignment conversions specify implicit cast and casting is not implemented\n");
        exit(EXIT_FAILURE);
    }
}

bool argConvCheck(struct astnode_quad_node *arg, struct astnode_hdr *param){
    if (param->type != NODE_SYMTAB){
        fprintf(stderr, "Error: parameter which is not an entry\n");
        exit(EXIT_FAILURE);
    }

    checkFloat(arg->dataType);
    return checkCompatibility(((struct symtab_entry_generic*)param)->child, arg->dataType, true, false);
}

void unaryConvCheck(struct astnode_quad_node *node){
    checkFloat(node->dataType);
    if (node->dataType->type == NODE_TYPESPEC){
        enum type_flag type = ((struct astnode_typespec*)node->dataType)->stype;
        if (hasFlag(type,sint_type) || hasFlag(type,char_type)){
            fprintf(stderr, "Error: unary conversions specify an implicit cast and casting is not implemented\n");
            exit(EXIT_FAILURE);
        }
    }
}

void binaryConvCheck(struct astnode_quad_node *left, struct astnode_quad_node *right){
    unaryConvCheck(left);
    unaryConvCheck(right);
    if (isInteger(left->dataType) && isInteger(right->dataType) && !checkCompatibility(left->dataType, right->dataType, true, false)){
        fprintf(stderr, "Error: binary conversions specify an implicit cast and casting is not implemented\n");
        exit(EXIT_FAILURE);
    }
}

struct astnode_quad* allocLEAQuad(struct astnode_quad *quad, struct astnode_quad **lastQuad, bool dontLEA){
    if ((quad->lval->dataType->type == NODE_ARY || quad->lval->dataType->type == NODE_FNCNDEC) && !dontLEA) {
        struct astnode_quad *leaQuad = mallocSafe(sizeof(struct astnode_quad));
        leaQuad->type = NODE_QUAD;
        leaQuad->rval1 = quad->lval;
        leaQuad->lval = (struct astnode_quad_node *) genRegister(quad->lval->dataType);
        leaQuad = setLEAQuad(leaQuad, quad->lval->dataType->type == NODE_ARY);
        (*lastQuad)->next = quad;
        quad->prev = *lastQuad;
        *lastQuad = quad;
        return leaQuad;
    }
    else
        return quad;
}

struct astnode_quad* setLEAQuad(struct astnode_quad *quad, bool ary){
    quad->opcode = QOP_LEA;
    struct astnode_spec_inter *ptr = (struct astnode_spec_inter*)allocPtr();
    ptr->child = ary ? quad->lval->dataType->child : quad->lval->dataType;
    quad->lval->dataType = (struct astnode_spec_inter*)ptr;
    return quad;
}

struct astnode_quad_node* allocQuadConst(enum type_flag type, LexVals value, bool string) {
    struct astnode_quad_node *quadConstNode = mallocSafe(sizeof(struct astnode_quad_const));
    quadConstNode->type = NODE_QUAD_VAL;
    quadConstNode->quadType = QUADNODE_CONST;
    quadConstNode->dataType = allocTypespec(type);
    ((struct astnode_quad_const *) quadConstNode)->value = value;
    ((struct astnode_quad_const *) quadConstNode)->string = string;
    return quadConstNode;
}

struct astnode_hdr* allocFncndecCopy(struct astnode_fncndec *fncn){
    struct astnode_hdr *fncnNode = mallocSafe(sizeof(struct astnode_fncndec));
    memcpy(fncnNode, fncn, sizeof(struct astnode_fncndec));
    fncnNode->type = NODE_FNCNDEC;
    return fncnNode;
}

struct astnode_spec_inter* allocTypespec(enum type_flag type){
    struct astnode_typespec *typeNode = (struct astnode_typespec*)mallocSafe(sizeof(struct astnode_typespec));
    typeNode->type = NODE_TYPESPEC;
    typeNode->stype = type;
    typeNode->qtype = 0;
    typeNode->type_specs = allocList((struct astnode_hdr*)NULL);
    typeNode->type_quals = allocList((struct astnode_hdr*)NULL);
    return (struct astnode_spec_inter*)typeNode;
}

struct astnode_quad* allocQuad(enum quad_opcode opcode, struct astnode_quad_node *rval1, struct astnode_quad_node *rval2, struct astnode_quad *lastQuad){
    struct astnode_quad *newQuad = (struct astnode_quad*) mallocSafe(sizeof(struct astnode_quad));
    newQuad->type = NODE_QUAD;
    newQuad->opcode = opcode;
    newQuad->rval1 = rval1;
    newQuad->rval2 = rval2;
    //If lastQuad NULL, it's the responsibility of the caller to link the parent block in
    if(lastQuad != NULL){
        newQuad->parentBlock = lastQuad->parentBlock;
        lastQuad->next = newQuad;
        newQuad->prev = lastQuad;
    }
    else
        newQuad->prev = NULL;
    newQuad->next = NULL;
    return newQuad;
}

struct astnode_quad* allocCmpQuad(struct astnode_quad_node *left, struct astnode_quad_node *right, struct astnode_quad *lastQuad){
    //TODO: CMP doesn't return anything - make sure this won't be a problem
    //Use branch or condition code instructions to extract result
    return allocQuad(QOP_CMP, left, right, lastQuad);
}

struct astnode_quad_bb* allocBBQuadNode(struct basic_block *bb){
    struct astnode_quad_bb *bbNode = mallocSafe(sizeof(struct astnode_quad_bb));
    bbNode->bb = bb;
    bbNode->type = NODE_QUAD_VAL;
    bbNode->quadType = QUADNODE_BASICBLOCK;
    bbNode->dataType = NULL;
    return bbNode;
}

struct astnode_quad* allocQuadBR(enum quad_opcode branchType, struct basic_block *trueBB, struct basic_block *falseBB, struct astnode_quad *lastQuad){
    struct astnode_quad_bb *bbRval1 = allocBBQuadNode(trueBB);
    struct astnode_quad_bb *bbRval2 = NULL;

    if(falseBB != NULL){
        bbRval2 = allocBBQuadNode(falseBB);
    }

    return allocQuad(branchType, (struct astnode_quad_node *) bbRval1, (struct astnode_quad_node *) bbRval2, lastQuad);
}

struct astnode_quad* allocMoveQuad(struct astnode_quad_node *lval, struct astnode_quad_node *rval, struct astnode_quad *lastQuad){
    struct astnode_quad *newQuad = allocQuad(QOP_MOVE, rval, NULL, lastQuad);
    newQuad->lval = lval;
    return newQuad;
}

struct astnode_quad* allocCCQuad(struct astnode_quad_node *lval, enum quad_opcode condCode, struct astnode_quad *lastQuad){
    struct astnode_quad *newQuad = allocQuad(condCode, NULL, NULL, lastQuad);
    newQuad->lval = lval;
    return newQuad;
}

void printQuads(struct basic_block *basicBlock, char *fname){
    printf("%s:\n", fname);
    while (basicBlock != NULL){
        printf(".BB%d.%d\n", basicBlock->funcIdx, basicBlock->blockIdx);
        struct astnode_quad *quad = basicBlock->quads;
        while (quad != NULL){
            printf("\t");
            if (quad->lval != NULL) {
                printQuadNode(quad->lval);
                printf(" = ");
            }
            else
                printf("          ");

            switch (quad->opcode) {
                case QOP_LOAD: printf("LOAD"); break;
                case QOP_STORE: printf("STORE"); break;
                case QOP_ADD: printf("ADD"); break;
                case QOP_SUB: printf("SUB"); break;
                case QOP_DIV: printf("DIV"); break;
                case QOP_MUL: printf("MUL"); break;
                case QOP_MOD: printf("MOD"); break;
                case QOP_MOVE: printf("MOV"); break;
                case QOP_LEA: printf("LEA"); break;
                case QOP_ARG: printf("ARG"); break;
                case QOP_CALL: printf("CALL"); break;
                case QOP_NOP: printf("NOOP"); break;
                case QOP_CMP: printf("CMP"); break;
                case QOP_BR_EQ: printf("BR_EQ"); break;
                case QOP_BR_NEQ: printf("BR_NEQ"); break;
                case QOP_BR_GT: printf("BR_GT"); break;
                case QOP_BR_LT: printf("BR_LT"); break;
                case QOP_BR_GTEQ: printf("BR_GTEQ"); break;
                case QOP_BR_LTEQ: printf("BR_LTEQ"); break;
                case QOP_BR_UNCOND: printf("BR"); break;
                case QOP_CC_EQ: printf("CC_EQ"); break;
                case QOP_CC_NEQ: printf("CC_NEQ"); break;
                case QOP_CC_GT: printf("CC_GT"); break;
                case QOP_CC_LT: printf("CC_LT"); break;
                case QOP_CC_GTEQ: printf("CC_GTEQ"); break;
                case QOP_CC_LTEQ: printf("CC_LTEQ"); break;
                case QOP_RETURN: printf("RETURN"); break;
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
            printf("%s{%s}", ((struct symtab_entry_generic*)((struct astnode_quad_var*)node)->varNode)->ident->value.string_val, type);
            break;
        case QUADNODE_CONST: ;
            struct astnode_quad_const *constNode = (struct astnode_quad_const*)node;
            if (constNode->string)
                printf("%s", constNode->value.string_val);
            else
                printf("%lld", constNode->value.num_val.integer_val);

            break;
        case QUADNODE_REGISTER:
            printf("%%T%05d", ((struct astnode_quad_register*)node)->registerNum);
            break;
        case QUADNODE_BASICBLOCK:
            printf(".BB%d.%d", ((struct astnode_quad_bb*)node)->bb->funcIdx, ((struct astnode_quad_bb*)node)->bb->blockIdx);
            break;
    }
}