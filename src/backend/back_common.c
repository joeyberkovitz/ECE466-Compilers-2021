#include "back_common.h"

//TODO: add hook when defining globals in parser to callback to here
//Alternatively - every time a function is assembled, look through file scope and declare any new globals

extern FILE *outFile;

void assembleFunc(struct basic_block *bb, struct astnode_fncndec *func){
    //Create strings in assembly
    fprintf(outFile, ".section    .rodata\n");
    asmblStrings(bb);
    fprintf(outFile, ".text\n");
    fprintf(outFile, ".globl    %S\n", (wchar_t*)func->ident->value.string_val);
    fprintf(outFile, ".type    %S, @function\n", (wchar_t*)func->ident->value.string_val);

    fprintf(outFile, "%S:\n", (wchar_t*)func->ident->value.string_val);
    //Setup function stack
    fprintf(outFile, "pushl   %%ebp\n");
    fprintf(outFile, "movl    %%esp, %%ebp\n");
    long stackSize = computeStackSize(bb, func);
    stackSize += stackSize % 16;
    //TODO: this seems overly large in some cases
    fprintf(outFile, "subl    $%ld, %%esp\n", stackSize);

    //At this point all registers/vars should have offsets on stack
    struct basic_block *currBB = bb;
    struct astnode_quad *currQuad = NULL;
    while(currBB != NULL){
        fprintf(outFile, ".BB%d.%d:\n", currBB->funcIdx, currBB->blockIdx);
        currQuad = currBB->quads;
        while(currQuad != NULL){

            quadToAsmbly(currQuad);

            currQuad = currQuad->next;
        }
        currBB = currBB->next;
    }
}

long computeStackSize(struct basic_block *bb, struct astnode_fncndec *func){
    long totalSize = 0;
    long startPos = 0;
    long newSize = 0;

    struct basic_block *currBB = bb;
    struct astnode_quad *currQuad = NULL;
    while (currBB != NULL){
        currQuad = currBB->quads;
        while (currQuad != NULL){
            if(currQuad->lval != NULL) {
                newSize = computeQValSize(currQuad->lval, startPos);
                startPos -= newSize;
                totalSize += newSize;
            }

            if(currQuad->rval1 != NULL){
                newSize = computeQValSize(currQuad->rval1, startPos);
                startPos -= newSize;
                totalSize += newSize;
            }

            if(currQuad->rval2 != NULL){
                newSize = computeQValSize(currQuad->rval2, startPos);
                startPos -= newSize;
                totalSize += newSize;
            }

            currQuad = currQuad->next;
        }
        currBB = bb->next;
    }


    if(func->args != NULL) {
        long posArgStack = 4;
        //Assign arg positions from right to left
        for (int i = func->args->numVals - 1; i >= 0; i--) {
            long argSize = (long)computeSizeof(func->args->els[i], false);
            struct astnode_var *argNode = (struct astnode_var*)func->args->els[i];
            if(argNode->st_type != ENTRY_VAR){
                fprintf(stderr, "%s:%d: Error: function argument not of type VAR\n", argNode->file, argNode->line);
                exit(EXIT_FAILURE);
            }
            if(argSize != 4){
                fprintf(stderr, "%s:%d: Error: functions with non-integer or pointer arguments not supported\n", argNode->file, argNode->line);
                exit(EXIT_FAILURE);
            }

            //Assuming 32-bit
            //Args go on stack above rbp (positive offset)
            posArgStack += (argNode->offset = posArgStack + argSize);
        }
    }
    return totalSize;
}

long computeQValSize(struct astnode_quad_node *node, long startPos){
    //Only care about locals and registers here, other vars don't directly hit stack
    //Params will be computed separately
    switch (node->quadType) {
        case QUADNODE_LOCAL: {
            struct astnode_var *varNode = ((struct astnode_var*)((struct astnode_quad_var*)node)->varNode);
            if(varNode->st_type != ENTRY_VAR){
                fprintf(stderr, "%s:%d: Error: trying to compute quad val size of non-var node ", varNode->file, varNode->line);
                exit(EXIT_FAILURE);
            }
            if(varNode->offset == 0){
                long varSize = (long)computeSizeof((struct astnode_hdr*)varNode, false);
                varNode->offset = startPos - varSize;
                return varSize;
            }
            break;
        }
        case QUADNODE_REGISTER: {
            struct astnode_quad_register *reg = (struct astnode_quad_register*)node;
            if(reg->stackPos == 0){
                long typeSize = (long)computeSizeofTypespec(getType(reg->dataType));;
                reg->stackPos = startPos - typeSize; //grow up
                return typeSize;
            }
            break;
        }
        default: return 0;
    }
    return 0;
}

void quadToAsmbly(struct astnode_quad *quad){
    switch (quad->opcode) {
        case QOP_LOAD:
            break;
        case QOP_STORE:
            break;
        case QOP_ADD:
        case QOP_SUB:
        case QOP_MUL:{
            //Technically add/sub/multiply isn't allowed for short/char and should be allowed for long long
            char opSuffix;
            char *op = quad->opcode == QOP_ADD ? "add" : quad->opcode == QOP_MUL ? "imul" : "sub";
            char *reg1, *reg2;
            size_t opSize = computeSizeofTypespec(getType(quad->rval1->dataType));
            if(opSize == 4){
                opSuffix = 'l';
                reg1 = "eax";
                reg2 = "edx";
            }
            else if(opSize == 2){
                opSuffix = 'w';
                reg1 = "ax";
                reg2 = "dx";

            }
            else if(opSize == 1){
                opSuffix = 'b';
                reg1 = "al";
                reg2 = "dl";
            }
            else {
                fprintf(stderr, "Error: unsupported operation size for add/sub/mul %zu\n", opSize);
                exit(EXIT_FAILURE);
            }

            //rval1 --> eax or equivalent
            fprintf(outFile, "mov%c    ", opSuffix);
            asmblVal(quad->rval1);
            fprintf(outFile, ", %%%s\n",reg1);
            //rval2 --> edx or equivalent
            fprintf(outFile, "mov%c    ", opSuffix);
            asmblVal(quad->rval2);
            fprintf(outFile, ", %%%s\n",reg2);

            //Perform actual add/subtract
            if(quad->opcode == QOP_MUL && opSize == 1){
                //See: https://docs.oracle.com/cd/E19455-01/806-3773/6jct9o0am/index.html
                //Use 1-address version for char, retrieve result from AL (lower byte of AX)
                fprintf(outFile, "imulb    %%%s\n", reg2);
            }
            else
                fprintf(outFile, "%s%c    %%%s,%%%s\n", op, opSuffix, reg2, reg1);

            //Move result into lval
            if(quad->lval != NULL) {
                fprintf(outFile, "mov%c    %%%s, ", opSuffix, reg1);
                asmblVal(quad->lval);
                fprintf(outFile, "\n");
            }

            break;
        }
        case QOP_DIV:
        case QOP_MOD: {
            char *op;
            //Only 32-bit is supported, others can be supported but are complex
            size_t opSize = computeSizeofTypespec(getType(quad->rval1->dataType));
            bool isUnsigned = hasFlag(getType(quad->rval1->dataType), unsigned_type);
            if(opSize != 4){
                fprintf(stderr, "Error: only 4-bytes supported for DIV/MOD\n");
                exit(EXIT_FAILURE);
            }
            //Quotient in EAX for DIV
            //Remainder in EDX for MOD

            //Move rval1 into EAX and upgrade to long double (64-bit)
            fprintf(outFile, "movl    ");
            asmblVal(quad->rval1);
            fprintf(outFile, ", %%eax\n");
            if(isUnsigned) {
                fprintf(outFile, "movl $0, %%edx\n"); //Clear out EDX
                op = "divl";
            }
            else {
                fprintf(outFile, "cltd\n"); //Sign extend into double long
                op = "idivl";
            }

            fprintf(outFile, "%s    ", op);
            asmblVal(quad->rval2);
            fprintf(outFile, "\n");

            if(quad->lval != NULL) {
                if (quad->opcode == QOP_DIV)
                    fprintf(outFile, "movl    %%eax, ");
                else
                    fprintf(outFile, "movl    %%edx, ");

                asmblVal(quad->lval);
                fprintf(outFile, "\n");
            }

            break;
        }
        case QOP_MOVE: {
            size_t regSize = computeSizeofTypespec(getType(quad->lval->dataType));
            char *op, *tmpReg;
            if(regSize == 4){
                op = "movl";
                tmpReg = "%eax";
            }
            else if(regSize == 2){
                op = "movw";
                tmpReg = "%ax";
            }
            else if(regSize == 1){
                op = "movb";
                tmpReg = "%al";
            }
            else if(regSize == 8){
                //TODO: do we need to support 64-bit in 32-bit environment?
                fprintf(stderr, "Error: 64-bit not implemented\n");
                exit(EXIT_FAILURE);
            }

            if(regSize == 1 || regSize == 2 || regSize == 4){
                fprintf(outFile, "%s    ", op);
                asmblVal(quad->rval1);
                fprintf(outFile, ", %s\n", tmpReg);
                fprintf(outFile, "%s    %s, ",op, tmpReg);
                asmblVal(quad->lval);
                fprintf(outFile, "\n");
            }

            break;
        }
        case QOP_LEA:
            fprintf(outFile, "leal    ");
            asmblVal(quad->rval1);
            fprintf(outFile, ", %%eax\n");

            fprintf(outFile, "movl    %%eax, ");
            asmblVal(quad->lval);
            fprintf(outFile, "\n");
            break;
        case QOP_ARG:
            if (quad->rval2 == NULL) {
                fprintf(stderr, "Error: ARG must have rval2 set\n");
                exit(EXIT_FAILURE);
            }
            // If arg is smaller than int, need to upgrade
            fprintf(outFile, "pushl    ");
            asmblVal(quad->rval2);
            fprintf(outFile, "\n");
            break;
        case QOP_CALL:
            fprintf(outFile, "call    ");
            asmblVal(quad->rval1);
            fprintf(outFile, "\n");

            if(quad->lval != NULL){
                fprintf(outFile, "movl    %%eax, ");
                asmblVal(quad->lval);
                fprintf(outFile, "\n");
            }

            if(quad->rval2 == NULL || quad->rval2->quadType != QUADNODE_CONST){
                fprintf(stderr, "Error: CALL must have CONST rval2\n");
                exit(EXIT_FAILURE);
            }

            fprintf(outFile, "addl    $%llu, %%esp\n",((struct astnode_quad_const*)quad->rval2)->value.num_val.integer_val*4);
            break;
        case QOP_NOP:
            break;
        case QOP_CMP:
            break;
        case QOP_RETURN:
            if(quad->rval1 != NULL){
                fprintf(outFile, "movl    ");
                asmblVal(quad->rval1);
                fprintf(outFile, ", %%eax\n");
            }
            fprintf(outFile, "leave\nret\n");
            break;
        case QOP_BR_EQ:
            break;
        case QOP_BR_NEQ:
            break;
        case QOP_BR_GT:
            break;
        case QOP_BR_LT:
            break;
        case QOP_BR_GTEQ:
            break;
        case QOP_BR_LTEQ:
            break;
        case QOP_BR_EQU:
            break;
        case QOP_BR_NEQU:
            break;
        case QOP_BR_GTU:
            break;
        case QOP_BR_LTU:
            break;
        case QOP_BR_GTEQU:
            break;
        case QOP_BR_LTEQU:
            break;
        case QOP_BR_UNCOND:
            break;
        case QOP_CC_EQ:
            break;
        case QOP_CC_NEQ:
            break;
        case QOP_CC_GT:
            break;
        case QOP_CC_LT:
            break;
        case QOP_CC_GTEQ:
            break;
        case QOP_CC_LTEQ:
            break;
        case QOP_CC_EQU:
            break;
        case QOP_CC_NEQU:
            break;
        case QOP_CC_GTU:
            break;
        case QOP_CC_LTU:
            break;
        case QOP_CC_GTEQU:
            break;
        case QOP_CC_LTEQU:
            break;
    }
}

void asmblStrings(struct basic_block *bb){
    static size_t stringCounter = 0;

    struct basic_block *currBB = bb;
    struct astnode_quad *currQuad = NULL;
    while(currBB != NULL){
        currQuad = currBB->quads;
        while(currQuad != NULL){
            if(currQuad->rval1 != NULL && currQuad->rval1->quadType == QUADNODE_CONST
                && ((struct astnode_quad_const*)currQuad->rval1)->string
                && ((struct astnode_quad_const*)currQuad->rval1)->stringNum == 0){
                ((struct astnode_quad_const*)currQuad->rval1)->stringNum = stringCounter++;
                fprintf(outFile, ".LC%zu:\n", ((struct astnode_quad_const*)currQuad->rval1)->stringNum);
                fprintf(outFile, "        .string \"%S\"\n", (wchar_t*)((struct astnode_quad_const*)currQuad->rval1)->value.string_val);
            }

            if(currQuad->rval2 != NULL && currQuad->rval2->quadType == QUADNODE_CONST
                && ((struct astnode_quad_const*)currQuad->rval2)->string
                && ((struct astnode_quad_const*)currQuad->rval2)->stringNum == 0){
                ((struct astnode_quad_const*)currQuad->rval2)->stringNum = stringCounter++;
                fprintf(outFile, ".LC%zu:\n", ((struct astnode_quad_const*)currQuad->rval2)->stringNum);
                fprintf(outFile, "        .string \"%S\"\n", (wchar_t*)((struct astnode_quad_const*)currQuad->rval2)->value.string_val);
            }

            currQuad = currQuad->next;
        }
        currBB = currBB->next;
    }
}

void asmblVal(struct astnode_quad_node *node){
    if(node->quadType == QUADNODE_CONST){
        struct astnode_quad_const *constNode = (struct astnode_quad_const*)node;

        if(constNode->string)
            fprintf(outFile, ".LC%zu", constNode->stringNum);
        else
            fprintf(outFile, "$%lld", constNode->value.num_val.integer_val);
    }
    else if(node->quadType == QUADNODE_REGISTER){
        struct astnode_quad_register *regNode = (struct astnode_quad_register*)node;
        fprintf(outFile, "%ld(%%ebp)", regNode->stackPos);
    }
    else if(node->quadType == QUADNODE_GLOBAL){
        fprintf(outFile, "%s", ((struct symtab_entry_generic*)((struct astnode_quad_var*)node)->varNode)->ident->value.string_val);
    }
    else if(node->quadType == QUADNODE_LOCAL || node->quadType == QUADNODE_PARAM){
        struct astnode_quad_var *quadVar = (struct astnode_quad_var*)node;
        struct astnode_var *varNode = (struct astnode_var*)quadVar->varNode;
        if(varNode->type != NODE_SYMTAB || varNode->st_type != ENTRY_VAR){
            fprintf(stderr, "Error: local/param variable must have an associated ENTRY_VAR\n");
            exit(EXIT_FAILURE);
        }
        fprintf(outFile, "%ld(%%ebp)", varNode->offset);
    }
}