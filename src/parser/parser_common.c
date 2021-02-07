#include "parser_common.h"

#include <stdio.h>

void yyerror(char const* s){
	fprintf(stderr, "Got error: %s\n", s);
}

void printAst(struct astnode_hdr *hdr, int lvl){
    //TODO: print tabs for level indication
    switch (hdr->type) {
        case NODE_LEXCTR:
            printLex(((struct astnode_lex*)hdr)->lexVal, ((struct astnode_lex*)hdr)->tokenNum);
            break;
        case NODE_LEX:
            break;
        case NODE_BINOP:
            break;
    }
}

struct astnode_hdr* allocLexCtr(struct LexVal *inNode, int tokNum){
    struct astnode_lex *retNode = malloc(sizeof(struct astnode_lex));
    retNode->type = NODE_LEXCTR;
    retNode->lexVal = inNode;
    retNode->tokenNum = tokNum;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType){
    struct astnode_binop *retNode = malloc(sizeof(struct astnode_binop));
    retNode->type = NODE_BINOP;
    retNode->left = (union astnode *) left;
    retNode->right = (union astnode *) right;
    retNode->op = opType;
    return (struct astnode_hdr *) retNode;
}