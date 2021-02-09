#include "parser_common.h"

#include <stdio.h>

void yyerror(char const* s){
	fprintf(stderr, "Got error: %s\n", s);
}

void printAst(struct astnode_hdr *hdr, int lvl){
    union astnode *node = (union astnode*) &hdr;
    //TODO: print tabs for level indication
    switch (hdr->type) {
        case NODE_LEXCTR:
            printLex(((struct astnode_lex*)hdr)->lexVal, ((struct astnode_lex*)hdr)->tokenNum);
            break;
        case NODE_LEX:
            break;
        case NODE_BINOP:
            printf("Got binop: \n");
            printAst((struct astnode_hdr *) node->binNode->left, lvl + 1);
            printAst((struct astnode_hdr *) node->binNode->right, lvl + 1);
            break;
        case NODE_LST:
            printf("Got list: \n");
            for(int i = 0; i < node->lst->numVals; i++){
                printAst(node->lst->els[i], lvl + 1);
            }
            break;
        case NODE_FNCN:
            printf("Got fncn: \n");
            printAst((struct astnode_hdr *) node->fncn->name, lvl + 1);
            printAst((struct astnode_hdr *) node->fncn->lst, lvl + 1);
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
    retNode->left = left;
    retNode->right = right;
    retNode->op = opType;
    return (struct astnode_hdr *) retNode;
}

struct astnode_lst* allocList(struct astnode_hdr *el){
    struct astnode_lst *lst = malloc(sizeof(struct astnode_lst));
    lst->type = NODE_LST;
    if(el == NULL) {
        lst->numVals = 0;
        lst->els = NULL;
    }
    else{
        lst->numVals = 1;
        lst->els = malloc(sizeof(union astnode));
        lst->els[0] = el;
    }
    return lst;
}


void addToList(struct astnode_lst *lst, struct astnode_hdr *el){
    lst->numVals++;
    lst->els = realloc(lst->els, sizeof(struct astnode_hdr*)*lst->numVals);
    lst->els[lst->numVals-1] = el;
}

struct astnode_hdr* allocFunc(struct astnode_hdr *name, struct astnode_lst *lst){
    struct astnode_fncn *fncn = malloc(sizeof(struct astnode_fncn));

    fncn->type = NODE_FNCN;
    fncn->lst = lst;
    fncn->name = name;

    return (struct astnode_hdr *) fncn;
}