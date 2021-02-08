#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"

int yylex(void);
void yyerror(char const*);

void printAst(struct astnode_hdr *hdr, int lvl);

struct astnode_hdr*  allocLexCtr(struct LexVal *inNode, int tokNum);
struct astnode_hdr*  allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType);

struct astnode_lst* allocList(struct astnode_hdr *el);
void addToList(struct astnode_lst *lst, struct astnode_hdr *el);

struct astnode_hdr* allocFunc(struct astnode_hdr *name, struct astnode_lst *lst);

#endif