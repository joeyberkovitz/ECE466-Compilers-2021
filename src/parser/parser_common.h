#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"

int yylex(void);
void yyerror(char const*);

void printAst(struct astnode_hdr *hdr, int lvl);

struct astnode_hdr*  allocLexCtr(struct LexVal *inNode, int tokNum);
struct astnode_hdr*  allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType);

#endif