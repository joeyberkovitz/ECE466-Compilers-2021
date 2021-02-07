#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"

int yylex(void);
void yyerror(char const*);

void printAst(struct astnode_hdr *hdr, int lvl);


#endif