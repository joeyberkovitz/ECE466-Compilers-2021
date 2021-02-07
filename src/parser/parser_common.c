#include "parser_common.h"

#include <stdio.h>

void yyerror(char const* s){
	fprintf(stderr, "Got error: %s\n", s);
}

void printAst(struct astnode_hdr *hdr, int lvl){
    switch (hdr->type) {
        case NODE_LEX:
            break;
        case NODE_BINOP:
            break;
    }
}