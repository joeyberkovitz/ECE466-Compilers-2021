#include "parser_common.h"

#include <stdio.h>

void yyerror(char const* s){
	fprintf(stderr, "Got error: %s\n", s);
}