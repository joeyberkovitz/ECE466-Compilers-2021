/* C Parser */

%{
	#include "parser_common.h"
	#include "lexer/lexer_common.h"
%}

%define api.value.type {struct LexVal}
%token NUMBER

%% /* Grammar */
exp:
	NUMBER	{ printf("Found number %llu\n", $1.value.num_val.integer_val); }


%% /* Code */