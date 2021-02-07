#include "build/src/lexer/lexer.l.o.h"
#include "build/src/parser/parser.y.o.h"


int currLine = 1;
char currFile[256] = "<stdin>"; //Linux limit is 255
char *currStr = NULL;
unsigned int currStrLen = 0;


int main(int argc, char *argv[]) {
	if(argc > 1){
		yyin = fopen(argv[1], "r");
		if(!yyin){
			fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
			return 1;
		}
		
		strncpy(currFile, argv[1], 255);
	}

	register_printf_specifier('S', print_esc_str, print_esc_strinfo);
	yyparse();
	return 0;
}
