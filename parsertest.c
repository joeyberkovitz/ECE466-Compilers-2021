#include "build/src/lexer/lexer.l.o.h"
#include "build/src/parser/parser.y.o.h"
#include <stdio.h>

#if GENQUAD == 1
    int doGenQuad = 1;
#else
    int doGenQuad = 0;
#endif

#if GENASSEMBLY == 1
    int doGenAssembly = 1;
#else
    int doGenAssembly = 0;
#endif


int currLine = 1;
char currFile[256] = "<stdin>"; //Linux limit is 255
// for quad debugging
int quadLastLine = 1;
char quadLastFile[256] = "<stdin>";
char *currStr = NULL;
FILE *outFile;
unsigned int currStrLen = 0;


int main(int argc, char *argv[]) {
    setvbuf(stdout, NULL, _IONBF, 0);
    if(doGenAssembly){
        outFile = fopen("out.S", "w");
    }


    if(argc > 1){
		yyin = fopen(argv[1], "r");
		if(!yyin){
			fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
			return 1;
		}
		
		strncpy(currFile, argv[1], 255);
	}

	register_printf_specifier('S', print_esc_str, print_esc_strinfo);
	//yydebug = 1;
	yyparse();
	return 0;
}

