#ifndef LEXER_COMMON_H
#define LEXER_COMMON_H

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <printf.h>

#include "ltests/tokens-manual.h"
#include "parser/parser.y.o.h"

#define hasFlag(x, flag)	((x & flag) == flag)
enum node_type {
    NODE_LEX,
    NODE_UNOP,
    NODE_BINOP,
    NODE_TEROP,
    NODE_LST,
    NODE_FNCN
};

typedef union {
	unsigned long long int integer_val;
	float float_val;
	double double_val;
	long double ldouble_val;
} NUMTYPE;

enum token_flags {
	int_type = 0x01,
	uint_type = 0x02,
	lint_type = 0x04,
	ulint_type = 0x04 | 0x02,
	llint_type = 0x08,
	ullint_type = 0x08 | 0x02,
	float_type = 0x10,
	double_type = 0x20,
	ldouble_type = 0x40,
	flag_escaped = 0x80, //For processing escaped characters appropriately
	flag_octal = 0x100,
	flag_hex = 0x200
};

//TODO: change this to something more useful
typedef union {
	char *string_val;
	NUMTYPE num_val;
} LexVals;

struct LexVal {
    enum node_type type;
    int sym;
	char *file;
	int line;
	int len;
	enum token_flags flags;
	LexVals value;
};

//Present only for identifying type of node - contains any common fields
struct astnode_hdr{
    enum node_type type;
};

struct astnode_unop {
    enum node_type type;
    int op;
    struct astnode_hdr *opand;
};

struct astnode_binop {
    enum node_type type;
    int op;
    struct astnode_hdr *left, *right;
};

struct astnode_terop {
    enum node_type type;
    struct astnode_hdr *first, *second, *third;
};

struct astnode_lst {
    enum node_type type;
    int numVals;
    struct astnode_hdr** els;
};

struct astnode_fncn {
    enum node_type type;
    struct astnode_hdr *name;
    struct astnode_lst *lst;
};

union astnode {
    struct astnode_hdr *hdr;
    struct LexVal *lexNode;
    struct astnode_unop *unNode;
    struct astnode_binop *binNode;
    struct astnode_terop *terNode;
    struct astnode_lst *lst;
    struct astnode_fncn *fncn;
};

union astnode yylval;

void *mallocSafeLex(size_t size);

void allocLex();

int print_esc_str(FILE *stream, const struct printf_info *info, const void *const *args);

int print_esc_strinfo(const struct printf_info *info, size_t n, int *argtypes, int *size);

void processLine();

void setStr(union astnode val, char *txt, size_t len);

void printWarning(union astnode val, char *txt, int orig_flags);

void setInt(union astnode val, char *txt, int flags, int base);

void setFloat(union astnode val, char *txt, int flags);

void resetString();

void initString();

void addChars(char *txt, size_t len, int flags);

void endString(union astnode yylval);

void endChar(union astnode yylval);

void printLex(struct LexVal *node, int tokType);

#endif
