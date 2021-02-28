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
    NODE_FNCN,

    NODE_SYMTAB, //if present - need to check st_type for more details
    NODE_TYPESPEC,
    NODE_ARY,
    NODE_PTR
};

typedef union {
	unsigned long long int integer_val;
	float float_val;
	double double_val;
	long double ldouble_val;
} NUMTYPE;

enum type_flag {
    void_type = 0x001,
    signed_type = 0x002,
    unsigned_type = 0x004,
    char_type = 0x008,
    uchar_type = 0x008 | 0x004,
    sint_type = 0x010,
    usint_type = 0x010 | 0x004,
    int_type = 0x020,
    uint_type = 0x020 | 0x004,
    lint_type = 0x040,
    ulint_type = 0x040 | 0x004,
    llint_type = 0x080,
    ullint_type = 0x080 | 0x004,
    float_type = 0x100,
    double_type = 0x200,
    ldouble_type = 0x200 | 0x040,
    bool_type = 0x400,
    complex_type = 0x800,
    fcomplex_type = 0x800 | 0x100,
    dcomplex_type = 0x800 | 0x200,
    ldcomplex_type = 0x800 | 0x200 | 0x040,
    struct_type = 0x1000
};

enum token_flag {
    flag_escaped = 0x01,
    flag_octal = 0x02, //For processing escaped characters appropriately
    flag_hex = 0x04
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
    enum token_flag flags;
    enum type_flag tflags;
    LexVals value;
};

//Present only for identifying type of node - contains any common fields
struct astnode_hdr {
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

    struct symtab_entry_generic *symEntry;
    struct astnode_varmem *varMem;
    struct astnode_fncndec *fncndec;
    struct astnode_tag *tag;
    struct astnode_struct *nodeStruct;
    // TODO: labels

    struct astnode_ptr *ptr;
    struct astnode_spec_inter *specInter;
};

union astnode yylval;

void *mallocSafeLex(size_t size);

void allocLex();

int print_esc_str(FILE *stream, const struct printf_info *info, const void *const *args);

int print_esc_strinfo(const struct printf_info *info, size_t n, int *argtypes, int *size);

void processLine();

void setStr(union astnode val, char *txt, size_t len);

void setFlag(union astnode val, int flag, char *txt, int orig_flags);

void setInt(union astnode val, char *txt, int flags, int base);

void setFloat(union astnode val, char *txt, int flags);

void resetString();

void initString();

void addChars(char *txt, size_t len, int flags);

void endString(union astnode yylval);

void endChar(union astnode yylval);

void printLex(struct LexVal *node, int tokType);

#endif
