#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"

int yylex(void);
void yyerror(char const*);
void* mallocSafe(size_t size);

void printTabs(int lvl);
void printAst(struct astnode_hdr *hdr, int lvl);

struct astnode_hdr*  allocUnop(struct astnode_hdr *opand, int opType);
struct astnode_hdr*  allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType);
struct astnode_hdr*  allocTerop(struct astnode_hdr *first, struct astnode_hdr *second, struct astnode_hdr *third);

struct astnode_hdr*  allocPostIncDec(struct LexVal *op, struct astnode_hdr *opand, int opType);
struct astnode_hdr*  allocAssignment(struct astnode_hdr *left, struct astnode_hdr *right, struct LexVal *opType);

struct astnode_lst* allocList(struct astnode_hdr *el);
void addToList(struct astnode_lst *lst, struct astnode_hdr *el);

struct astnode_hdr* allocFunc(struct astnode_hdr *name, struct astnode_lst *lst);

enum symtab_type {
    GENERIC
};

enum symtab_ns {
    TAG,
    LABEL,
    MEMBER,
    OTHER
};

union symtab_entry {
    struct symtab_entry_generic *generic;
};

struct symtab_entry_generic {
    enum symtab_type type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;2
    union astnode *astnode;
};

struct symtab {
    enum {
        SCOPE_FILE,
        SCOPE_FUNC,
        SCOPE_BLOCK,
        SCOPE_PROTO
    } scope;
    struct symtab *parent;
    struct symtab **children; //Array of pointers to direct children - dynamically allocated
    size_t children_len; //Number of children pointers allocated
    union symtab_entry entry;
};

#endif
