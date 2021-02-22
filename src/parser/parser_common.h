#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"
#include <stdbool.h>

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
    ENTRY_GENERIC
};

enum symtab_ns {
    TAG,
    LABEL,
    MEMBER,
    OTHER
};

enum symtab_scope {
    SCOPE_FILE,
    SCOPE_FUNC,
    SCOPE_BLOCK,
    SCOPE_PROTO
};

union symtab_entry {
    struct symtab_entry_generic *generic;
};

enum entry_spec_type {
    SPEC_STORAGE,
    SPEC_TYPE_SPEC,
    SPEC_TYPE_QUAL
};

struct symtab_entry_generic {
    enum symtab_type type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;

    //Specifiers
    struct LexVal *storage;
    struct astnode_hdr **typeSpec;
    size_t numTypeSpec;
    struct LexVal *typeQual;
};

struct symtab {
    enum symtab_scope scope;
    struct symtab *parent;
    struct symtab **children; //Array of pointers to direct children - dynamically allocated
    size_t numChildren; //Number of children pointers allocated
    union symtab_entry head;
};

struct symtab* symtabCreate(enum symtab_scope scope);
void symtabDestroy(struct symtab *symtab);
union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name);
bool symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace);
struct symtab_entry_generic* allocEntry(enum symtab_type type);
struct symtab_entry_generic* clearEntry(union symtab_entry entry);
size_t getEntrySize(enum symtab_type type);
void setEntrySpec(union symtab_entry entry, struct astnode_hdr *val, enum entry_spec_type type);




struct symtab *currTab;
union symtab_entry currDecl;

#endif
