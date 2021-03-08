#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"
#include <stdbool.h>

int yylex(void);
void yyerror(char const*);
void* mallocSafe(size_t size);

void printTabs1(int lvl);
void printAst(struct astnode_hdr *hdr, int lvl);

struct astnode_hdr*  allocUnop(struct astnode_hdr *opand, int opType);
struct astnode_hdr*  allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType);
struct astnode_hdr*  allocTerop(struct astnode_hdr *first, struct astnode_hdr *second, struct astnode_hdr *third);

struct astnode_hdr*  allocPostIncDec(struct LexVal *op, struct astnode_hdr *opand, int opType);
struct astnode_hdr*  allocSizeof();
struct astnode_cast*  allocCast();
struct astnode_hdr*  allocAssignment(struct astnode_hdr *left, struct astnode_hdr *right, struct LexVal *opType);

struct astnode_lst* allocList(struct astnode_hdr *el);
void addToList(struct astnode_lst *lst, struct astnode_hdr *el);

struct astnode_hdr* allocFunc(struct astnode_hdr *name, struct astnode_lst *lst);
size_t computeSizeof(struct astnode_hdr* el);
size_t getStructSize(struct astnode_tag *structNode, bool ignoreIncomplete);

enum tab_type {
    TAB_GENERIC,
    TAB_STRUCT,
    TAB_FUNC
};

enum symtab_type {
    ENTRY_GENERIC,
    ENTRY_VAR,
    ENTRY_FNCN,
    ENTRY_STAG,
    ENTRY_UTAG,
    ENTRY_SMEM,
    ENTRY_UMEM
    // TODO: labels
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
    SCOPE_PROTO,

    //Not per spec, but indicates in struct
    //only store members instead of variables
    //nested structs go in outer scope
    SCOPE_STRUCT
};

struct symtab;

union symtab_entry {
    struct symtab_entry_generic *generic;
    struct astnode_var *var;
    struct astnode_memb *memb;
    struct astnode_fncndec *fncn;
    struct astnode_tag *tag;
    // TODO: labels
};

enum entry_spec_type {
    SPEC_STORAGE,
    SPEC_TYPE_SPEC,
    SPEC_TYPE_QUAL
};

enum storage_class {
    STG_TYPEDEF,
    STG_EXTERN,
    STG_STATIC,
    STG_AUTO,
    STG_REGISTER
};

enum qual_flag {
    QUAL_CONST = 0x1,
    QUAL_RESTRICT = 0x2,
    QUAL_VOLATILE = 0x4
};

struct symtab_entry_generic {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
    enum symtab_type st_type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;
    char *file;
    int line;

    //Specifiers
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;
    /*struct astnode_hdr **typeSpec;
    size_t numTypeSpec;
    struct LexVal *typeQual;*/
};

struct astnode_var {
    struct symtab_entry_generic;

    //Variable specific values:
    long long offset; //Stack frame offset for auto storage
};

struct astnode_memb {
    struct symtab_entry_generic;

    //Struct/Union member specific attributes
    size_t structOffset;
    //Unsupported but params present:
    char bitWidth;
    char bitOffset;
};

struct astnode_fncndec {
    struct symtab_entry_generic;

    bool unknown;
    bool none;
    bool varArgs;
    struct symtab_func *scope;
    struct astnode_lst *args;
};

struct astnode_tag {
    struct symtab_entry_generic;

    //Struct specific info:
    bool complete;
    bool incAry;
    bool namedEntry;
    struct symtab *container;
};

/* TODO: labels at label-statements */

// header for type specifiers, pointers and arrays
struct astnode_spec_inter {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
};

struct astnode_typespec {
    enum node_type type;
    struct astnode_spec_inter *parent, *child; // child for header

    struct astnode_spec_inter **parents;
    long numParents;

    enum type_flag stype;
    enum qual_flag qtype;

    struct astnode_lst *type_specs;
    struct astnode_lst *type_quals;
};

struct astnode_ary {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;

    int length;
    bool complete;
};

struct astnode_ptr {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;

    enum qual_flag qtype;

    struct astnode_lst *type_quals;
};

struct symtab {
    enum tab_type tabType;
    enum symtab_scope scope;
    struct symtab *parent;
    struct symtab **children; //Array of pointers to direct children - dynamically allocated
    size_t numChildren; //Number of children pointers allocated
    union symtab_entry head;
    char *file;
    int line;
};

struct symtab_struct {
    struct symtab;
    //Struct specific els:
    struct astnode_tag *parentStruct;
};

struct symtab_func {
    struct symtab;
    //Struct specific els:
    struct astnode_fncndec *parentFunc;
};

size_t getTabSize(enum tab_type tabType);
struct symtab* symtabCreate(enum symtab_scope scope, enum tab_type tabType, struct LexVal *startLex);
//Traverse up to parent scope without destroying current scope (ex: for struct sym tables)
void exitScope();
void enterBlockScope(struct LexVal *lexVal);
void enterFuncScope(struct astnode_hdr* func);
void symtabDestroy(struct symtab *symtab);
union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name, bool singleScope);
bool symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace);
bool structMembEnter(struct symtab *symtab, union symtab_entry entry);
struct astnode_hdr* symCopyAndEnter(bool enter);
struct astnode_hdr* genStruct(struct LexVal *type, struct symtab *symtab, union symtab_entry baseEntry, struct LexVal *ident, struct LexVal *scopeStart, bool complete);
void checkVoid();
int checkStructValidity();

struct symtab_entry_generic* allocEntry(enum symtab_type type, bool clear);
struct symtab_entry_generic* clearEntry(union symtab_entry entry);
size_t getEntrySize(enum symtab_type type);

void setStgSpec(union symtab_entry entry, struct symtab *symtab, struct LexVal *val);
void handleStgDefaults(union symtab_entry entry, struct symtab *symtab);
void addTypeNode(int *flags, struct astnode_lst* type_list, struct astnode_hdr *val, int flag);
void addTypeSpec(union symtab_entry entry, struct astnode_hdr *val);
void addTypeQual(enum qual_flag *qtype, struct astnode_lst *qual_types, struct LexVal *val, bool ptr);

void finalizeSpecs(union symtab_entry entry);
void finalizeStruct(struct astnode_hdr *structHdr, bool complete);

struct astnode_ptr* allocPtr();
struct astnode_spec_inter* setPtr(struct astnode_spec_inter *ptr, struct astnode_spec_inter *next);
struct astnode_spec_inter* allocAry(struct astnode_spec_inter *prev, struct LexVal *val, union symtab_entry entry, struct symtab *symtab);
void freeInterNodes();

struct astnode_fncndec* startFuncDef(bool params, struct LexVal *lexVal);
struct astnode_lst* startFncnArgs(struct astnode_hdr *arg);
void addFncnArg(struct astnode_lst *lst, struct astnode_hdr *arg);
struct astnode_spec_inter* setFncn(struct astnode_fncndec *fncndec, struct astnode_spec_inter *prev);
struct astnode_fncndec* addFuncArgs(struct astnode_lst *args, struct symtab *symtab, bool varArgs);

void printDecl(struct symtab *symtab, union symtab_entry entry, long argNum);
void printSpec(struct astnode_spec_inter *next, struct symtab *symtab, bool func, long level, long castLevel);
void printQual(enum qual_flag qflags);
void printTabs2(bool func, long level, long castLevel);
void printArgs(struct astnode_fncndec *fncn, struct symtab *symtab, bool func, long level, long castLevel);
void printStruct(struct astnode_hdr *structHdr);


struct symtab *currTab;
union symtab_entry currDecl;

#endif
