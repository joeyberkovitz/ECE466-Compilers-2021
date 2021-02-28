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
size_t computeSizeof(struct astnode_hdr* el);
size_t getStructSize(struct astnode_tag *structNode);

enum tab_type {
    TAB_GENERIC,
    TAB_STRUCT
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

    //Specifiers
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;
    /*struct astnode_hdr **typeSpec;
    size_t numTypeSpec;
    struct LexVal *typeQual;*/
};

struct astnode_var {
    //Generic values:
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
    enum symtab_type st_type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;

    //Variable specific values:
    long long offset; //Stack frame offset for auto storage
};

struct astnode_memb {
    //Generic values:
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
    enum symtab_type st_type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;

    //Struct/Union member specific attributes
    size_t structOffset;
    //Unsupported but params present:
    char bitWidth;
    char bitOffset;
};

struct astnode_fncndec {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
    enum symtab_type st_type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;

    bool unknown;
    struct symtab *scope;
};

struct astnode_tag {
    enum node_type type;
    struct astnode_spec_inter *parent, *child;
    enum symtab_type st_type;
    union symtab_entry prev;
    union symtab_entry next;
    enum symtab_ns ns;
    struct LexVal *ident;
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;

    bool complete;
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
};

struct symtab_struct {
    enum tab_type tabType;
    enum symtab_scope scope;
    struct symtab *parent;
    struct symtab **children; //Array of pointers to direct children - dynamically allocated
    size_t numChildren; //Number of children pointers allocated
    union symtab_entry head;

    //Struct specific els:
    struct astnode_tag *parentStruct;
};

size_t getTabSize(enum tab_type tabType);
struct symtab* symtabCreate(enum symtab_scope scope, enum tab_type tabType);
//Traverse up to parent scope without destroying current scope (ex: for struct sym tables)
void exitScope();
void symtabDestroy(struct symtab *symtab);
union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name, bool singleScope);
bool symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace);
bool structMembEnter(struct symtab *symtab, union symtab_entry entry);
bool varEnter(struct symtab *symtab, union symtab_entry entry);
struct astnode_hdr* genStruct(struct LexVal *type, struct symtab *symtab, union symtab_entry baseEntry, struct LexVal *ident, bool complete);

struct symtab_entry_generic* allocEntry(enum symtab_type type, bool clear);
struct symtab_entry_generic* clearEntry(union symtab_entry entry);
struct symtab_entry_generic* copyEntry(union symtab_entry entry);
size_t getEntrySize(enum symtab_type type);

void setStgSpec(union symtab_entry entry, struct symtab *symtab, struct LexVal *val);
void handleStgDefaults(union symtab_entry entry, struct symtab *symtab);
void addTypeNode(int *flags, struct astnode_lst* type_list, struct LexVal *val, int flag);
void addTypeSpec(union symtab_entry entry, struct LexVal *val);
void addTypeQual(enum qual_flag *qtype, struct astnode_lst *qual_types, struct LexVal *val, bool ptr);

void finalizeSpecs(union symtab_entry entry);

struct astnode_ptr* allocPtr();
struct astnode_spec_inter* setPtr(struct astnode_spec_inter *ptr, struct astnode_spec_inter *next, union symtab_entry entry);
struct astnode_spec_inter* allocAry(struct astnode_spec_inter *prev, struct LexVal *val, union symtab_entry entry, struct symtab *symtab);
void freeInterNodes(union symtab_entry entry);

void printDecl(struct symtab *symtab, union symtab_entry entry);
void printQual(enum qual_flag qflags);
void printStructEnd(struct astnode_hdr *structHdr);
//void allocAry(union symtab_entry entry, struct LexVal *val);


struct symtab *currTab;
union symtab_entry currDecl;

#endif
