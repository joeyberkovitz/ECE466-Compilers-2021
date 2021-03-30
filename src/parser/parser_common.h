#ifndef PARSER_COMMON_H
#define PARSER_COMMON_H

#include "lexer/lexer_common.h"
#include <stdbool.h>

int yylex(void);
void yyerror(char const*);
void* mallocSafe(size_t size);

void printTabs1(int lvl);
void printAst(struct astnode_hdr *hdr, int lvl, bool isFunc);
void printFunc();
void dumpStatements(struct astnode_lst *stmtLst, int level);

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

struct astnode_cast {
    enum node_type type;
    struct astnode_spec_inter *cast_spec;
    struct astnode_hdr *opand;
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

enum ctrl_type {
    CTRL_IF,
    CTRL_SWITCH,
    CTRL_WHILE,
    CTRL_DO,
    CTRL_FOR
};

struct astnode_ctrl {
    struct astnode_hdr;
    enum ctrl_type ctrlType;
    struct astnode_hdr *ctrlExpr; //for FOR loop - expect a list here
    struct astnode_hdr *stmt;
    struct astnode_hdr *stmtSecondary;
};

enum jump_type {
    JUMP_GOTO,
    JUMP_CONT,
    JUMP_BREAK,
    JUMP_RET
};

struct astnode_jump {
    struct astnode_hdr;
    enum jump_type jumpType;
    struct astnode_hdr *val; //Ident for GOTO, expression for RETURN
};

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
    ENTRY_UMEM,
    ENTRY_LABEL
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
    SCOPE_STRUCT,

    //Not per spec, but allows for handling switch cases
    SCOPE_SWITCH
};

struct symtab;

union symtab_entry {
    struct symtab_entry_generic *generic;
    struct astnode_var *var;
    struct astnode_memb *memb;
    struct astnode_fncndec *fncn;
    struct astnode_tag *tag;
    struct astnode_label *label;
};

enum linkage_type {
    LINK_EXT,
    LINK_INT,
    LINK_NONE
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
    enum linkage_type linkage;

    //Specifiers
    enum storage_class stgclass;
    struct LexVal *storageNode;
    struct astnode_typespec *type_spec;
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

    bool defined;
    bool unknown; // note: defined && unknown = no params
    bool unknownCheck; // for edge case check in symtabEnter
    bool noIdent;
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

enum label_type {
    LAB_GENERIC,
    LAB_CASE,
    LAB_DEFAULT
};

struct astnode_label {
    struct symtab_entry_generic;

    //Label specific info:
    enum label_type labelType;
    struct astnode_hdr *stmtNode;
    struct astnode_hdr *exprNode; //For case this is expression, for regular this is ident
};

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

    struct astnode_lst *stmtList; //List of statements for function/block
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
void enterSwitchScope(struct LexVal *lexVal);
void enterFuncScope(struct astnode_hdr* func);
union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name, bool singleScope, enum linkage_type linkage);
struct symtab_entry_generic* symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace);
struct symtab_entry_generic* structMembEnter(struct symtab *symtab, union symtab_entry entry);
struct astnode_hdr* symCopyAndEnter(bool enter);
struct astnode_hdr* genStruct(struct LexVal *type, struct symtab *symtab, union symtab_entry baseEntry, struct LexVal *ident, struct LexVal *scopeStart, bool complete);
void checkVoid();
int checkStructValidity();
bool checkCompatibility(struct astnode_spec_inter *entry1, struct astnode_spec_inter *entry2, struct symtab *symtab, bool qual);
bool checkCompatibilityFncn(struct astnode_fncndec *entry1, struct astnode_fncndec *entry2, struct symtab *symtab);
struct astnode_hdr* exprAssocVar(struct astnode_hdr *opand, enum symtab_ns ns, struct symtab *tab, bool isFunc);

struct symtab_entry_generic* allocEntry(enum symtab_type type, bool clear);
struct symtab_entry_generic* clearEntry(union symtab_entry entry);
void checkDeclDoesStuff(union symtab_entry decl);
size_t getEntrySize(enum symtab_type type);

void setStgSpec(union symtab_entry entry, struct symtab *symtab, struct LexVal *val);
void handleStgDefaults(union symtab_entry entry, struct symtab *symtab);
void addTypeNode(int *flags, struct astnode_lst* type_list, struct astnode_hdr *val, int flag);
void addTypeSpec(union symtab_entry entry, struct astnode_hdr *val);
void addTypeQual(enum qual_flag *qtype, struct astnode_lst *qual_types, struct LexVal *val, bool ptr);

void finalizeSpecs(union symtab_entry entry);
void finalizeStruct(struct astnode_hdr *structHdr, struct symtab *searchTab, bool complete);

struct astnode_ptr* allocPtr();
struct astnode_spec_inter* setPtr(struct astnode_spec_inter *ptr, struct astnode_spec_inter *prev);
struct astnode_spec_inter* allocAry(struct astnode_spec_inter *next, struct LexVal *val, union symtab_entry entry, struct symtab *symtab);
void freeInterNodes();

struct astnode_fncndec* startFuncDecl(bool params, struct LexVal *lexVal);
struct astnode_lst* startFncnArgs(struct astnode_hdr *arg);
void addFncnArg(struct astnode_lst *lst, struct astnode_hdr *arg);
struct astnode_spec_inter* setFncn(struct astnode_fncndec *fncndec, struct astnode_spec_inter *next);
struct astnode_fncndec* addFuncArgs(struct astnode_lst *args, struct symtab *symtab, bool varArgs);

void printDecl(struct symtab *symtab, union symtab_entry entry, long argNum);
void printSpec(struct astnode_spec_inter *next, struct symtab *symtab, bool func, long level, long castLevel);
void printQual(enum qual_flag qflags);
void printTabs2(bool func, long level, long castLevel);
void printArgs(struct astnode_fncndec *fncn, struct symtab *symtab, bool func, long level, long castLevel);
void printStruct(struct astnode_hdr *structHdr);

void addStmt(struct symtab *symtab, struct astnode_hdr *stmt);
struct astnode_hdr* genNoopStmt();
char* autoSprintf(long long inVal);
struct astnode_hdr* genLabel(enum label_type labelType, struct LexVal *ident, struct astnode_hdr *stmt, struct symtab *symtab);
struct astnode_hdr* genCtrl(enum ctrl_type ctrlType, struct astnode_hdr *expr, struct astnode_hdr *stmt,
                            struct astnode_hdr *stmtAlt, struct astnode_hdr *expr2, struct astnode_hdr *expr3);
struct astnode_hdr* genJump(enum jump_type jumpType, struct astnode_hdr *val);

struct symtab *currTab;
union symtab_entry currDecl;

#endif
