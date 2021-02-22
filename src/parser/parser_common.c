#include "parser_common.h"
#include <stdio.h>
#include <stdlib.h>

extern int currLine;
extern char currFile[];

void *mallocSafe(size_t size){
    void *ret = malloc(size);
    if(ret == NULL){
        yyerror("parser failed to malloc");
        exit(-1);
    }
    return ret;
}

void yyerror(char const* s){
    fprintf(stderr, "%s:%d: Error: %s\n", currFile, currLine, s);
}

void printTabs(int lvl){
    for(int i = 0; i < lvl; i++)
        printf("  ");
}

void printAst(struct astnode_hdr *hdr, int lvl){
    union astnode *node = (union astnode*) &hdr;
    printTabs(lvl);

    switch (hdr->type) {
        case NODE_LEX:
            switch (node->lexNode->sym) {
                case IDENT:
                    printf("IDENT  %s\n", node->lexNode->value.string_val); break;
                case NUMBER:
                    switch (node->lexNode->flags) {
                        case int_type: 
                            printf("CONSTANT:  (type=int)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case uint_type: 
                            printf("CONSTANT:  (type=unsigned,int)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case lint_type: 
                            printf("CONSTANT:  (type=long)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case ulint_type: 
                            printf("CONSTANT:  (type=unsigned,long)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case llint_type: 
                            printf("CONSTANT:  (type=longlong)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case ullint_type: 
                            printf("CONSTANT:  (type=unsigned,longlong)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case float_type: 
                            printf("CONSTANT:  (type=float)%g\n", node->lexNode->value.num_val.float_val); break;
                        case double_type: 
                            printf("CONSTANT:  (type=double)%g\n", node->lexNode->value.num_val.double_val); break;
                        case ldouble_type: 
                            printf("CONSTANT:  (type=longdouble)%Lg\n", node->lexNode->value.num_val.ldouble_val); break;
                    }
                    break;
                case CHARLIT:
                    printf("CONSTANT:  (type=int)%d\n", node->lexNode->value.string_val[0]); break;
                case STRING:
                    printf("STRING  %S\n", (wchar_t *) node->lexNode); break;
            }
            break;
        case NODE_UNOP:
            switch (node->unNode->op) {
                case PLUSPLUS:
                    printf("UNARY  OP  POSTINC\n"); break;
                case MINUSMINUS:
                    printf("UNARY  OP  POSTDEC\n"); break;
                case '&':
                    printf("ADDRESSOF\n"); break;
                case '*':
                    printf("DEREF\n"); break;
                case SIZEOF:
                    printf("SIZEOF\n"); break;
                default:
                    printf("UNARY  OP  %c\n", node->unNode->op);
            }
            printAst((struct astnode_hdr *) node->unNode->opand, lvl + 1);
            break;
        case NODE_BINOP:
            switch (node->binNode->op) {
                case '.':
                    printf("SELECT\n"); break;
                case SHL:
                    printf("BINARY  OP  <<\n"); break;
                case SHR:
                    printf("BINARY  OP  >>\n"); break;
                case '<':
                    printf("COMPARISON  OP  <\n"); break;
                case '>':
                    printf("COMPARISON  OP  >\n"); break;
                case LTEQ:
                    printf("COMPARISON  OP  <=\n"); break;
                case GTEQ:
                    printf("COMPARISON  OP  >=\n"); break;
                case EQEQ:
                    printf("COMPARISON  OP  ==\n"); break;
                case NOTEQ:
                    printf("COMPARISON  OP  !=\n"); break;
                case LOGAND:
                    printf("LOGICAL  OP  &&\n"); break;
                case LOGOR:
                    printf("LOGICAL  OP  ||\n"); break;
                case '=':
                    printf("ASSIGNMENT\n"); break;
                default:
                    printf("BINARY  OP  %c\n", node->binNode->op);
            }
            printAst(node->binNode->left, lvl + 1);
            printAst(node->binNode->right, lvl + 1);
            break;
        case NODE_TEROP:
            printf("TERNARY  OP,  IF:\n");
            printAst(node->terNode->first, lvl + 1);
            printf("THEN:\n");
            printAst(node->terNode->second, lvl + 1);
            printf("ELSE:\n");
            printAst(node->terNode->third, lvl + 1);
            break;
        case NODE_LST:
            for(int i = 0; i < node->lst->numVals; i++){
                if(i > 0)
                    printTabs(lvl);

                printf("arg  #%d=\n", i + 1);
                printAst(node->lst->els[i], lvl + 1);
            }
            break;
        case NODE_FNCN:
            printf("FNCALL,  %d  arguments\n", node->fncn->lst->numVals);
            printAst(node->fncn->name, lvl + 1);
            printAst((struct astnode_hdr *) node->fncn->lst, lvl);
            break;
    }

    if(lvl == 0 && hdr->type != NODE_LST)
        printf("\n");
}

struct astnode_hdr* allocUnop(struct astnode_hdr *opand, int opType){
    struct astnode_unop *retNode = mallocSafe(sizeof(struct astnode_unop));
    retNode->type = NODE_UNOP;
    retNode->opand = opand;
    retNode->op = opType;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType){
    struct astnode_binop *retNode = mallocSafe(sizeof(struct astnode_binop));
    retNode->type = NODE_BINOP;
    retNode->left = left;
    retNode->right = right;
    retNode->op = opType;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocTerop(struct astnode_hdr *first, struct astnode_hdr *second, struct astnode_hdr *third){
    struct astnode_terop *retNode = mallocSafe(sizeof(struct astnode_terop));
    retNode->type = NODE_TEROP;
    retNode->first = first;
    retNode->second = second;
    retNode->third = third;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocPostIncDec(struct LexVal *op, struct astnode_hdr *opand, int opType){
    struct LexVal *lexVal = mallocSafe(sizeof(struct LexVal));
    lexVal->type = NODE_LEX;
    lexVal->file = op->file;
    lexVal->line = op->line;
    lexVal->flags = int_type;
    lexVal->value.num_val.integer_val = 1;
    lexVal->sym = NUMBER;

    return (struct astnode_hdr *) allocBinop(opand, allocBinop(opand, (struct astnode_hdr *) lexVal, opType), '=');
}

struct astnode_hdr* allocAssignment(struct astnode_hdr *left, struct astnode_hdr *right, struct LexVal *opType){
    if(opType->sym != '=') {
        switch (opType->sym) {
            case TIMESEQ:
                right = allocBinop(left, right, '*'); break; 
            case DIVEQ:
                right = allocBinop(left, right, '/'); break;
            case MODEQ:
                right = allocBinop(left, right, '%'); break;
            case PLUSEQ:
                right = allocBinop(left, right, '+'); break;
            case MINUSEQ:
                right = allocBinop(left, right, '-'); break;
            case SHLEQ:
                right = allocBinop(left, right, SHL); break;
            case SHREQ:
                right = allocBinop(left, right, SHR); break;
            case ANDEQ:
                right = allocBinop(left, right, '&'); break;
            case XOREQ:
                right = allocBinop(left, right, '^'); break;
            case OREQ:
                right = allocBinop(left, right, '|'); break;
        }
    }

    return allocBinop(left, right, '=');
}

struct astnode_lst* allocList(struct astnode_hdr *el){
    struct astnode_lst *lst = mallocSafe(sizeof(struct astnode_lst));
    lst->type = NODE_LST;
    if(el == NULL) {
        lst->numVals = 0;
        lst->els = NULL;
    }
    else{
        lst->numVals = 1;
        lst->els = mallocSafe(sizeof(union astnode));
        lst->els[0] = el;
    }
    return lst;
}


void addToList(struct astnode_lst *lst, struct astnode_hdr *el){
    lst->numVals++;
    lst->els = realloc(lst->els, sizeof(struct astnode_hdr*)*lst->numVals);
    lst->els[lst->numVals-1] = el;
}

struct astnode_hdr* allocFunc(struct astnode_hdr *name, struct astnode_lst *lst){
    struct astnode_fncn *fncn = mallocSafe(sizeof(struct astnode_fncn));

    fncn->type = NODE_FNCN;
    fncn->lst = lst;
    fncn->name = name;

    return (struct astnode_hdr *) fncn;
}

struct symtab* symtabCreate(enum symtab_scope scope){
    struct symtab *symtab = mallocSafe(sizeof (struct symtab));
    symtab->scope = scope;
    symtab->parent = currTab;
    symtab->head.generic = NULL;
    symtab->numChildren = 0;

    if(currTab != NULL){
        currTab->numChildren++;
        //TODO: assuming this won't fail
        currTab->children = realloc(currTab->children, sizeof(struct symtab*) * currTab->numChildren);
        currTab->children[currTab->numChildren - 1] = symtab;
    }
    currTab = symtab;

    return symtab;
}

void symtabDestroy(struct symtab *symtab){
    if(symtab->numChildren > 0){
        fprintf(stderr, "Warning: symtab being destroyed has children which will also be destroyed");
        for(int i = 0; i < symtab->numChildren; i++){
            symtabDestroy(symtab->children[i]);
        }
        free(symtab->children);
    }

    struct symtab *nextTab = symtab->parent;

    union symtab_entry currEntry = symtab->head;
    union symtab_entry nextEntry;
    while(currEntry.generic != NULL){
        nextEntry = currEntry.generic->next;
        //TODO: free elements within an entry - ex: AST Node, ...
        free(currEntry.generic);
        currEntry = nextEntry;
    }

    free(symtab);

    currTab = nextTab;
}

union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name){
    if(symtab == NULL)
        return (union symtab_entry)(struct symtab_entry_generic*)NULL;

    union symtab_entry currEntry = symtab->head;
    union symtab_entry nextEntry;
    while(currEntry.generic != NULL){
        nextEntry = currEntry.generic->next;
        if( currEntry.generic->ns == ns &&
                strcmp(currEntry.generic->ident->value.string_val, name) == 0){
            return currEntry;
        }
        currEntry = nextEntry;
    }

    return symtabLookup(symtab->parent, ns, name);
}

bool symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace){
    union symtab_entry existingVal = symtabLookup(symtab, entry.generic->ns, entry.generic->ident->value.string_val);
    if(existingVal.generic != NULL){
        if(replace){
            if(existingVal.generic->next.generic != NULL)
                existingVal.generic->next.generic->prev = entry;

            if(existingVal.generic->prev.generic != NULL)
                existingVal.generic->prev.generic->next = entry;
            return true;
        }
        else{
            fprintf(stderr, "Warning: value already exists in symtab and replace not set, ignoring");
            return false;
        }
    }

    //Insert element onto beginning of list
    union symtab_entry oldHead = symtab->head;
    symtab->head = entry;
    entry.generic->prev.generic = NULL;
    entry.generic->next = oldHead;
}

size_t getEntrySize(enum symtab_type type){
    switch (type) {
        case ENTRY_GENERIC:
            return sizeof(struct symtab_entry_generic);
    }
}

struct symtab_entry_generic* allocEntry(enum symtab_type type){
    struct symtab_entry_generic *ret;

    ret = mallocSafe(getEntrySize(type));
    ret->type = type;
    clearEntry(ret);

    return ret;
}

struct symtab_entry_generic* clearEntry(union symtab_entry entry){
    enum symtab_type type = entry.generic->type;
    size_t size = getEntrySize(type);
    //TODO: potential memory leak due to un-freed elements in the entry
    memset(entry, 0, size);
    entry.generic->type = type;
}