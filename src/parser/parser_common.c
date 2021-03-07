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

void printTabs1(int lvl){
    for(int i = 0; i < lvl; i++)
        printf("  ");
}

void printAst(struct astnode_hdr *hdr, int lvl){
    union astnode *node = (union astnode*) &hdr;
    printTabs1(lvl);

    switch (hdr->type) {
        case NODE_LEX:
            switch (node->lexNode->sym) {
                case IDENT:
                    printf("IDENT  %s\n", node->lexNode->value.string_val); break;
                case NUMBER:
                    switch (node->lexNode->tflags) {
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
                    printTabs1(lvl);

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
    lexVal->tflags = int_type;
    lexVal->value.num_val.integer_val = 1;
    lexVal->sym = NUMBER;

    return (struct astnode_hdr *) allocBinop(opand, allocBinop(opand, (struct astnode_hdr *) lexVal, opType), '=');
}

struct astnode_hdr* allocSizeof(){
    if(currDecl.generic->child->type == NODE_FNCNDEC){
        fprintf(stderr, "attempting to compute sizeof function");
        exit(-1);
    }

    if((hasFlag(currDecl.generic->type_spec->stype,void_type) && currDecl.generic->type_spec->parent->type != NODE_PTR) || (currDecl.generic->child->type == NODE_ARY && !((struct astnode_ary*)currDecl.generic->child)->complete) || checkStructValidity() > 0){
        fprintf(stderr, "invalid application of sizeof to incomplete type");
        exit(-1);
    }

    struct LexVal *lexVal = mallocSafe(sizeof(struct LexVal));
    lexVal->type = NODE_LEX;
    lexVal->file = currDecl.generic->file;
    lexVal->line = currDecl.generic->line;
    // TODO: target specific type of size_t
    lexVal->tflags = ullint_type;
    lexVal->value.num_val.integer_val = computeSizeof((struct astnode_hdr*)currDecl.generic);
    lexVal->sym = NUMBER;

    clearEntry(currDecl);

    return (struct astnode_hdr*)lexVal;
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

size_t computeSizeof(struct astnode_hdr* el){
    union astnode elUnion = (union astnode)el;

    switch (el->type) {
        case NODE_SYMTAB:
            switch (elUnion.symEntry->st_type) {
                case ENTRY_GENERIC:
                case ENTRY_VAR:
                case ENTRY_SMEM:
                case ENTRY_UMEM:
                    ;
                    //TODO: unions not properly handled and not planned
                    long multiplier = 1;
                    struct astnode_spec_inter *node = elUnion.symEntry->child;
                    while(node != NULL && node->type == NODE_ARY){
                        multiplier *= ((struct astnode_ary*)node)->length;
                        node = node->child;
                    }

                    //If first child is PTR, then don't care about anything else - size is PTR
                    if(node == NULL){
                        fprintf(stderr, "unknown error occured");
                        exit(-1);
                    }

                    // TODO: target specific size of pointers
                    if(node != NULL && node->type == NODE_PTR)
                        return multiplier*sizeof(long);

                    if(node->type != NODE_TYPESPEC){
                        fprintf(stderr, "Error: sizeof called, but typespec node isn't of type NODE_TYPESPEC\n");
                        return 0;
                    }
                    struct astnode_typespec *specNode = (struct astnode_typespec *)node;
                    if(hasFlag(specNode->stype,char_type))
                        return multiplier*sizeof(char);
                    else if(hasFlag(specNode->stype,sint_type))
                        return multiplier*sizeof(short);
                    else if(specNode->stype == int_type || specNode->stype == (int_type | signed_type) || specNode->stype == (int_type | unsigned_type))
                        return multiplier*sizeof(int);
                    else if(hasFlag(specNode->stype,lint_type) && !hasFlag(specNode->stype,double_type))
                        return multiplier*sizeof(long);
                    else if(hasFlag(specNode->stype,llint_type))
                        return multiplier*sizeof(long long);
                    else if(hasFlag(specNode->stype,float_type))
                        return multiplier*sizeof(float);
                    else if(specNode->stype == double_type)
                        return multiplier*sizeof(double);
                    else if(specNode->stype == ldouble_type)
                        return multiplier*sizeof(long double);
                    else if(hasFlag(specNode->stype,bool_type))
                        return multiplier*sizeof(_Bool);
                    else if(hasFlag(specNode->stype,fcomplex_type))
                        return multiplier*sizeof(float _Complex);
                    else if(hasFlag(specNode->stype,dcomplex_type))
                        return multiplier*sizeof(double _Complex);
                    else if(hasFlag(specNode->stype,ldcomplex_type))
                        return multiplier*sizeof(long double _Complex);
                    else if(hasFlag(specNode->stype,struct_type))
                        return multiplier*getStructSize((struct astnode_tag*)specNode->type_specs->els[0]);
                    break;
                case ENTRY_STAG:
                case ENTRY_UTAG:
                    break;
                case ENTRY_FNCN:
                    fprintf(stderr, "Error: sizeof can't be applied to function\n");
                    return 0;
            }
            break;
        default:
            fprintf(stderr, "Unknown type %d passed to sizeof - unable to compute size\n", el->type);
            return 0;
    }

    fprintf(stderr, "Error: failed to compute sizeof\n");
    return 0;
}

size_t getStructSize(struct astnode_tag *structNode){
    if(structNode->type != NODE_SYMTAB || (structNode->st_type != ENTRY_STAG && structNode->st_type != ENTRY_UTAG)){
        fprintf(stderr, "Error: getStructSize called with node not of type struct/union tag\n");
        return 0;
    }

    //TODO: not handling ENTRY_UTAG, just treating as struct
    size_t totalSize = 0;
    struct symtab *structSymtab;
    if(structNode->complete && structNode->container != NULL)
        structSymtab = structNode->container;
    else {
        //TODO: will we always be in the correct symbol table when attempting to compute sizeof?
        union symtab_entry lookupVal = symtabLookup(currTab, TAG, structNode->ident->value.string_val, false);
        if(lookupVal.generic == NULL ||
            (lookupVal.generic->st_type != ENTRY_STAG && lookupVal.generic->st_type != ENTRY_UTAG) ||
            !lookupVal.tag->complete || lookupVal.tag->container == NULL
        ){
            fprintf(stderr, "Attempting to compute sizeof struct/union %s with incomplete type\n",
                    structNode->ident->value.string_val);
            return 0;
        }
        structSymtab = lookupVal.tag->container;
    }

    union symtab_entry currEntry = structSymtab->head;
    while(currEntry.generic != NULL){
        totalSize += computeSizeof((struct astnode_hdr*)currEntry.generic);
        currEntry = currEntry.generic->next;
    }

    return totalSize;
}

size_t getTabSize(enum tab_type tabType){
    switch (tabType) {
        case TAB_GENERIC:
            return sizeof (struct symtab);
        case TAB_STRUCT:
            return sizeof (struct symtab_struct);
        case TAB_FUNC:
            return sizeof (struct symtab_func);
        default:
            fprintf(stderr, "Error: getTabSize called with unknown tabType %d\n", tabType);
            return 0;
    }
}

struct symtab* symtabCreate(enum symtab_scope scope, enum tab_type tabType){
    struct symtab *symtab = mallocSafe(getTabSize(tabType));
    symtab->tabType = tabType;
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

void exitScope(){
    currTab = currTab->parent;
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

union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name, bool singleScope){
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

    if(singleScope)
        return (union symtab_entry)(struct symtab_entry_generic*)NULL;
    return symtabLookup(symtab->parent, ns, name, false);
}

bool symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace){
    //All non-struct/union member entries go in the parent scope
    if(symtab->scope == SCOPE_STRUCT && entry.generic->ns != MEMBER)
        return symtabEnter(symtab->parent, entry, replace);

    if(entry.generic->stgclass == -1 && symtab->scope != SCOPE_PROTO)
        handleStgDefaults(entry, symtab);

    union symtab_entry existingVal = symtabLookup(symtab, entry.generic->ns, entry.generic->ident->value.string_val, true);
    if(existingVal.generic != NULL){
        if(replace){
            if(existingVal.generic->next.generic != NULL) {
                existingVal.generic->next.generic->prev = entry;
                entry.generic->next = existingVal.generic->next;
            }

            if(existingVal.generic->prev.generic != NULL) {
                existingVal.generic->prev.generic->next = entry;
                entry.generic->prev = existingVal.generic->prev;
            }
            else if(existingVal.generic->prev.generic == NULL)
                //Edge case: we are replacing the head of the linked list
                symtab->head = entry;

            goto success;
        }
        else{
            fprintf(stderr, "Warning: value already exists in symtab and replace not set, ignoring\n");
            return false;
        }
    }

    //Insert element onto beginning of list
    union symtab_entry oldHead = symtab->head;
    symtab->head = entry;
    entry.generic->prev.generic = NULL;
    entry.generic->next = oldHead;

    success:
    //Struct/union members will have printing occur later
    //Function prototype will be printed at end of prototype
    if(entry.generic->ns != TAG && entry.generic->ns != MEMBER && symtab->scope != SCOPE_PROTO)
        printDecl(symtab, entry, 0);
    return true;
}

bool structMembEnter(struct symtab *symtab, union symtab_entry entry){
    checkVoid();
    int a = checkStructValidity();
    if(a == 1){
        fprintf(stderr, "Error: struct type set, but struct not present in type specs\n");
        exit(-1);
    }
    if(a == 2){
        fprintf(stderr, "Error: attempt to declare incomplete struct not of type pointer");
        exit(-1);
    }

    struct astnode_memb *membNode = (struct astnode_memb*)allocEntry(ENTRY_SMEM, false);
    memcpy(membNode, entry.generic, getEntrySize(entry.generic->st_type));

    membNode->type=NODE_SYMTAB;
    membNode->st_type=ENTRY_SMEM;
    membNode->ns=MEMBER;

    struct astnode_tag *structNode = ((struct symtab_struct*)symtab)->parentStruct;
    membNode->bitOffset = 0;
    membNode->bitWidth = 0;
    membNode->structOffset = getStructSize(structNode);

    return symtabEnter(symtab, (union symtab_entry)membNode, false);
}

struct astnode_hdr* symCopyAndEnter(bool enter){
    // Enter later in structMembEnter
    if(currTab->scope == SCOPE_STRUCT)
        return (struct astnode_hdr*)NULL;

    checkVoid();

    if(currTab->scope == SCOPE_PROTO){
        if(currDecl.generic->child->type == NODE_ARY){
            // TODO: error check
            currDecl.generic->child = realloc(currDecl.generic->child, sizeof(struct astnode_ptr));
            currDecl.generic->child->type = NODE_PTR;
            ((struct astnode_ptr*)currDecl.generic->child)->qtype = 0;
        }
        else if(currDecl.generic->child->type == NODE_FNCNDEC)
            setPtr((struct astnode_spec_inter*)allocPtr(), currDecl.generic->child);
    }

    union symtab_entry entry;
    if(currDecl.generic->child->type == NODE_FNCNDEC){
        struct astnode_fncndec *fncndec = (struct astnode_fncndec*)currDecl.generic->child;
        struct astnode_spec_inter *child = fncndec->child;
        memcpy(fncndec, currDecl.generic, sizeof(struct symtab_entry_generic));
        fncndec->st_type = ENTRY_FNCN;
        fncndec->child = child;
        fncndec->unknown = true;
        fncndec->ns = OTHER;
        entry = (union symtab_entry)fncndec;
    }
    else{
        int a = checkStructValidity();
        if(a == 1){
            fprintf(stderr, "Error: struct type set, but struct not present in type specs\n");
            exit(-1);
        }
        if(a == 2){
            fprintf(stderr, "Error: attempt to declare incomplete struct not of type pointer");
            exit(-1);
        }

        struct astnode_var *varNode = (struct astnode_var*)allocEntry(ENTRY_VAR, false);
        memcpy(varNode, currDecl.generic, sizeof(struct symtab_entry_generic));
        varNode->st_type = ENTRY_VAR;
        varNode->ns = OTHER;
        entry = (union symtab_entry)varNode;
    }

    if(enter)
        symtabEnter(currTab, entry, false);
    
    freeInterNodes();

    return (struct astnode_hdr*) entry.generic;
}

void checkVoid(){
    if(hasFlag(currDecl.generic->type_spec->stype,void_type)){
        if(currDecl.generic->type_spec->parent->type == NODE_ARY){
            fprintf(stderr, "declaration of type name of array of voids");
            exit(-1);
        }

        if(currDecl.generic->type_spec->parent->type != NODE_PTR && currDecl.generic->type_spec->parent->type != NODE_FNCNDEC && currDecl.generic->ident != NULL){
            fprintf(stderr, "variable or field declared void");
            exit(-1);
        }
    }
}

int checkStructValidity(){
    // Check struct validity
    struct astnode_typespec *spec_node = currDecl.generic->type_spec;
    if (hasFlag(spec_node->stype, struct_type)) {
        if(spec_node->type_specs->numVals != 1 || spec_node->type_specs->els[0]->type != NODE_SYMTAB || ((struct symtab_entry_generic*)spec_node->type_specs->els[0])->st_type != ENTRY_STAG)
            return 1;

        struct astnode_tag *structNode = (struct astnode_tag*)spec_node->type_specs->els[0];
        bool structComplete = false;
        if(!structNode->complete && structNode->ident != NULL){
            struct astnode_tag *lookupNode = symtabLookup(currTab, TAG, structNode->ident->value.string_val, false).tag;
            if(lookupNode != NULL && lookupNode->complete)
                structComplete = true;
        }
        else if(structNode->complete)
            structComplete = true;

        if(!structComplete && currDecl.generic->type_spec->parent->type != NODE_PTR)
            return 2;
    }

    return 0;
}

struct astnode_hdr* genStruct(struct LexVal *type, struct symtab *symtab, union symtab_entry baseEntry, struct LexVal *ident, bool complete){
    struct astnode_tag* structNode = (struct astnode_tag*)allocEntry(ENTRY_STAG, false);
    //Copy base node into curr node - works because same header for generic
    memcpy(structNode, baseEntry.generic, sizeof(struct symtab_entry_generic));
    structNode->type = NODE_SYMTAB;
    structNode->st_type = type->sym == STRUCT ? ENTRY_STAG : ENTRY_UTAG;
    structNode->complete = complete;
    structNode->ident = ident;
    structNode->ns = TAG;
    structNode->container = complete ? symtabCreate(SCOPE_STRUCT, TAB_STRUCT) : NULL;
    structNode->file = type->file;
    structNode->line = type->line;
    if(structNode->container != NULL){
        ((struct symtab_struct*)structNode->container)->parentStruct = structNode;
    }

    if(ident != NULL) {
        struct symtab *searchTab = symtab;
        while(searchTab->scope == SCOPE_STRUCT)
            searchTab = searchTab->parent;

        union symtab_entry existingEntry = symtabLookup(searchTab, TAG, ident->value.string_val, true);

        //Only enter into symtab under two conditions:
        //1. Existing entry not present
        //2. Existing entry present, but not complete and current entry complete
        if (existingEntry.generic == NULL || (!existingEntry.tag->complete && complete))
            symtabEnter(symtab, (union symtab_entry) structNode, true);
        else if (existingEntry.generic != NULL && existingEntry.tag->complete && complete)
            fprintf(stderr, "Attempted redeclaration of struct %s failed\n", ident->value.string_val);
    }

    //Clear entry since it'll be reused for struct members, or if not present declaration is done anyway
    clearEntry(baseEntry);

    return (struct astnode_hdr*)structNode;
}

size_t getEntrySize(enum symtab_type type){
    switch (type) {
        case ENTRY_GENERIC:
            return sizeof(struct symtab_entry_generic);
        case ENTRY_VAR:
            return sizeof(struct astnode_var);
        case ENTRY_SMEM:
        case ENTRY_UMEM:
            return sizeof(struct astnode_memb);
        case ENTRY_FNCN:
            return sizeof(struct astnode_fncndec);
        case ENTRY_STAG:
        case ENTRY_UTAG:
            return sizeof(struct astnode_tag);
        default:
            fprintf(stderr, "Error: unknown type %d passed to getEntrySize\n", type);
            return 0;
    }
}

struct symtab_entry_generic* allocEntry(enum symtab_type type, bool clear){
    struct symtab_entry_generic *ret;

    ret = mallocSafe(getEntrySize(type));
    if(clear)
        clearEntry((union symtab_entry)ret);
    ret->st_type = type;
    ret->type = NODE_SYMTAB;

    return ret;
}

struct symtab_entry_generic* clearEntry(union symtab_entry entry){
    enum node_type nodeType = entry.generic->type;
    enum symtab_type type = entry.generic->st_type;
    size_t size = getEntrySize(type);
    //TODO: potential memory leak due to un-freed elements in the entry
    memset(entry.generic, 0, size);
    entry.generic->st_type = type;
    entry.generic->stgclass = -1;
    entry.generic->type = nodeType; //This should always be NODE_SYMTAB here

    struct astnode_typespec *spec_node = mallocSafe(sizeof(struct astnode_typespec));
    spec_node->type = NODE_TYPESPEC;
    spec_node->parent = (struct astnode_spec_inter*)entry.generic;
    spec_node->numParents = spec_node->stype = spec_node->qtype = 0;
    spec_node->type_specs = allocList((struct astnode_hdr*)NULL);
    spec_node->type_quals = allocList((struct astnode_hdr*)NULL);
    entry.generic->parent = (struct astnode_spec_inter*)NULL;
    entry.generic->child = (struct astnode_spec_inter*)spec_node;
    entry.generic->type_spec = spec_node;
    return entry.generic;
}

void setStgSpec(union symtab_entry entry, struct symtab *symtab, struct LexVal *val){
    if(entry.generic->stgclass == -1){
        if(symtab->scope == SCOPE_FILE && (val->sym == AUTO || val->sym == REGISTER)){
            fprintf(stderr, "no auto or register in file scope");
            exit(-1);
        }

        // TODO: ensure register is ignored if func decl
        if(symtab->scope == SCOPE_PROTO && val->sym != REGISTER){
            fprintf(stderr, "only register in parameters");
            exit(-1);
        }

        switch(val->sym){
            case TYPEDEF:
                entry.generic->stgclass = STG_TYPEDEF; break;
            case EXTERN:
                entry.generic->stgclass = STG_EXTERN; break;
            case STATIC:
                entry.generic->stgclass = STG_STATIC; break;
            case AUTO:
                entry.generic->stgclass = STG_AUTO; break;
            case REGISTER:
                entry.generic->stgclass = STG_REGISTER; break;
        }

        entry.generic->storageNode = val;
    }
    else{
        // TODO: proper error recovery
        fprintf(stderr, "cannot have multiple storage classes");
        exit(-1);
    }
}

void handleStgDefaults(union symtab_entry entry, struct symtab *symtab){
    if((symtab->scope == SCOPE_FUNC || symtab->scope == SCOPE_BLOCK) && entry.generic->st_type != ENTRY_FNCN)
        entry.generic->stgclass = STG_AUTO;
    else
        entry.generic->stgclass = STG_EXTERN;
}        

void addTypeNode(int *flags, struct astnode_lst *type_list, struct astnode_hdr *val, int flag){
    *flags |= flag;
    addToList(type_list, val);
}

void addTypeSpec(union symtab_entry entry, struct astnode_hdr *val){
    struct astnode_typespec *spec_node = entry.generic->type_spec;

    int flag = spec_node->stype;
    if(val->type == NODE_LEX) {
        entry.generic->file = ((struct LexVal*)val)->file;
        entry.generic->line = ((struct LexVal*)val)->line;
        switch (((struct LexVal*)val)->sym) {
            case VOID:
                /* TODO: checks on void */
                if (flag != 0) {
                    fprintf(stderr, "invalid type specifier: VOID");
                    exit(-1);
                }
                addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, void_type);
                break;
            case CHAR:
                if (flag == 0 || flag == signed_type || flag == unsigned_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, char_type);
                else {
                    fprintf(stderr, "invalid type specifier: CHAR");
                    exit(-1);
                }
                break;
            case SHORT:
                if (flag == 0 || flag == signed_type || flag == unsigned_type || flag == int_type ||
                    flag == (int_type | signed_type) || flag == uint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, sint_type);
                else {
                    fprintf(stderr, "invalid type specifier: SHORT");
                    exit(-1);
                }
                break;
            case INT:
                if (flag == 0 || flag == signed_type || flag == unsigned_type || flag == sint_type ||
                    flag == (sint_type | signed_type) || flag == usint_type || flag == lint_type ||
                    flag == (lint_type | signed_type) || flag == ulint_type || flag == llint_type ||
                    flag == (llint_type | signed_type) || flag == ullint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, int_type);
                else {
                    fprintf(stderr, "invalid type specifier: INT");
                    exit(-1);
                }
                break;
            case LONG:
                if (flag == 0 || flag == signed_type || flag == unsigned_type || flag == int_type ||
                    flag == (int_type | signed_type) || flag == uint_type || flag == double_type ||
                    flag == complex_type || flag == dcomplex_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, lint_type);
                else if (flag == lint_type || flag == (lint_type | signed_type) || flag == ulint_type ||
                         flag == (int_type | lint_type) || flag == (int_type | lint_type | signed_type) ||
                         flag == (int_type | ulint_type)) {
                    spec_node->stype &= ~lint_type;
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, llint_type);
                } else {
                    fprintf(stderr, "invalid type specifier: LONG");
                    exit(-1);
                }
                break;
            case FLOAT:
                if (flag == 0 || flag == complex_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, float_type);
                else {
                    fprintf(stderr, "invalid type specifier: FLOAT");
                    exit(-1);
                }
                break;
            case DOUBLE:
                if (flag == 0 || flag == lint_type || flag == complex_type || flag == (complex_type | lint_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, double_type);
                else {
                    fprintf(stderr, "invalid type specifier: DOUBLE");
                    exit(-1);
                }
                break;
            case SIGNED:
                if (flag == 0 || flag == char_type || flag == sint_type || flag == int_type || flag == lint_type ||
                    flag == llint_type || flag == (sint_type | int_type) || flag == (lint_type | int_type) ||
                    flag == (llint_type | int_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, signed_type);
                else {
                    fprintf(stderr, "invalid type specifier: SIGNED");
                    exit(-1);
                }
                break;
            case UNSIGNED:
                if (flag == 0 || flag == char_type || flag == sint_type || flag == int_type || flag == lint_type ||
                    flag == llint_type || flag == (sint_type | int_type) || flag == (lint_type | int_type) ||
                    flag == (llint_type | int_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, unsigned_type);
                else {
                    fprintf(stderr, "invalid type specifier: UNSIGNED");
                    exit(-1);
                }
                break;
            case _BOOL:
                if (flag != 0) {
                    fprintf(stderr, "invalid type specifier: _BOOL");
                    exit(-1);
                }
                addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, bool_type);
                break;
            case _COMPLEX:
                if (flag == 0 || flag == float_type || hasFlag(flag, double_type) || flag == lint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, complex_type);
                else {
                    fprintf(stderr, "invalid type specifier: _COMPLEX");
                    exit(-1);
                }
        }
    }
    else if(val->type == NODE_SYMTAB && ((struct symtab_entry_generic*)val)->st_type == ENTRY_STAG){
        entry.generic->file = ((struct symtab_entry_generic*)val)->file;
        entry.generic->line = ((struct symtab_entry_generic*)val)->line;
        //Variable is of type struct, need to store pointer to struct in curr declaration for use with variable type
        if(flag != 0){
            fprintf(stderr, "invalid type specifier");
            exit(-1);
        }
        addTypeNode((int*)&spec_node->stype, spec_node->type_specs, val, struct_type);
    }
}

void addTypeQual(enum qual_flag *qtype, struct astnode_lst *qual_types, struct LexVal *val, bool ptr){
    switch(val->sym){
        case CONST:
            if(!hasFlag(*qtype,QUAL_CONST))
                addTypeNode((int*)qtype, qual_types, (struct astnode_hdr *) val, QUAL_CONST);
            break;
        case RESTRICT:
            if(!ptr){
                fprintf(stderr, "restrict can only qualify pointers");
                exit(-1);
            }

            if(!hasFlag(*qtype,QUAL_RESTRICT))
                addTypeNode((int*)qtype, qual_types, (struct astnode_hdr *) val, QUAL_RESTRICT);
            break;
        case VOLATILE:
            if(!hasFlag(*qtype,QUAL_VOLATILE))
                addTypeNode((int*)qtype, qual_types, (struct astnode_hdr *) val, QUAL_VOLATILE);
            break;
    }
}

void finalizeSpecs(union symtab_entry entry){
    struct astnode_typespec *spec_node = entry.generic->type_spec;
    // TODO: Warning?
    if(spec_node->stype == 0 || spec_node->stype == signed_type || spec_node->stype == unsigned_type)
        spec_node->stype |= int_type;

    // TODO: _Complex alone not in spec but worked on my computer?
    if(spec_node->stype == complex_type || spec_node->stype == (complex_type | lint_type)){
        fprintf(stderr, "complex must have a further real specifier");
        exit(-1);
    }
}

struct astnode_fncndec* startFuncDef(bool params){
    struct astnode_fncndec *fncndec = (struct astnode_fncndec *) allocEntry(ENTRY_FNCN, false);
    
    fncndec->type = NODE_FNCNDEC;
    fncndec->unknown = true;
    fncndec->none = false;
    if(params){
        fncndec->scope = (struct symtab_func*)symtabCreate(SCOPE_PROTO, TAB_FUNC);
        fncndec->scope->parentFunc = fncndec;
        currDecl.generic = allocEntry(ENTRY_GENERIC, true);
    }

    return fncndec;
}

struct astnode_lst* startFncnArgs(struct astnode_hdr *arg){
    struct symtab_entry_generic *entry = (struct symtab_entry_generic*)arg;
    if(entry->child == (struct astnode_spec_inter*)entry->type_spec && hasFlag(entry->type_spec->stype,void_type))
        ((struct symtab_func*)currTab)->parentFunc->none = true;

    clearEntry(currDecl);

    return allocList(arg);
}

void addFncnArg(struct astnode_lst *lst, struct astnode_hdr *arg){
    struct symtab_entry_generic *entry = (struct symtab_entry_generic*)arg;
    if(entry->child == (struct astnode_spec_inter*)entry->type_spec && hasFlag(entry->type_spec->stype,void_type)){
        fprintf(stderr, "void must be the only parameter");
        exit(-1);
    }

    clearEntry(currDecl);
    addToList(lst, arg);
}

struct astnode_spec_inter* setFncn(struct astnode_fncndec *fncndec, struct astnode_spec_inter *prev){
    if(prev->type == NODE_ARY){
        fprintf(stderr, "cannot declare an array of functions");
        exit(-1);
    }
    else if(prev->type == NODE_FNCNDEC){
        fprintf(stderr, "function return type cannot be function");
        exit(-1);
    }
    
    fncndec->child = prev->child;
    prev->child->parent = (struct astnode_spec_inter*)fncndec;
    fncndec->parent = prev;
    prev->child = (struct astnode_spec_inter*)fncndec;
    
    return (struct astnode_spec_inter*)fncndec;
}


struct astnode_fncndec* addFuncArgs(struct astnode_lst *args, struct symtab *symtab, bool varArgs){
    if (symtab->tabType != TAB_FUNC){
        fprintf(stderr, "Error: attempting to add function arguments to non-function symtab\n");
        return NULL;
    }

    struct symtab_func *funcTab = (struct symtab_func*)symtab;
    struct astnode_fncndec *parentFunc = funcTab->parentFunc;

    parentFunc->args = args;
    parentFunc->varArgs = varArgs;

    return parentFunc;
}

struct astnode_ptr* allocPtr(){
    struct astnode_ptr *ptr = mallocSafe(sizeof(struct astnode_ptr));
    ptr->type = NODE_PTR;
    ptr->qtype = 0;
    ptr->type_quals = allocList((struct astnode_hdr*)NULL);

    return ptr;
}

struct astnode_spec_inter* setPtr(struct astnode_spec_inter *ptr, struct astnode_spec_inter *next){
    ptr->parent = next->parent;
    next->parent->child = ptr;
    ptr->child = next;
    next->parent = ptr;

    return ptr;
}

struct astnode_spec_inter* allocAry(struct astnode_spec_inter *prev, struct LexVal *val, union symtab_entry entry, struct symtab *symtab){
    if(prev->type == NODE_FNCNDEC){
        fprintf(stderr, "function cannot return array");
        exit(-1);
    }

    if(val == NULL && symtab->scope != SCOPE_PROTO && entry.generic->stgclass != STG_EXTERN && (entry.generic->stgclass != -1 || symtab->scope != SCOPE_FILE)){
        fprintf(stderr, "array size not specified");
        exit(-1);
    }

    if(val != NULL){
        if(val->tflags >= float_type){
            fprintf(stderr, "array size must be an integral type");
            exit(-1);
        }

        if(val->value.num_val.integer_val == 0){
            fprintf(stderr, "array size must be positive");
            exit(-1);
        }
    }

    if(prev->child->type == NODE_ARY && !((struct astnode_ary*)prev->child)->complete || prev->type == NODE_ARY && val == NULL){
        fprintf(stderr, "only first dimension can be empty");
        exit(-1);
    }

    struct astnode_ary *ary = mallocSafe(sizeof(struct astnode_ary));
    ary->type = NODE_ARY;
    if(val == NULL)
        ary->complete = false;
    else{
        ary->complete = true;
        ary->length = val->value.num_val.integer_val;
    }

    ary->child = prev->child;
    prev->child->parent = (struct astnode_spec_inter*)ary;
    ary->parent = prev;
    prev->child = (struct astnode_spec_inter*)ary;

    return (struct astnode_spec_inter*)ary;
}

void freeInterNodes(){
    currDecl.generic->child = (struct astnode_spec_inter*)currDecl.generic->type_spec;
    currDecl.generic->type_spec->numParents++;
    currDecl.generic->type_spec->parents = realloc(currDecl.generic->type_spec->parents, sizeof(struct astnode_spec_inter*) * currDecl.generic->type_spec->numParents);
    currDecl.generic->type_spec->parents[currDecl.generic->type_spec->numParents-1] = currDecl.generic->type_spec->parent;
    currDecl.generic->type_spec->parent = (struct astnode_spec_inter*)currDecl.generic;
}

void printQual(enum qual_flag qflags){
    if(hasFlag(qflags,QUAL_CONST))
        printf("const ");
    if(hasFlag(qflags,QUAL_RESTRICT))
        printf("restrict ");
    if(hasFlag(qflags,QUAL_VOLATILE))
        printf("volatile ");
}

void printDecl(struct symtab *symtab, union symtab_entry entry, long argNum){
    char *storage, *usage;
    switch(entry.generic->stgclass){
        case STG_TYPEDEF:
            storage = "typedef"; break;
        case STG_EXTERN:
            storage = "extern"; break;
        case STG_STATIC:
            storage = "static"; break;
        case STG_AUTO:
            storage = "auto"; break;
        case STG_REGISTER:
            storage = "register"; break;
    }

    char *scope;
    switch (symtab->scope) {
        case SCOPE_FILE:
            scope = "global";
            break;
        case SCOPE_BLOCK:
            scope = "block";
            break;
        case SCOPE_FUNC:
            scope = "function";
            break;
        case SCOPE_PROTO:
            scope = "prototype";
            break;
        case SCOPE_STRUCT:
            scope = "struct/union";
            break;
    }

    bool isTag = false, isMemb = false, isFunc = false;
    switch (entry.generic->ns) {
        case OTHER:
            if(entry.generic->st_type == ENTRY_FNCN)
                isFunc = true;
            else
                usage = "variable";
            break;
        case TAG:
            isTag = true;
            break;
        case LABEL:
            usage = "label";
            break;
        case MEMBER:
            isMemb = true;
            break;
    }

    if(isTag){
        //Don't print incomplete forward definitions
        char *structName;
        char *fileName = entry.generic->file;
        int line = entry.generic->line;
        if(entry.tag->ident == NULL)
            structName = "(anonymous)";
        else
            structName = entry.tag->ident->value.string_val;

        if(entry.tag->complete){
            printf("struct %s definition at %s:%d{\n",
                   structName, fileName, line);
        }
        /*else{
            printf("struct %s definition at %s:%d (incomplete)\n",
                   structName, fileName, line);
        }*/
        return;
    }

    printf("%s is defined at %s:%d [in %s scope] as a \n", entry.generic->ident->value.string_val,
           entry.generic->file, entry.generic->line, scope);
    if(isMemb) {
        char *structName;
        if (symtab->tabType != TAB_STRUCT) {
            fprintf(stderr, "Error: attempting to print member info with tab not of type TAB_STRUCT\n");
            structName = "";
        }
        else if (((struct symtab_struct*)symtab)->parentStruct->ident != NULL)
            structName = ((struct symtab_struct*)symtab)->parentStruct->ident->value.string_val;
        else
            structName = "(anonymous)";

        printf("field of struct %s  off=%zu bit_off=%d bit_wid=%d, type:\n  ",
               structName, entry.memb->structOffset, entry.memb->bitOffset, entry.memb->bitWidth
        );
    }
    else if(isFunc)
        printf("%s   function returning:\n  ", storage);
    else{
        printf("%s ", usage);
        if(argNum > 0 && symtab->scope == SCOPE_PROTO)
            printf("(argument #%d) ", argNum);

        if(entry.generic->stgclass != -1)
            printf("with stgclass %s  ", storage);

        printf("of type\n  ");
    }

    struct astnode_spec_inter *child = entry.generic->child;
    struct astnode_typespec *spec_node = entry.generic->type_spec;
    if(child != NULL && spec_node != NULL)
        printSpec(child, spec_node, symtab, isFunc, 0);

    if(isFunc)
        printArgs(entry.fncn, symtab, true, 0);

    if(symtab->scope != SCOPE_STRUCT && symtab->scope != SCOPE_PROTO)
        printf("\n");
}

void printSpec(struct astnode_spec_inter *next, struct astnode_typespec *spec_node, struct symtab *symtab, bool func, long level){
    /* TODO: iterate over pointers, arrays, functions */
    printTabs2(func, level);
    if(next->type != NODE_TYPESPEC){    
        if(next->type == NODE_ARY){
            printf("array of  ");
            if(((struct astnode_ary*)next)->complete)
                printf("%d", ((struct astnode_ary*)next)->length);
            else
                printf("incomplete");

            printf(" elements of type\n  ");
            printSpec(next->child, spec_node, symtab, func, level+1);
            return;
        }
        else if(next->type == NODE_PTR){
            printQual(((struct astnode_ptr*)next)->qtype);
            printf("pointer to \n  ");
            printSpec(next->child, spec_node, symtab, func, level+1);
            return;
        }
        else if(next->type == NODE_FNCNDEC){
            printf("function returning\n  ");
            printSpec(next->child, spec_node, symtab, func, level+1);
            printTabs2(func, level);
            printArgs((struct astnode_fncndec*)next, symtab, func, level+1);
            return;
        }
    }

    printQual(spec_node->qtype);

    enum type_flag sflags = spec_node->stype;
    if(hasFlag(sflags,void_type))
        printf("void");

    if(hasFlag(sflags,signed_type))
        printf("signed ");
    if(hasFlag(sflags,unsigned_type))
        printf("unsigned ");

    if(hasFlag(sflags,char_type))
        printf("char");
    if(hasFlag(sflags,sint_type))
        printf("short");
    if(hasFlag(sflags,lint_type))
        printf("long");
    if(hasFlag(sflags,llint_type))
        printf("long long");
    if(hasFlag(sflags,int_type)){
        if((sflags & ~uint_type & ~signed_type) != 0)
            printf(" ");
        printf("int");
    }

    if(hasFlag(sflags,float_type))
        printf("float");
    if(hasFlag(sflags,double_type)){
        if(hasFlag(sflags,lint_type))
            printf(" ");
        printf("double");
    }

    if (hasFlag(sflags, bool_type))
        printf("_Bool");
    if (hasFlag(sflags, complex_type))
        printf(" _Complex");
    if (hasFlag(sflags, struct_type)){
        char *structName, *structFile;
        int line = 0;
        struct astnode_tag *tagNode = ((struct astnode_tag*)spec_node->type_specs->els[0]);
        if(tagNode->ident == NULL)
            structName = "(anonymous)";
        else
            structName = tagNode->ident->value.string_val;

        printf("struct %s ", structName);

        struct astnode_tag *structDef = NULL;
        if(tagNode->ident == NULL) {
            if(tagNode->complete)
                structDef = ((struct symtab_struct*)tagNode->container)->parentStruct;
            else
                fprintf(stderr, "Error: anonymous struct incomplete");
        }
        else
            structDef = symtabLookup(symtab, TAG, structName, false).tag;

        if(structDef == NULL || !structDef->complete)
            printf("(incomplete)");
        else
            printf("(defined at %s:%d)", structDef->file, structDef->line);
        //TODO: lookup struct in curr table and print incomplete/location defined
    }

    printf("\n");
}

void printTabs2(bool func, long level){
    if(func)
        printf(" ");

    for(int i = 0; i < level; i++)
        printf(" ");
}

void printArgs(struct astnode_fncndec *fncn, struct symtab *symtab, bool func, long level){
    if(fncn->none){
        printf("  and taking no arguments\n");
    }
    else if(fncn->args == NULL || fncn->args->numVals == 0){
        printf("  and taking unknown arguments\n");
    }
    else {
        printf("  and taking the following arguments\n");
        for(int i = 0; i < fncn->args->numVals; i++){
            //TODO: will this cast be safe?
            struct symtab_entry_generic *arg = (struct symtab_entry_generic *)fncn->args->els[i];
            if(arg->child != NULL && arg->type_spec != NULL) {
                printf("  ");
                printSpec(arg->child, arg->type_spec, symtab, func, level);
            }
        }

        for(int i = 0; i < fncn->args->numVals; i++){
            struct symtab_entry_generic *arg = (struct symtab_entry_generic*)fncn->args->els[i];
            if(arg->ident != NULL && arg->child != NULL && arg->type_spec != NULL){
                printf("\n");
                printDecl((struct symtab*)fncn->scope, (union symtab_entry)arg, i+1);
            }
        }
    }
}

void printStruct(struct astnode_hdr *structHdr){
    if(structHdr->type != NODE_SYMTAB ||
        (((struct symtab_entry_generic*)structHdr)->st_type != ENTRY_STAG
                && ((struct symtab_entry_generic*)structHdr)->st_type != ENTRY_UTAG)){
        fprintf(stderr, "Error: printStructEnd called with non-struct node\n");
        return;
    }

    struct astnode_tag *structNode = (struct astnode_tag*)structHdr;

    struct symtab *parentTab = structNode->container->parent;
    while(parentTab != NULL && parentTab->scope == SCOPE_STRUCT)
        parentTab = parentTab->parent;

    //Print struct start info
    printDecl(parentTab, (union symtab_entry)structNode, 0);

    union symtab_entry currEntry = structNode->container->head;
    while(currEntry.generic != NULL){
        printDecl(structNode->container, currEntry, 0);

        currEntry = currEntry.generic->next;
    }

    printf("} (size==%zu)\n\n", getStructSize(structNode));
}

void finalizeStruct(struct astnode_hdr *structHdr){
    struct astnode_tag *structNode = (struct astnode_tag*)structHdr;
    currDecl.generic->stgclass = structNode->stgclass;
    currDecl.generic->storageNode = structNode->storageNode;
    currDecl.generic->type_spec->qtype = structNode->type_spec->qtype;
    currDecl.generic->type_spec->type_quals = structNode->type_spec->type_quals;
}
