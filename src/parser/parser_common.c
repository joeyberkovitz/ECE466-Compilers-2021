#include "parser_common.h"
#include "../quads/quad_common.h"
#include <stdio.h>
#include <stdlib.h>

extern int currLine;
extern char currFile[];
extern int doGenQuad;

void *mallocSafeParse(size_t size){
    void *ret = malloc(size);
    if(ret == NULL){
        yyerror("parser failed to malloc");
        exit(EXIT_FAILURE);
    }
    return ret;
}

void yyerror(char const* s){
    fprintf(stderr, "%s:%d: Error: %s\n", currFile, currLine, s);
}

struct astnode_hdr* exprAssocVar(struct astnode_hdr *opand, enum symtab_ns ns, struct symtab *tab){
    struct LexVal *lexNode;
    char *name;
    if(opand->type != NODE_LEX) return opand;
    lexNode = (struct LexVal*)opand;
    if(lexNode->sym != IDENT) return opand;

    if(lexNode->value.string_val == NULL){
        fprintf(stderr, "%s:%d: Error: received LexVal of type IDENT without identifier\n", currFile, currLine);
        exit(EXIT_FAILURE);
    }
    name = lexNode->value.string_val;

    union symtab_entry entry = symtabLookup(tab, ns, name, false, -1);
    if(entry.generic == NULL){
        fprintf(stderr, "%s:%d: Error: failed to find IDENT '%s' in symtab\n", currFile, currLine, name);
        exit(EXIT_FAILURE);
    }

    return (struct astnode_hdr*)entry.generic;
}

struct astnode_hdr* allocUnop(struct astnode_hdr *opand, int opType){
    if (opType == '&' && opand->type == NODE_SYMTAB && ((struct symtab_entry_generic*)opand)->stgclass == STG_REGISTER){
        fprintf(stderr, "%s:%d: Error: address of register variable '%s' requested\n", currFile, currLine, ((struct symtab_entry_generic*)opand)->ident->value.string_val);
        exit(EXIT_FAILURE);
    }

    struct astnode_unop *retNode = mallocSafeParse(sizeof(struct astnode_unop));
    retNode->type = NODE_UNOP;
    retNode->opand = opand;
    retNode->op = opType;
    retNode->file = currFile;
    retNode->line = currLine;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocBinop(struct astnode_hdr *left, struct astnode_hdr *right, int opType){
    struct astnode_binop *retNode = mallocSafeParse(sizeof(struct astnode_binop));
    retNode->type = NODE_BINOP;
    retNode->left = left;
    retNode->right = right;
    retNode->op = opType;
    retNode->file = currFile;
    retNode->line = currLine;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocTerop(struct astnode_hdr *first, struct astnode_hdr *second, struct astnode_hdr *third){
    struct astnode_terop *retNode = mallocSafeParse(sizeof(struct astnode_terop));
    retNode->type = NODE_TEROP;
    retNode->first = first;
    retNode->second = second;
    retNode->third = third;
    retNode->file = currFile;
    retNode->line = currLine;
    return (struct astnode_hdr *) retNode;
}

struct astnode_hdr* allocConst(int num){
    struct LexVal *lexVal = mallocSafeParse(sizeof(struct LexVal));
    lexVal->type = NODE_LEX;

    lexVal->file = currFile;
    lexVal->line = currLine;
    lexVal->tflags = int_type;
    lexVal->value.num_val.integer_val = num;
    lexVal->sym = NUMBER;

    return (struct astnode_hdr*)lexVal;
}

struct astnode_hdr* allocSizeof(){
    if(currDecl.generic->child->type == NODE_FNCNDEC){
        fprintf(stderr, "%s:%d: Error: invalid application of 'sizeof' to function\n", currFile, currLine);
        exit(EXIT_FAILURE);
    }

    if((hasFlag(currDecl.generic->type_spec->stype,void_type) && currDecl.generic->type_spec->parent->type != NODE_PTR) || (currDecl.generic->child->type == NODE_ARY && !((struct astnode_ary*)currDecl.generic->child)->complete) || checkStructValidity() > 0){
        fprintf(stderr, "%s:%d: Error: invalid application of 'sizeof' to incomplete type\n", currFile, currLine);
        exit(EXIT_FAILURE);
    }

    struct LexVal *lexVal = mallocSafeParse(sizeof(struct LexVal));
    lexVal->type = NODE_LEX;
    lexVal->file = currDecl.generic->file;
    lexVal->line = currDecl.generic->line;
    lexVal->tflags = int_type; // to allow greater variety of expressions, may really be other type
    lexVal->value.num_val.integer_val = computeSizeof((struct astnode_hdr*)currDecl.generic, false);
    lexVal->sym = NUMBER;

    clearEntry(currDecl);

    return (struct astnode_hdr*)lexVal;
}

struct astnode_cast* allocCast(){
    if(currDecl.generic->child->type == NODE_FNCNDEC || currDecl.generic->child->type == NODE_ARY || (currDecl.generic->child == (struct astnode_spec_inter*)currDecl.generic->type_spec && hasFlag(currDecl.generic->type_spec->stype,struct_type))){
        fprintf(stderr, "%s:%d: Error: conversion to non-scalar type requested\n", currFile, currLine);
        exit(EXIT_FAILURE);
    }

    struct astnode_cast *cast = mallocSafeParse(sizeof(struct astnode_cast));
    cast->type = NODE_CAST;
    cast->cast_spec = currDecl.generic->child;
    cast->file = currFile;
    cast->line = currLine;

    clearEntry(currDecl);

    return cast;
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
    struct astnode_lst *lst = mallocSafeParse(sizeof(struct astnode_lst));
    lst->type = NODE_LST;
    if(el == NULL) {
        lst->numVals = 0;
        lst->els = NULL;
    }
    else{
        lst->numVals = 1;
        lst->els = mallocSafeParse(sizeof(union astnode));
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
    struct astnode_fncn *fncn = mallocSafeParse(sizeof(struct astnode_fncn));

    fncn->type = NODE_FNCN;
    fncn->lst = lst;
    fncn->name = name;
    fncn->file = currFile;
    fncn->line = currLine;

    return (struct astnode_hdr *) fncn;
}

size_t computeSizeof(struct astnode_hdr* el, bool expr){
    if (expr)
        goto inter;

    union astnode elUnion = (union astnode)el;

    switch (el->type) {
        case NODE_SYMTAB:
            switch (elUnion.symEntry->st_type) {
                case ENTRY_GENERIC:
                case ENTRY_VAR:
                case ENTRY_SMEM:
                case ENTRY_UMEM:
                inter:
                    ;
                    long multiplier = 1;
                    struct astnode_spec_inter *node;
                    if (expr)
                        node = (struct astnode_spec_inter*)el;
                    else
                        node = elUnion.symEntry->child;

                    if (node->type == NODE_FNCNDEC){
                        fprintf(stderr, "%s:%d: Error: invalid application of 'sizeof' to function\n", currFile, currLine);
                        exit(EXIT_FAILURE);
                    }

                    while(node != NULL && node->type == NODE_ARY){
                        if(!((struct astnode_ary*)node)->complete){
                            if (expr){
                                fprintf(stderr, "%s:%d: Error: invalid application of 'sizeof' to incomplete type\n", currFile, currLine);
                                exit(EXIT_FAILURE);
                            }
                            else
                                return 0;
                        }

                        multiplier *= ((struct astnode_ary*)node)->length;
                        node = node->child;
                    }

                    if(node == NULL){
                        fprintf(stderr, "%s:%d: Error: unknown error occured\n", currFile, currLine);
                        exit(EXIT_FAILURE);
                    }

                    //If first child is PTR, then don't care about anything else - size is PTR
                    if(node != NULL && node->type == NODE_PTR)
                        return multiplier*sizeof(int *);

                    if(node->type != NODE_TYPESPEC){
                        fprintf(stderr, "%s:%d: Error: sizeof called, but typespec node isn't of type NODE_TYPESPEC\n", currFile, currLine);
                        return 0;
                    }
                    struct astnode_typespec *specNode = (struct astnode_typespec *)node;
                    if(hasFlag(specNode->stype,void_type)) {
                        fprintf(stderr, "%s:%d: Error: invalid application of 'sizeof' to incomplete type\n", currFile,
                                currLine);
                        exit(EXIT_FAILURE);
                    }
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
                        return multiplier*getStructSize((struct astnode_tag*)specNode->type_specs->els[0], false);
                    break;
                case ENTRY_STAG:
                case ENTRY_UTAG:
                    break;
                case ENTRY_FNCN:
                    fprintf(stderr, "%s:%d: Error: sizeof can't be applied to function\n", currFile, currLine);
                    return 0;
            }
            break;
        default:
            fprintf(stderr, "%s:%d: Error: unknown type %d passed to sizeof - unable to compute size\n", currFile, currLine, el->type);
            return 0;
    }

    fprintf(stderr, "%s:%d: Error: failed to compute sizeof\n", currFile, currLine);
    return 0;
}

size_t getStructSize(struct astnode_tag *structNode, bool ignoreIncomplete){
    // Does not take alignment issues into account since structs/unions are not implemented past parsing
    if(structNode->type != NODE_SYMTAB || (structNode->st_type != ENTRY_STAG && structNode->st_type != ENTRY_UTAG)){
        fprintf(stderr, "%s:%d: Error: getStructSize called with node not of type struct/union tag\n", currFile, currLine);
        return 0;
    }

    size_t totalSize = 0;
    struct symtab *structSymtab;
    if(structNode->container != NULL)
        structSymtab = structNode->container;
    else {
        union symtab_entry lookupVal = symtabLookup(currTab, TAG, structNode->ident->value.string_val, false, -1);
        if(lookupVal.generic == NULL ||
            (lookupVal.generic->st_type != ENTRY_STAG && lookupVal.generic->st_type != ENTRY_UTAG) ||
            (!lookupVal.tag->complete && !ignoreIncomplete) || lookupVal.tag->container == NULL
        ){
            fprintf(stderr, "%s:%d: Error: attempting to compute sizeof struct/union %s with incomplete type\n",
                    structNode->ident->file, structNode->ident->line, structNode->ident->value.string_val);
            return 0;
        }
        structSymtab = lookupVal.tag->container;
    }

    union symtab_entry currEntry = structSymtab->head;
    while(currEntry.generic != NULL){
        if(structNode->st_type == ENTRY_STAG)
            totalSize += computeSizeof((struct astnode_hdr*)currEntry.generic, false);
        else {
            size_t entrySize = computeSizeof((struct astnode_hdr*)currEntry.generic, false);
            if(entrySize > totalSize)
                totalSize = entrySize;
        }
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
            fprintf(stderr, "%s:%d: Error: getTabSize called with unknown tabType %d\n", currFile, currLine, tabType);
            return 0;
    }
}

struct symtab* symtabCreate(enum symtab_scope scope, enum tab_type tabType, struct LexVal *startLex){
    struct symtab *symtab = mallocSafeParse(getTabSize(tabType));
    symtab->tabType = tabType;
    symtab->scope = scope;
    symtab->parent = currTab;
    symtab->head.generic = NULL;
    symtab->numChildren = 0;
    symtab->swtch = NULL;
    char *startFile;
    int startLine;
    if(startLex->type == NODE_LEX){
        startFile = startLex->file;
        startLine = startLex->line;
    }
    else if(startLex->type == NODE_SYMTAB){
        startFile = ((struct symtab_entry_generic*)startLex)->file;
        startLine = ((struct symtab_entry_generic*)startLex)->line;
    }
    else{
        startFile = "n/a";
        startLine = 0;
        fprintf(stderr, "%s:%d: Error: received unknown type %d in symtabCreate, unable to extract debug information\n", startLex->file, startLex->line, startLex->type);
    }

    symtab->file = startFile;
    symtab->line = startLine;

    symtab->forDecl = false;
    symtab->stmtList = allocList(NULL);

    if(currTab != NULL){
        currTab->numChildren++;
        currTab->children = realloc(currTab->children, sizeof(struct symtab*) * currTab->numChildren);
        if(currTab->children == NULL){
            fprintf(stderr, "%s:%d: Error: unknown error occurred\n", startLex->file, startLex->line);
            exit(EXIT_FAILURE);
        }

        currTab->children[currTab->numChildren - 1] = symtab;
    }
    currTab = symtab;

    return symtab;
}

void exitScope(){
    if(currTab->scope == SCOPE_FUNC) {
        struct symtab_func *funcTab = ((struct symtab_func *) currTab);
        funcTab->parentFunc->defined = true;

        printFunc(funcTab->parentFunc);
    }
    
    currTab = currTab->parent;
}

void enterBlockScope(struct LexVal *lexVal){
    if(currTab->scope == SCOPE_FUNC && ((struct symtab_func*)currTab)->parentFunc->defined){
        struct astnode_fncndec *fncNode = ((struct symtab_func*)currTab)->parentFunc;
        fprintf(stderr, "%s:%d: Error: redefinition of '%s'\n", lexVal->file, lexVal->line, fncNode->ident->value.string_val);
        exit(EXIT_FAILURE);
    }
    else if(currTab->scope == SCOPE_PROTO){
        struct astnode_fncndec *fncNode = ((struct symtab_func*)currTab)->parentFunc;
        // Edge case described in symtabEnter
        if(fncNode->unknownCheck){
            fprintf(stderr, "%s:%d: Error: number of arguments does not match prototype\n", lexVal->file, lexVal->line);
            exit(EXIT_FAILURE);
        }

        if(fncNode->noIdent){
            fprintf(stderr, "%s:%d: parameter name omitted\n", lexVal->file, lexVal->line);
            exit(EXIT_FAILURE);
        }

        currTab->scope = SCOPE_FUNC;
        currTab->file = lexVal->file;
        currTab->line = lexVal->line;
    }
    else
        symtabCreate(SCOPE_BLOCK, TAB_GENERIC, lexVal);
}

void enterFuncScope(struct astnode_hdr *func){
    struct astnode_fncndec *fncNode = (struct astnode_fncndec*)func;
    currTab = (struct symtab*)(fncNode)->scope;
}

union symtab_entry symtabLookup(struct symtab *symtab, enum symtab_ns ns, char *name, bool singleScope, enum linkage_type linkage){
    if(symtab == NULL)
        return (union symtab_entry)(struct symtab_entry_generic*)NULL;

    union symtab_entry currEntry = symtab->head;
    union symtab_entry nextEntry;
    while(currEntry.generic != NULL){
        nextEntry = currEntry.generic->next;
        if( currEntry.generic->ns == ns &&
                strcmp(currEntry.generic->ident->value.string_val, name) == 0 &&
                (linkage == -1 || currEntry.generic->linkage == linkage)){
            return currEntry;
        }
        currEntry = nextEntry;
    }

    //If scope is struct, then only members are in single scope, all others are in parent
    //If NS is LABEL, entries are only in parent function/switch scope
    if(singleScope && (symtab->scope != SCOPE_STRUCT || ns == MEMBER) &&
        (symtab->scope == SCOPE_FUNC || ns != LABEL))
        return (union symtab_entry)(struct symtab_entry_generic*)NULL;
    return symtabLookup(symtab->parent, ns, name, false, linkage);
}

struct symtab_entry_generic* symtabEnter(struct symtab *symtab, union symtab_entry entry, bool replace){
    //All non-struct/union member entries go in the parent scope
    if(symtab->scope == SCOPE_STRUCT && entry.generic->ns != MEMBER)
        return symtabEnter(symtab->parent, entry, replace);

    //Labels are global to the function/switch
    if(entry.generic->ns == LABEL && symtab->scope != SCOPE_FUNC)
        return symtabEnter(symtab->parent, entry, replace);

    if(entry.generic->stgclass == -1 && symtab->scope != SCOPE_PROTO)
        handleStgDefaults(entry, symtab);

    if(entry.generic->st_type == ENTRY_FNCN && (symtab->scope == SCOPE_FUNC || symtab->scope == SCOPE_BLOCK) && entry.generic->stgclass != STG_EXTERN){
        fprintf(stderr, "%s:%d: Error: invalid storage class for function '%s'\n", entry.generic->file, entry.generic->line, entry.generic->ident->value.string_val);
        exit(EXIT_FAILURE);
    }

    if((entry.generic->st_type != ENTRY_VAR && entry.generic->st_type != ENTRY_FNCN) || symtab->scope == SCOPE_PROTO ||
        ((symtab->scope == SCOPE_BLOCK || symtab->scope == SCOPE_FUNC)
            && entry.generic->stgclass != STG_EXTERN))
        entry.generic->linkage = LINK_NONE;
    else if(entry.generic->stgclass == STG_STATIC && symtab->scope == SCOPE_FILE)
        entry.generic->linkage = LINK_INT;
    else if(entry.generic->stgclass == STG_EXTERN){
        union symtab_entry existingTemp = symtabLookup(symtab, entry.generic->ns, entry.generic->ident->value.string_val, false, -1);
        if(existingTemp.generic != NULL && existingTemp.generic->linkage != LINK_NONE)
            entry.generic->linkage = existingTemp.generic->linkage;
        else
            entry.generic->linkage = LINK_EXT;
    }
    else
        entry.generic->linkage = LINK_EXT;

    if(entry.generic->linkage == LINK_EXT || entry.generic->linkage == LINK_INT){
        union symtab_entry existingTemp = symtabLookup(symtab, entry.generic->ns, entry.generic->ident->value.string_val, false, entry.generic->linkage);
        if(existingTemp.generic != NULL && !checkCompatibility((struct astnode_spec_inter*)existingTemp.generic, (struct astnode_spec_inter*)entry.generic, true, true)){
            fprintf(stderr, "%s:%d: Error: conflicting types for '%s'\n", entry.generic->file, entry.generic->line, entry.generic->ident->value.string_val);
            exit(EXIT_FAILURE);
        }
    }        

    union symtab_entry existingVal = symtabLookup(symtab, entry.generic->ns, entry.generic->ident->value.string_val, true, -1);
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
            if(existingVal.generic->linkage == LINK_NONE){
                fprintf(stderr, "%s:%d: Error: redeclaration of '%s' with no linkage\n", entry.generic->file, entry.generic->line, entry.generic->ident->value.string_val);
                exit(EXIT_FAILURE);
            }

            // Set check for empty defn params but non-empty prior proto params
            if(existingVal.generic->st_type == ENTRY_FNCN && entry.generic->st_type == ENTRY_FNCN){
                if(!existingVal.fncn->unknown && entry.fncn->unknown)
                    existingVal.fncn->unknownCheck = true;
                else
                    existingVal.fncn->unknownCheck = false;
            }
        }
    }

    if(existingVal.generic != NULL && existingVal.generic->linkage != entry.generic->linkage){
        fprintf(stderr, "%s:%d: Error: attempted redeclaration of '%s' with conflicting linkage\n", entry.generic->file, entry.generic->line, entry.generic->ident->value.string_val);
        exit(EXIT_FAILURE);
    }

    if(existingVal.generic == NULL){
        //Insert element onto beginning of list
        union symtab_entry oldHead = symtab->head;
        symtab->head = entry;
        entry.generic->prev.generic = NULL;
        entry.generic->next = oldHead;
        if(oldHead.generic != NULL)
            oldHead.generic->prev = entry;
    }

    success:
    if(existingVal.generic == NULL || replace) {
        entry.generic->parentTab = symtab;
        entry.generic->inProto = symtab->scope == SCOPE_PROTO;
    }
    //Struct/union members will have printing occur later
    //Function prototype will be printed at end of prototype
    if(entry.generic->ns != TAG && entry.generic->ns != MEMBER && symtab->scope != SCOPE_PROTO)
        printDecl(symtab, entry, 0);

    if(existingVal.generic != NULL && !replace)
        return existingVal.generic;
    else
        return entry.generic;
}

struct symtab_entry_generic* structMembEnter(struct symtab *symtab, union symtab_entry entry){
    struct astnode_tag *structNode = ((struct symtab_struct*)symtab)->parentStruct;
    if(structNode->incAry){
        fprintf(stderr, "%s:%d: Error: flexible array member not at end of struct\n", entry.generic->file, entry.generic->line);
        exit(EXIT_FAILURE);
    }

    if(entry.generic->child->type == NODE_FNCNDEC){
        fprintf(stderr, "%s:%d: Error: field '%s' declared as a function\n", entry.generic->file, entry.generic->line, entry.generic->ident->value.string_val);
        exit(EXIT_FAILURE);
    }

    checkVoid();
    int a = checkStructValidity();
    if(a == 1){
        fprintf(stderr, "%s:%d: Error: struct type set, but struct not present in type specs\n", entry.generic->file, entry.generic->line);
        exit(EXIT_FAILURE);
    }
    if(a == 2){
        fprintf(stderr, "%s:%d: Error: attempt to declare incomplete struct not of type pointer\n", entry.generic->file, entry.generic->line);
        exit(EXIT_FAILURE);
    }
    if(a == 3){
        if(structNode->st_type == ENTRY_UTAG)
            structNode->incAry = true;
        else{
            fprintf(stderr, "%s:%d: Error: attempt to declare struct with member struct with flexible array member\n", entry.generic->file, entry.generic->line);
            exit(EXIT_FAILURE);
        }
    }

    if(entry.generic->child->type == NODE_ARY && !((struct astnode_ary*)entry.generic->child)->complete){
        if(structNode->st_type == ENTRY_UTAG){
            fprintf(stderr, "%s:%d: Error: flexible array member in union\n", entry.generic->file, entry.generic->line);
            exit(EXIT_FAILURE);
        }

        structNode->incAry = true;
    }
    else
        structNode->namedEntry = true;

    //ENTRY_SMEM and ENTRY_UMEM are same struct, so no problem here
    struct astnode_memb *membNode = (struct astnode_memb*)allocEntry(ENTRY_SMEM, false);
    memcpy(membNode, entry.generic, getEntrySize(entry.generic->st_type));

    membNode->type=NODE_SYMTAB;
    membNode->st_type= structNode->st_type == ENTRY_STAG ? ENTRY_SMEM : ENTRY_UMEM;
    membNode->ns=MEMBER;

    membNode->bitOffset = 0;
    membNode->bitWidth = 0;
    membNode->structOffset = structNode->st_type == ENTRY_STAG ? getStructSize(structNode, true) : 0;

    struct symtab_entry_generic *ret = symtabEnter(symtab, (union symtab_entry)membNode, false);

    freeInterNodes();

    return ret;
}

struct astnode_hdr* symCopyAndEnter(bool enter){
    // Enter later in structMembEnter
    if(currTab->scope == SCOPE_STRUCT)
        return (struct astnode_hdr*)NULL;

    checkVoid();

    if(currTab->scope == SCOPE_PROTO){
        if(currDecl.generic->child->type == NODE_ARY){
            currDecl.generic->child = realloc(currDecl.generic->child, sizeof(struct astnode_ptr));
            if(currDecl.generic->child == NULL){
                fprintf(stderr, "%s:%d: Error: unknown error occurred\n", currDecl.generic->file, currDecl.generic->line);
                exit(EXIT_FAILURE);
            }

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
        fncndec->defined = false;
        fncndec->unknownCheck = false;
        fncndec->ns = OTHER;
        entry = (union symtab_entry)fncndec;
    }
    else{
        int a = checkStructValidity();
        if(a == 1){
            fprintf(stderr, "%s:%d: Error: struct type set, but struct not present in type specs\n", currDecl.generic->file, currDecl.generic->line);
            exit(EXIT_FAILURE);
        }
        else if(a == 2){
            fprintf(stderr, "%s:%d: Error: attempt to declare incomplete struct not of type pointer\n", currDecl.generic->file, currDecl.generic->line);
            exit(EXIT_FAILURE);
        }
        else if(a == 3 && currDecl.generic->type_spec->parent->type == NODE_ARY){
            fprintf(stderr, "%s:%d: Error: attempt to declare array of structs with flexible array member\n", currDecl.generic->file, currDecl.generic->line);
            exit(EXIT_FAILURE);
        }

        struct astnode_var *varNode = (struct astnode_var*)allocEntry(ENTRY_VAR, false);
        memcpy(varNode, currDecl.generic, sizeof(struct symtab_entry_generic));
        varNode->st_type = ENTRY_VAR;
        varNode->ns = OTHER;
        entry = (union symtab_entry)varNode;
    }

    if(enter)
        entry.generic = symtabEnter(currTab, entry, false);
    
    freeInterNodes();

    return (struct astnode_hdr*) entry.generic;
}

void checkVoid(){
    if(hasFlag(currDecl.generic->type_spec->stype,void_type)){
        if(currDecl.generic->type_spec->parent->type == NODE_ARY){
            fprintf(stderr, "%s:%d: Error: declaration of '%s' of array of voids\n", currDecl.generic->file, currDecl.generic->line, currDecl.generic->ident->value.string_val);
            exit(EXIT_FAILURE);
        }

        if(currDecl.generic->type_spec->parent->type != NODE_PTR && currDecl.generic->type_spec->parent->type != NODE_FNCNDEC && currDecl.generic->ident != NULL){
            fprintf(stderr, "%s:%d: Error: variable or field '%s' declared void\n", currDecl.generic->file, currDecl.generic->line, currDecl.generic->ident->value.string_val);
            exit(EXIT_FAILURE);
        }
    }
}

int checkStructValidity(){
    struct astnode_typespec *spec_node = currDecl.generic->type_spec;
    if (hasFlag(spec_node->stype, struct_type)) {
        if(spec_node->type_specs->numVals != 1 || spec_node->type_specs->els[0]->type != NODE_SYMTAB ||
                ( ((struct symtab_entry_generic*)spec_node->type_specs->els[0])->st_type != ENTRY_STAG &&
                    ((struct symtab_entry_generic*)spec_node->type_specs->els[0])->st_type != ENTRY_UTAG )
        )
            return 1;

        struct astnode_tag *structNode = (struct astnode_tag*)spec_node->type_specs->els[0];
        bool structComplete = false;
        if(!structNode->complete && structNode->ident != NULL){
            struct astnode_tag *lookupNode = symtabLookup(currTab, TAG, structNode->ident->value.string_val, false, -1).tag;
            if(lookupNode != NULL && lookupNode->complete)
                structComplete = true;
        }
        else if(structNode->complete)
            structComplete = true;
        
        if(!structComplete && currDecl.generic->type_spec->parent->type != NODE_PTR)
            return 2;
        else if(structComplete && structNode->incAry)
            return 3;
    }

    return 0;
}

bool checkCompatibility(struct astnode_spec_inter *entry1, struct astnode_spec_inter *entry2, bool qual, bool comp){
    if(entry1->type != entry2->type)
        return false;

    switch(entry1->type){
        case NODE_SYMTAB:
            if(((struct symtab_entry_generic*)entry1)->st_type != ((struct symtab_entry_generic*)entry2)->st_type)
                return false;

            if(((struct symtab_entry_generic*)entry1)->st_type == ENTRY_VAR)
                return checkCompatibility(entry1->child, entry2->child, qual, comp);
            else if(((struct symtab_entry_generic*)entry1)->st_type == ENTRY_FNCN)
                return checkCompatibilityFncn((struct astnode_fncndec*)entry1, (struct astnode_fncndec*)entry2, comp);
        case NODE_PTR:
            if(((struct astnode_ptr*)entry1)->qtype != ((struct astnode_ptr*)entry2)->qtype && qual)
                return false;

            return checkCompatibility(entry1->child, entry2->child, true, comp);
        case NODE_ARY:
            if(((struct astnode_ary*)entry1)->complete && ((struct astnode_ary*)entry2)->complete && ((struct astnode_ary*)entry1)->length != ((struct astnode_ary*)entry2)->length)
                return false;
            else if(((struct astnode_ary*)entry2)->complete && !((struct astnode_ary*)entry1)->complete){
                if (comp) {
                    ((struct astnode_ary *) entry1)->complete = true;
                    ((struct astnode_ary *) entry1)->length = ((struct astnode_ary *) entry2)->length;
                }
                else
                    return false;
            }
            else if(((struct astnode_ary*)entry1)->complete && !((struct astnode_ary*)entry2)->complete){
                if (comp) {
                    ((struct astnode_ary *) entry2)->complete = true;
                    ((struct astnode_ary *) entry2)->length = ((struct astnode_ary *) entry1)->length;
                }
                else
                    return false;
            }

            return checkCompatibility(entry1->child, entry2->child, true, comp);
        case NODE_FNCNDEC:
            return checkCompatibilityFncn((struct astnode_fncndec*)entry1, (struct astnode_fncndec*)entry2, comp);
        case NODE_TYPESPEC: ;
            struct astnode_typespec *spec_node1 = (struct astnode_typespec*)entry1, *spec_node2 = (struct astnode_typespec*)entry2;
            if(spec_node1->stype == void_type && spec_node2->stype != void_type)
                return false;
            if(hasFlag(spec_node1->stype,char_type) && spec_node1->stype != spec_node2->stype)
                return false;
            if(hasFlag(spec_node1->stype,sint_type)){
                if(!hasFlag(spec_node2->stype,sint_type))
                    return false;
                else if((spec_node1->stype & unsigned_type) != (spec_node2->stype & unsigned_type))
                    return false;
            }
            if((spec_node1->stype == int_type || spec_node1->stype == (int_type | signed_type)) && !(spec_node2->stype == int_type || spec_node2->stype == (int_type | signed_type)))
                return false;
            if(spec_node1->stype == (int_type | unsigned_type) && spec_node1->stype != spec_node2->stype)
                return false;
            if((spec_node1->stype == lint_type || spec_node1->stype == (lint_type | signed_type) || spec_node1->stype == (lint_type | int_type) || spec_node1->stype == (lint_type | int_type | signed_type)) && !(spec_node2->stype == lint_type || spec_node2->stype == (lint_type | signed_type) || spec_node2->stype == (lint_type | int_type) || spec_node2->stype == (lint_type | int_type | signed_type)))
                return false;
            if((spec_node1->stype == (lint_type | unsigned_type) || spec_node1->stype == (lint_type | int_type | unsigned_type)) && !(spec_node2->stype == (lint_type | unsigned_type) || spec_node2->stype == (lint_type | int_type | unsigned_type)))
                return false;
            if(hasFlag(spec_node1->stype,llint_type)){
                if(!hasFlag(spec_node2->stype,llint_type))
                    return false;
                else if((spec_node1->stype & unsigned_type) != (spec_node2->stype & unsigned_type))
                    return false;
            }
            if(spec_node1->stype == float_type && spec_node2->stype != float_type)
                return false;
            if(spec_node1->stype == double_type && spec_node2->stype != double_type)
                return false;
            if(spec_node1->stype == ldouble_type && spec_node2->stype != ldouble_type)
                return false;
            if(spec_node1->stype == bool_type && spec_node2->stype != bool_type)
                return false;
            if(spec_node1->stype == fcomplex_type && spec_node2->stype != fcomplex_type)
                return false;
            if(spec_node1->stype == dcomplex_type && spec_node2->stype != dcomplex_type)
                return false;
            if(spec_node1->stype == ldcomplex_type && spec_node2->stype != ldcomplex_type)
                return false;
            if(spec_node1->stype == struct_type){
                if(spec_node2->stype != struct_type)
                    return false;

                struct astnode_tag *tagNode1 = ((struct astnode_tag*)spec_node1->type_specs->els[0]);
                struct astnode_tag *tagNode2 = ((struct astnode_tag*)spec_node2->type_specs->els[0]);
                if(tagNode1->ident == NULL || strcmp(tagNode1->ident->value.string_val, tagNode2->ident->value.string_val))
                    return false;
                
                struct astnode_tag *structDef1 = symtabLookup(currTab, TAG, tagNode1->ident->value.string_val, false, -1).tag;
                struct astnode_tag *structDef2 = symtabLookup(currTab, TAG, tagNode2->ident->value.string_val, false, -1).tag;

                if(structDef1 != structDef2)
                    return false;
            }
            
            if(spec_node1->qtype != spec_node2->qtype && qual)
                return false;

            return true;
    }
}

bool checkCompatibilityFncn(struct astnode_fncndec *entry1, struct astnode_fncndec *entry2, bool comp){
    if(entry1->defined && entry1->unknown && !entry2->unknown && !entry2->none){
        fprintf(stderr, "%s:%d: Error: number of arguments does not match prototype\n", entry2->file, entry2->line);
        exit(EXIT_FAILURE);
    }

    if(!entry1->unknown && !entry2->unknown) {
        if (entry1->args->numVals != entry2->args->numVals || entry1->varArgs != entry2->varArgs)
            return false;
        else {
            for (int i = 0; i < entry1->args->numVals; i++) {
                if (!checkCompatibility((struct astnode_spec_inter *) entry2->args->els[i],
                                        (struct astnode_spec_inter *) entry1->args->els[i], false, comp))
                    return false;

                // keep resaving the param names if func not defined yet, decl w/o def param names don't matter
                if (!entry1->defined) {
                    if (comp)
                        entry1->args->els[i] = entry2->args->els[i];
                    else
                        return false;
                }
            }
        }
    }
    else if((entry1->unknown || entry2->unknown) && !(entry1->unknown && entry2->unknown)) {
        struct astnode_fncndec *funcWithArgs = entry1->unknown ? entry2 : entry1;
        for (int i = 0; i < funcWithArgs->args->numVals; i++) {
            if (((struct astnode_spec_inter *) funcWithArgs->args->els[i])->child->type == NODE_TYPESPEC) {
                enum type_flag type = ((struct astnode_typespec *) ((struct astnode_spec_inter *) funcWithArgs->args->els[i])->child)->stype;
                if (hasFlag(type, char_type) || hasFlag(type, sint_type) ||
                    hasFlag(type, float_type) && !hasFlag(type, complex_type))
                    return false;
            }

            // keep resaving the param names if func not defined yet, decl w/o def param names don't matter
            if (!entry1->defined && entry1->unknown) {
                if (comp)
                    entry1->args->els[i] = entry2->args->els[i];
                else
                    return false;
            }
        }
    }

    // keep resaving the scope if func not defined yet, decl w/o def scope only has params
    if(!entry1->defined){
        if (comp) {
            entry1->scope = entry2->scope;
            entry1->noIdent = entry2->noIdent;
        }
        else
            return false;
    }

    return checkCompatibility(entry1->child, entry2->child, true, comp);
}

struct astnode_hdr* genStruct(struct LexVal *type, struct symtab *symtab, union symtab_entry baseEntry, struct LexVal *ident, struct LexVal *scopeStart, bool complete){
    struct astnode_tag* structNode = (struct astnode_tag*)allocEntry(ENTRY_STAG, false);
    //Copy base node into curr node - works because same header for generic
    memcpy(structNode, baseEntry.generic, sizeof(struct symtab_entry_generic));
    structNode->type = NODE_SYMTAB;
    structNode->st_type = type->sym == STRUCT ? ENTRY_STAG : ENTRY_UTAG;
    structNode->complete = false; //Not complete until closing "}"
    structNode->ident = ident;
    structNode->ns = TAG;
    structNode->incAry = false;
    structNode->namedEntry = false;
    structNode->container = complete ? symtabCreate(SCOPE_STRUCT, TAB_STRUCT, scopeStart) : NULL;
    structNode->file = type->file;
    structNode->line = type->line;
    if(structNode->container != NULL)
        ((struct symtab_struct*)structNode->container)->parentStruct = structNode;

    if(ident != NULL) {
        struct symtab *searchTab = symtab;
        while(searchTab->scope == SCOPE_STRUCT)
            searchTab = searchTab->parent;

        union symtab_entry existingEntry = symtabLookup(searchTab, TAG, ident->value.string_val, true, -1);

        //Only enter into symtab if complete. If incomplete, will consider entering at end of declaration
        if ((existingEntry.generic == NULL && complete) || (existingEntry.generic != NULL && !existingEntry.tag->complete && complete))
            symtabEnter(symtab, (union symtab_entry) structNode, true);
        else if (existingEntry.generic != NULL && existingEntry.tag->complete && complete) {
            fprintf(stderr, "%s:%d: attempted redeclaration of struct %s failed\n", ident->file, ident->line, ident->value.string_val);
            exit(EXIT_FAILURE);
        }
        else if(existingEntry.generic != NULL){
            structNode->incAry = existingEntry.tag->incAry;
            structNode->namedEntry = existingEntry.tag->namedEntry;
        }
    }

    //Clear entry since it'll be reused for struct members, or if not present declaration is done anyway
    clearEntry(baseEntry);

    return (struct astnode_hdr*)structNode;
}

void finalizeStruct(struct astnode_hdr *structHdr, struct symtab *searchTab, bool complete){
    struct astnode_tag *structNode = (struct astnode_tag*)structHdr;
    if(complete && structNode->ident != NULL) {
        union symtab_entry existingEntry = symtabLookup(searchTab, TAG, structNode->ident->value.string_val, true, -1);
        if(existingEntry.generic != NULL &&
            (existingEntry.generic->st_type == ENTRY_STAG || existingEntry.generic->st_type == ENTRY_UTAG)
            && existingEntry.tag->complete){
            fprintf(stderr, "%s:%d: Error: attempting to set struct/union as complete with existing complete value present\n",
                    structNode->file, structNode->line);
            exit(EXIT_FAILURE);
        }
    }

    structNode->complete = complete;

    if(structNode->complete && !structNode->namedEntry){
        fprintf(stderr, "%s:%d: Error: flexible array member in struct with no named members\n", structNode->file, structNode->line);
        exit(EXIT_FAILURE);
    }

    currDecl.generic->stgclass = structNode->stgclass;
    currDecl.generic->storageNode = structNode->storageNode;
    currDecl.generic->type_spec->qtype = structNode->type_spec->qtype;
    currDecl.generic->type_spec->type_quals = structNode->type_spec->type_quals;
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
        case ENTRY_LABEL:
            return sizeof(struct astnode_label);
        default:
            fprintf(stderr, "%s:%d: Error: unknown type %d passed to getEntrySize\n", currFile, currLine, type);
            return 0;
    }
}

struct symtab_entry_generic* allocEntry(enum symtab_type type, bool clear){
    struct symtab_entry_generic *ret;

    ret = mallocSafeParse(getEntrySize(type));
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
    memset(entry.generic, 0, size);
    entry.generic->st_type = type;
    entry.generic->stgclass = -1;
    entry.generic->type = nodeType; //This should always be NODE_SYMTAB here

    struct astnode_typespec *spec_node = mallocSafeParse(sizeof(struct astnode_typespec));
    spec_node->type = NODE_TYPESPEC;
    spec_node->parent = (struct astnode_spec_inter*)entry.generic;
    spec_node->child = (struct astnode_spec_inter*)NULL;
    spec_node->numParents = spec_node->stype = spec_node->qtype = 0;
    spec_node->type_specs = allocList((struct astnode_hdr*)NULL);
    spec_node->type_quals = allocList((struct astnode_hdr*)NULL);
    entry.generic->parent = (struct astnode_spec_inter*)NULL;
    entry.generic->child = (struct astnode_spec_inter*)spec_node;
    entry.generic->type_spec = spec_node;
    return entry.generic;
}

void checkDeclDoesStuff(union symtab_entry decl){
    if(decl.generic->type == NODE_SYMTAB){
        struct astnode_tag *tagNode = NULL;
        for(int i = 0; i < decl.generic->type_spec->type_specs->numVals; i++){
            struct astnode_hdr *currNode = decl.generic->type_spec->type_specs->els[i];
            if(currNode->type == NODE_SYMTAB &&
                    (((struct symtab_entry_generic*)currNode)->st_type == ENTRY_STAG ||
                            ((struct symtab_entry_generic*)currNode)->st_type == ENTRY_UTAG)) {
                tagNode = (struct astnode_tag *) currNode;
                break;
            }
        }

        if(tagNode != NULL && tagNode->ident != NULL){
            union symtab_entry existingNode = symtabLookup(currTab, TAG, tagNode->ident->value.string_val, true, -1);
            if(existingNode.generic == NULL)
                symtabEnter(currTab, (union symtab_entry)tagNode, false);
            return;
        }
    }

    fprintf(stderr, "%s:%d: Error: declaration is useless\n", decl.generic->file, decl.generic->line);
}

void setStgSpec(union symtab_entry entry, struct symtab *symtab, struct LexVal *val){
    if(entry.generic->stgclass == -1){
        if(symtab->scope == SCOPE_FILE && (val->sym == AUTO || val->sym == REGISTER)){
            fprintf(stderr, "%s:%d: Error: file-scope declaration specifies '%s'\n", val->file, val->line, val->sym == AUTO ? "auto" : "register");
            exit(EXIT_FAILURE);
        }

        // note that register is eventually ignored if func decl
        if(symtab->scope == SCOPE_PROTO && val->sym != REGISTER){
            fprintf(stderr, "%s:%d: Error: only register allowed in parameters\n", val->file, val->line);
            exit(EXIT_FAILURE);
        }

        if(symtab->forDecl && !(val->sym == AUTO || val->sym == REGISTER)){
            fprintf(stderr, "%s:%d: Error: declaration of '%s' variable in 'for' loop initial declaration", val->file, val->line, val->sym == EXTERN ? "extern" : val->sym == STATIC ? "static" : "typedef");
            exit(EXIT_FAILURE);
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
        fprintf(stderr, "%s:%d: Error: multiple storage classes in declaration specifiers\n", val->file, val->line);
        exit(EXIT_FAILURE);
    }
}

void handleStgDefaults(union symtab_entry entry, struct symtab *symtab){
    if((symtab->scope == SCOPE_FUNC || symtab->scope == SCOPE_BLOCK) && entry.generic->st_type != ENTRY_FNCN)
        entry.generic->stgclass = STG_AUTO;
    else if(entry.generic->st_type == ENTRY_FNCN){
        entry.generic->stgclass = STG_EXTERN;
        if(symtab->forDecl){
            fprintf(stderr, "%s:%d: Error: declaration of 'extern' variable in 'for' loop initial declaration", entry.generic->file, entry.generic->line);
            exit(EXIT_FAILURE);
        }
    }
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
                if (flag != 0) {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: VOID\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, void_type);
                break;
            case CHAR:
                if (flag == 0 || flag == signed_type || flag == unsigned_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, char_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: CHAR\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case SHORT:
                if (flag == 0 || flag == signed_type || flag == unsigned_type || flag == int_type ||
                    flag == (int_type | signed_type) || flag == uint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, sint_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: SHORT\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case INT:
                if (flag == 0 || flag == signed_type || flag == unsigned_type || flag == sint_type ||
                    flag == (sint_type | signed_type) || flag == usint_type || flag == lint_type ||
                    flag == (lint_type | signed_type) || flag == ulint_type || flag == llint_type ||
                    flag == (llint_type | signed_type) || flag == ullint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, int_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: INT\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
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
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: LONG\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case FLOAT:
                if (flag == 0 || flag == complex_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, float_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: FLOAT\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case DOUBLE:
                if (flag == 0 || flag == lint_type || flag == complex_type || flag == (complex_type | lint_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, double_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: DOUBLE\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case SIGNED:
                if (flag == 0 || flag == char_type || flag == sint_type || flag == int_type || flag == lint_type ||
                    flag == llint_type || flag == (sint_type | int_type) || flag == (lint_type | int_type) ||
                    flag == (llint_type | int_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, signed_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: SIGNED\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case UNSIGNED:
                if (flag == 0 || flag == char_type || flag == sint_type || flag == int_type || flag == lint_type ||
                    flag == llint_type || flag == (sint_type | int_type) || flag == (lint_type | int_type) ||
                    flag == (llint_type | int_type))
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, unsigned_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: UNSIGNED\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                break;
            case _BOOL:
                if (flag != 0) {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: _BOOL\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
                addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, bool_type);
                break;
            case _COMPLEX:
                if (flag == 0 || flag == float_type || hasFlag(flag, double_type) || flag == lint_type)
                    addTypeNode((int *) &spec_node->stype, spec_node->type_specs, val, complex_type);
                else {
                    fprintf(stderr, "%s:%d: Error: invalid type specifier: _COMPLEX\n", entry.generic->file, entry.generic->line);
                    exit(EXIT_FAILURE);
                }
        }
    }
    else if(val->type == NODE_SYMTAB && ( ((struct symtab_entry_generic*)val)->st_type == ENTRY_STAG
                || ((struct symtab_entry_generic*)val)->st_type == ENTRY_UTAG ) ){
        entry.generic->file = ((struct symtab_entry_generic*)val)->file;
        entry.generic->line = ((struct symtab_entry_generic*)val)->line;
        //Variable is of type struct, need to store pointer to struct in curr declaration for use with variable type
        if(flag != 0){
            fprintf(stderr, "%s:%d: Error: two or more data types in declaration specifiers\n", entry.generic->file, entry.generic->line);
            exit(EXIT_FAILURE);
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
                fprintf(stderr, "%s:%d: Error: invalid use of restrict\n", val->file, val->line);
                exit(EXIT_FAILURE);
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
    if(spec_node->stype == 0 || spec_node->stype == signed_type || spec_node->stype == unsigned_type){
        spec_node->stype |= int_type;
        if(spec_node->stype == int_type){
            if(spec_node->qtype != 0){
                entry.generic->file = ((struct LexVal*)spec_node->type_quals->els[0])->file;
                entry.generic->line = ((struct LexVal*)spec_node->type_quals->els[0])->line;
            }
            else if(entry.generic->stgclass != -1){
                entry.generic->file = entry.generic->storageNode->file;
                entry.generic->line = entry.generic->storageNode->line;
            }
            else{
                fprintf(stderr, "%s:%d: Error: no declaration specifiers\n", currFile, currLine);
                exit(EXIT_FAILURE);
            }

            fprintf(stderr, "%s:%d: Warning: type defaults to 'int' in declaration\n", entry.generic->file, entry.generic->line);
        }
    }

    if(spec_node->stype == complex_type || spec_node->stype == (complex_type | lint_type)){
        fprintf(stderr, "%s:%d: Error: _Complex type must have a further real specifier\n", entry.generic->file, entry.generic->line);
        exit(EXIT_FAILURE);
    }
}

struct astnode_fncndec* startFuncDecl(bool params, struct LexVal *lexVal){
    struct astnode_fncndec *fncndec = (struct astnode_fncndec *) allocEntry(ENTRY_FNCN, false);
    
    fncndec->type = NODE_FNCNDEC;
    fncndec->defined = false;
    fncndec->unknown = true;
    fncndec->noIdent = false;
    fncndec->none = false;
    fncndec->scope = (struct symtab_func*)symtabCreate(SCOPE_PROTO, TAB_FUNC, lexVal);
    fncndec->scope->parentFunc = fncndec;
    if(params){
        fncndec->unknown = false;
        currDecl.generic = allocEntry(ENTRY_GENERIC, true);
    }

    return fncndec;
}

struct astnode_lst* startFncnArgs(struct astnode_hdr *arg){
    struct symtab_entry_generic *entry = (struct symtab_entry_generic*)arg;
    if(entry->child == (struct astnode_spec_inter*)entry->type_spec && hasFlag(entry->type_spec->stype,void_type)){
        ((struct symtab_func*)currTab)->parentFunc->none = true;
        ((struct symtab_func*)currTab)->parentFunc->noIdent = false;
    }

    clearEntry(currDecl);

    return allocList(arg);
}

void addFncnArg(struct astnode_lst *lst, struct astnode_hdr *arg){
    struct symtab_entry_generic *entry = (struct symtab_entry_generic*)arg;
    if(entry->child == (struct astnode_spec_inter*)entry->type_spec && hasFlag(entry->type_spec->stype,void_type)){
        fprintf(stderr, "%s:%d: Error: 'void' must be the only parameter\n", entry->file, entry->line);
        exit(EXIT_FAILURE);
    }

    clearEntry(currDecl);
    addToList(lst, arg);
}

struct astnode_spec_inter* setFncn(struct astnode_fncndec *fncndec, struct astnode_spec_inter *next){
    if(next->parent->type == NODE_ARY){
        fprintf(stderr, "%s:%d: Error: declaration of an array of functions\n", currDecl.generic->file, currDecl.generic->line);
        exit(EXIT_FAILURE);
    }
    else if(next->parent->type == NODE_FNCNDEC){
        fprintf(stderr, "%s:%d: Error: declaration of a function returning a function\n", currDecl.generic->file, currDecl.generic->line);
        exit(EXIT_FAILURE);
    }
    
    fncndec->child = next;
    next->parent->child = (struct astnode_spec_inter*)fncndec;
    fncndec->parent = next->parent;
    next->parent = (struct astnode_spec_inter*)fncndec;
    
    return (struct astnode_spec_inter*)fncndec;
}


struct astnode_fncndec* addFuncArgs(struct astnode_lst *args, struct symtab *symtab, bool varArgs){
    if (symtab->tabType != TAB_FUNC){
        fprintf(stderr, "%s:%d: Error: attempting to add function arguments to non-function symtab\n", currFile, currLine);
        return NULL;
    }

    struct symtab_func *funcTab = (struct symtab_func*)symtab;
    struct astnode_fncndec *parentFunc = funcTab->parentFunc;

    parentFunc->args = args;
    parentFunc->varArgs = varArgs;

    return parentFunc;
}

struct astnode_ptr* allocPtr(){
    struct astnode_ptr *ptr = mallocSafeParse(sizeof(struct astnode_ptr));
    ptr->type = NODE_PTR;
    ptr->qtype = 0;
    ptr->type_quals = allocList((struct astnode_hdr*)NULL);

    return ptr;
}

struct astnode_spec_inter* setPtr(struct astnode_spec_inter *ptr, struct astnode_spec_inter *prev){
    ptr->parent = prev;
    prev->child->parent = ptr;
    ptr->child = prev->child;
    prev->child = ptr;

    return ptr;
}

struct astnode_spec_inter* allocAry(struct astnode_spec_inter *next, struct LexVal *val, union symtab_entry entry, struct symtab *symtab){
    if(next->parent->type == NODE_FNCNDEC){
        fprintf(stderr, "%s:%d: Error: declaration of a function returning an array\n", currDecl.generic->file, currDecl.generic->line);
        exit(EXIT_FAILURE);
    }

    if(val == NULL && symtab->scope != SCOPE_PROTO && symtab->scope != SCOPE_STRUCT && entry.generic->stgclass != STG_EXTERN && (entry.generic->stgclass != -1 || symtab->scope != SCOPE_FILE)){
        fprintf(stderr, "%s:%d: Error: array size missing\n", currDecl.generic->file, currDecl.generic->line);
        exit(EXIT_FAILURE);
    }

    if(val != NULL){
        if(val->tflags >= float_type){
            fprintf(stderr, "%s:%d: Error: size of array has non-integer type\n", currDecl.generic->file, currDecl.generic->line);
            exit(EXIT_FAILURE);
        }

        if(val->value.num_val.integer_val == 0){
            fprintf(stderr, "%s:%d: Error: array size must be positive\n", currDecl.generic->file, currDecl.generic->line);
            exit(EXIT_FAILURE);
        }
    }

    if(next->parent->child->type == NODE_ARY && !((struct astnode_ary*)next->parent->child)->complete || next->parent->type == NODE_ARY && val == NULL){
        fprintf(stderr, "%s:%d: Error: only first dimension of multidimensional array can be empty\n", currDecl.generic->file, currDecl.generic->line);
        exit(EXIT_FAILURE);
    }

    struct astnode_ary *ary = mallocSafeParse(sizeof(struct astnode_ary));
    ary->type = NODE_ARY;
    if(val == NULL)
        ary->complete = false;
    else{
        ary->complete = true;
        ary->length = val->value.num_val.integer_val;
    }

    ary->child = next;
    next->parent->child = (struct astnode_spec_inter*)ary;
    ary->parent = next->parent;
    next->parent = (struct astnode_spec_inter*)ary;

    return (struct astnode_spec_inter*)ary;
}

void freeInterNodes(){
    currDecl.generic->child = (struct astnode_spec_inter*)currDecl.generic->type_spec;
    currDecl.generic->type_spec->numParents++;
    currDecl.generic->type_spec->parents = realloc(currDecl.generic->type_spec->parents, sizeof(struct astnode_spec_inter*) * currDecl.generic->type_spec->numParents);
    currDecl.generic->type_spec->parents[currDecl.generic->type_spec->numParents-1] = currDecl.generic->type_spec->parent;
    currDecl.generic->type_spec->parent = (struct astnode_spec_inter*)currDecl.generic;
}

void addStmt(struct symtab *symtab, struct astnode_hdr *stmt){
    if(stmt == NULL || symtab == NULL || symtab->stmtList == NULL){
        fprintf(stderr, "%s:%d: Error: failed to add statement - invalid params\n", currFile, currLine);
        return;
    }

    addToList(symtab->stmtList, stmt);
}

struct astnode_hdr* genNoopStmt(){
    struct astnode_hdr *noop = mallocSafeParse(sizeof(struct astnode_hdr));
    noop->type = NODE_NOOP;
    return noop;
}

char* autoSprintf(long long inVal){
    size_t neededSize = snprintf(NULL, 0, "%lld", inVal);
    char *buff = mallocSafeParse(neededSize+1);
    sprintf(buff, "%lld", inVal);
    return buff;
}

struct astnode_hdr* genLabel(enum label_type labelType, struct astnode_hdr *val, struct astnode_hdr *stmt, struct symtab *symtab){
    struct astnode_label *label = (struct astnode_label*)allocEntry(ENTRY_LABEL, true);
    label->labelType = labelType;
    label->stmtNode = stmt;
    label->ns = LABEL;

    if(labelType != LAB_GENERIC){
        while(symtab->swtch == NULL){
            symtab = symtab->parent;
            if(symtab == NULL){
                fprintf(stderr, "%s:%d: Error: %s label not within a switch statement\n", currFile, currLine, labelType == LAB_CASE ? "case label" : "'default'");
                exit(EXIT_FAILURE);
            }
        }

        struct astnode_ctrl *swtch = symtab->swtch;
        label->line = currLine;
        label->file = currFile;
        label->exprNode = val;
        if(labelType == LAB_CASE)
            addToList(swtch->caseList, val);
        else{
            if(swtch->dflt != NULL){
                fprintf(stderr, "%s:%d: Error: multiple default labels in one switch\n", currFile, currLine);
                exit(EXIT_FAILURE);
            }

            swtch->dflt = val;
        }
    }
    else{
        if(val->type != NODE_LEX && ((struct LexVal*)val)->sym != IDENT){
            fprintf(stderr, "%s:%d: Error: label is not an identifier\n", currFile, currLine);
            exit(EXIT_FAILURE);
        }
        
        label->line = ((struct LexVal*)val)->line;
        label->file = ((struct LexVal*)val)->file;
        label->linkage = LINK_NONE;
        label->ident = (struct LexVal*)val;

        symtabEnter(symtab, (union symtab_entry)label, false);
    }

    return (struct astnode_hdr*)label;
}

struct astnode_hdr* genCtrl(enum ctrl_type ctrlType, struct astnode_hdr *expr, struct astnode_hdr *stmt,
                            struct astnode_hdr *stmtAlt, struct astnode_hdr *expr2, struct astnode_hdr *expr3,
                            struct symtab *symtab){
    struct astnode_ctrl *selStmt = mallocSafeParse(sizeof(struct astnode_ctrl));
    selStmt->type = NODE_CTRL;
    selStmt->ctrlType = ctrlType;
    selStmt->caseList = allocList(NULL);
    selStmt->dflt = NULL;
    selStmt->file = currFile;
    selStmt->line = currLine;
    if(ctrlType == CTRL_FOR) {
        if(expr == NULL)
            expr = genNoopStmt();
        struct astnode_lst *exprLst = (struct astnode_lst*)allocList(expr);
        addToList(exprLst, expr2);
        if(expr3 == NULL)
            expr3 = genNoopStmt();
        addToList(exprLst, expr3);
        selStmt->ctrlExpr = (struct astnode_hdr*)exprLst;
    }
    else{
        if(ctrlType == CTRL_SWITCH){
            selStmt->outerSwtch = symtab->swtch;
            symtab->swtch = selStmt;
        }

        selStmt->ctrlExpr = expr;
    }

    selStmt->stmt = stmt;
    selStmt->stmtSecondary = stmtAlt;
    return (struct astnode_hdr*)selStmt;
}

void setSwitchStmt(struct astnode_hdr *swtch, struct astnode_hdr *stmt, struct symtab *symtab){
    ((struct astnode_ctrl*)swtch)->stmt = stmt;
    symtab->swtch = ((struct astnode_ctrl*)swtch)->outerSwtch;
}

struct astnode_hdr* genJump(enum jump_type jumpType, struct astnode_hdr *val, struct symtab *symtab){
    if(jumpType == JUMP_RET){
        while(symtab->scope != SCOPE_FUNC){
            symtab = symtab->parent;
            // should not happen, statements only in functions
            if(symtab == NULL){
                fprintf(stderr, "%s:%d: Error: unknown error occurred\n", currFile, currLine);
                exit(EXIT_FAILURE);
            }
        }

        struct astnode_fncndec *func = ((struct symtab_func*)symtab)->parentFunc;
        bool retVoid = func->child->type == NODE_TYPESPEC && ((struct astnode_typespec*)func->child)->stype == void_type;
        if(retVoid && val->type != NODE_NOOP){
            fprintf(stderr, "%s:%d: Error: 'return' with a value, in function returning void\n", currFile, currLine);
            exit(EXIT_FAILURE);
        }
        else if(!retVoid && val->type == NODE_NOOP){
            fprintf(stderr, "%s:%d: Error: 'return' with no value, in function returning non-void\n", currFile, currLine);
            exit(EXIT_FAILURE);
        }
    }

    struct astnode_jump *jmpNode = (struct astnode_jump*)mallocSafeParse(sizeof(struct astnode_jump));
    jmpNode->type = NODE_JMP;
    jmpNode->jumpType = jumpType;
    jmpNode->val = val;
    jmpNode->file = currFile;
    jmpNode->line = currLine;
    return (struct astnode_hdr*)jmpNode;
}

void printAst(struct astnode_hdr *hdr, int lvl, bool isFunc){
    union astnode *node = (union astnode*) &hdr;
    if(hdr->type == NODE_NOOP)
        return;

    printTabs(false, lvl, 0);

    switch (hdr->type) {
        case NODE_LEX:
            switch (node->lexNode->sym) {
                case IDENT:
                    printf("IDENT %s\n", node->lexNode->value.string_val); break;
                case NUMBER:
                    switch (node->lexNode->tflags) {
                        case int_type: 
                            printf("CONSTANT: (type=int)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case uint_type: 
                            printf("CONSTANT: (type=unsigned,int)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case lint_type: 
                            printf("CONSTANT: (type=long)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case ulint_type: 
                            printf("CONSTANT: (type=unsigned,long)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case llint_type: 
                            printf("CONSTANT: (type=longlong)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case ullint_type: 
                            printf("CONSTANT: (type=unsigned,longlong)%lld\n", node->lexNode->value.num_val.integer_val); break;
                        case float_type: 
                            printf("CONSTANT: (type=float)%g\n", node->lexNode->value.num_val.float_val); break;
                        case double_type: 
                            printf("CONSTANT: (type=double)%g\n", node->lexNode->value.num_val.double_val); break;
                        case ldouble_type: 
                            printf("CONSTANT: (type=longdouble)%Lg\n", node->lexNode->value.num_val.ldouble_val); break;
                    }
                    break;
                case CHARLIT:
                    printf("CONSTANT: (type=int)%d\n", node->lexNode->value.string_val[0]); break;
                case STRING:
                    printf("STRING   %S\n", (wchar_t *) node->lexNode->value.string_val); break;
            }
            break;
        case NODE_UNOP:
            switch (node->unNode->op) {
                case PLUSPLUS:
                    printf("UNARY OP POSTINC\n"); break;
                case MINUSMINUS:
                    printf("UNARY OP POSTDEC\n"); break;
                case '&':
                    printf("ADDRESSOF\n"); break;
                case '*':
                    printf("DEREF\n"); break;
                case SIZEOF:
                    printf("SIZEOF\n"); break;
                default:
                    printf("UNARY OP %c\n", node->unNode->op);
            }
            printAst((struct astnode_hdr *) node->unNode->opand, lvl + 1, false);
            break;
        case NODE_BINOP:
            switch (node->binNode->op) {
                case '.':
                    printf("SELECT, member %s\n", ((struct LexVal*)node->binNode->right)->value.string_val); break;
                case SHL:
                    printf("BINARY OP <<\n"); break;
                case SHR:
                    printf("BINARY OP >>\n"); break;
                case '<':
                    printf("COMPARISON OP <\n"); break;
                case '>':
                    printf("COMPARISON OP >\n"); break;
                case LTEQ:
                    printf("COMPARISON OP <=\n"); break;
                case GTEQ:
                    printf("COMPARISON OP >=\n"); break;
                case EQEQ:
                    printf("COMPARISON OP ==\n"); break;
                case NOTEQ:
                    printf("COMPARISON OP !=\n"); break;
                case LOGAND:
                    printf("LOGICAL OP &&\n"); break;
                case LOGOR:
                    printf("LOGICAL OP ||\n"); break;
                case '=':
                    printf("ASSIGNMENT\n"); break;
                default:
                    printf("BINARY OP %c\n", node->binNode->op);
            }
            printAst(node->binNode->left, lvl + 1, false);
            if(node->binNode->op != '.')
                printAst(node->binNode->right, lvl + 1, false);
            break;
        case NODE_TEROP:
            printf("TERNARY OP, IF:\n");
            printAst(node->terNode->first, lvl + 1, false);
            printTabs(false, lvl, 0);
            printf("THEN:\n");
            printAst(node->terNode->second, lvl + 1, false);
            printTabs(false, lvl, 0);
            printf("ELSE:\n");
            printAst(node->terNode->third, lvl + 1, false);
            break;
        case NODE_CAST:
            printf("CAST OP\n  ");
            printSpec(node->castNode->cast_spec, currTab, false, 0, lvl);
            printAst(node->castNode->opand, lvl + 1, false);
            break;
        case NODE_LST: {
            if (!isFunc) printf("LIST {\n");
            for (int i = 0; i < node->lst->numVals; i++) {
                if (isFunc) {
                    if (i > 0)
                        printTabs(false, lvl, 0);
                    printf("arg #%d=\n", i + 1);
                    printAst(node->lst->els[i], lvl + 1, true);
                } else
                    printAst(node->lst->els[i], lvl + 1, false);
            }
            if (!isFunc) {
                printTabs(false, lvl, 0);
                printf("}\n");
            }
            break;
        }
        case NODE_FNCN:
            printf("FNCALL, %d arguments\n", node->fncn->lst->numVals);
            printAst(node->fncn->name, lvl + 1, false);
            if (node->fncn->lst->numVals > 0)
                printAst((struct astnode_hdr *) node->fncn->lst, lvl, true);
            break;
        case NODE_CTRL: {
            switch (node->ctrl->ctrlType) {
                case CTRL_IF:
                    printf("IF:\n");
                    printAst(node->ctrl->ctrlExpr, lvl+1, false);
                    printTabs(false, lvl, 0);
                    printf("THEN:\n");
                    printAst(node->ctrl->stmt, lvl+1, false);
                    if(node->ctrl->stmtSecondary != NULL){
                        printTabs(false, lvl, 0);
                        printf("ELSE:\n");
                        printAst(node->ctrl->stmtSecondary, lvl+1, false);
                    }
                    break;
                case CTRL_SWITCH:
                case CTRL_WHILE:
                case CTRL_DO:
                    if(node->ctrl->ctrlType == CTRL_SWITCH)
                        printf("SWITCH");
                    else if(node->ctrl->ctrlType == CTRL_WHILE)
                        printf("WHILE");
                    else
                        printf("DO WHILE");
                    printf(", EXPR:\n");
                    printAst(node->ctrl->ctrlExpr, lvl+1, false);
                    printTabs(false, lvl, 0);
                    printf("BODY:\n");
                    printAst(node->ctrl->stmt, lvl+1, false);
                    break;
                case CTRL_FOR:
                    printf("FOR\n");

                    printTabs(false, lvl, 0);
                    printf("INIT:\n");
                    struct astnode_lst *forLst = (struct astnode_lst *)node->ctrl->ctrlExpr;
                    printAst(forLst->els[0], lvl+1, false);

                    printTabs(false, lvl, 0);
                    printf("COND:\n");
                    printAst(forLst->els[1], lvl+1, false);

                    printTabs(false, lvl, 0);
                    printf("BODY:\n");
                    printAst(node->ctrl->stmt, lvl+1, false);

                    printTabs(false, lvl, 0);
                    printf("INCR:\n");
                    printAst(forLst->els[2], lvl+1, false);
                    break;
            }
            
            break;
        }
        case NODE_JMP:
            switch (node->jump->jumpType) {
                case JUMP_GOTO:
                    printf("GOTO %s", ((struct LexVal*)node->jump->val)->value.string_val);
                    union symtab_entry lab = symtabLookup(currTab, LABEL, ((struct LexVal*)node->jump->val)->value.string_val, true, LINK_NONE);
                    if(lab.label != NULL)
                        printf(" (DEF)");
                    printf("\n");
                    break;
                case JUMP_CONT:
                    printf("CONTINUE\n");
                    break;
                case JUMP_BREAK:
                    printf("BREAK\n");
                    break;
                case JUMP_RET:
                    printf("RETURN:\n");
                    printAst(node->jump->val, lvl+1, false);
                    break;
            }
            break;
        case NODE_SYMTAB: {
            switch (node->symEntry->st_type) {
                case ENTRY_VAR:
                    printf("stab_var name=%s def @%s:%d\n", node->symEntry->ident->value.string_val,
                           node->symEntry->file, node->symEntry->line);
                    break;
                case ENTRY_FNCN:
                    printf("stab_fn name=%s def @%s:%d\n", node->symEntry->ident->value.string_val,
                           node->symEntry->file, node->symEntry->line);
                    break;
                case ENTRY_LABEL: {
                    int labLvl = lvl + 1;
                    switch (node->label->labelType) {
                        case LAB_GENERIC:
                            printf("LABEL(%s):\n", ((struct LexVal*)node->label->ident)->value.string_val);
                            break;
                        case LAB_CASE:
                            printf("CASE\n");

                            printTabs(false, lvl+1, 0);
                            printf("EXPR:\n");
                            printAst(node->label->exprNode, lvl+2, false);

                            printTabs(false, lvl+1, 0);
                            printf("STMT:\n");
                            labLvl++;
                            break;
                        case LAB_DEFAULT:
                            printf("DEFAULT\n");
                            break;
                    }
                    printAst(node->label->stmtNode, labLvl, false);
                    break;
                }
                default:
                    printf("Unable to print symtab entry %d as statement\n", node->symEntry->st_type);
            }
            break;
        }
    }

    if(lvl == 0 && hdr->type != NODE_LST)
        printf("\n");
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
        default:
            if(entry.generic->linkage == LINK_EXT)
                storage = "extern";
            break;
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
        char *structName, *structType;
        char *fileName = entry.generic->file;
        int line = entry.generic->line;
        if(entry.tag->ident == NULL)
            structName = "(anonymous)";
        else
            structName = entry.tag->ident->value.string_val;

        structType = entry.tag->st_type == ENTRY_STAG ? "struct" : "union";

        if(entry.tag->complete){
            printf("%s %s definition at %s:%d{\n",
                   structType, structName, fileName, line);
        }

        return;
    }

    printf("%s is defined at %s:%d [in %s scope starting at %s:%d] as a \n", entry.generic->ident->value.string_val,
           entry.generic->file, entry.generic->line, scope, symtab->file, symtab->line);
    if(isMemb) {
        char *structName, *structType;
        if (symtab->tabType != TAB_STRUCT) {
            fprintf(stderr, "%s:%d: Error: attempting to print member info with tab not of type TAB_STRUCT\n", entry.generic->file, entry.generic->line);
            structName = "";
        }
        else if (((struct symtab_struct*)symtab)->parentStruct->ident != NULL)
            structName = ((struct symtab_struct*)symtab)->parentStruct->ident->value.string_val;
        else
            structName = "(anonymous)";

        structType = ((struct symtab_struct*)symtab)->parentStruct->st_type == ENTRY_STAG ? "struct" : "union";

        printf("field of %s %s  off=%zu bit_off=%d bit_wid=%d, type:\n  ",
               structType, structName, entry.memb->structOffset, entry.memb->bitOffset, entry.memb->bitWidth
        );
    }
    else if(isFunc)
        printf("%s   function returning\n  ", storage);
    else{
        printf("%s ", usage);
        if(argNum > 0 && symtab->scope == SCOPE_PROTO)
            printf("(argument #%ld) ", argNum);

        if(entry.generic->stgclass != -1 || entry.generic->linkage == LINK_EXT)
            printf("with stgclass %s  ", storage);

        if(entry.generic->ns == LABEL)
            printf("\n");
        else
            printf("of type:\n  ");
    }

    struct astnode_spec_inter *child = entry.generic->child;
    if(child != NULL)
        printSpec(child, symtab, isFunc, 0, 0);

    if(isFunc)
        printArgs(entry.fncn, symtab, true, 0, 0);

    if(symtab->scope != SCOPE_STRUCT && symtab->scope != SCOPE_PROTO)
        printf("\n");
}

void printSpec(struct astnode_spec_inter *next, struct symtab *symtab, bool func, long level, long castLevel){
    printTabs(func, level, castLevel);
    if(next->type != NODE_TYPESPEC){    
        if(next->type == NODE_ARY){
            printf("array of  ");
            if(((struct astnode_ary*)next)->complete)
                printf("%d", ((struct astnode_ary*)next)->length);
            else
                printf("incomplete");

            printf(" elements of type\n  ");
            printSpec(next->child, symtab, func, level+1, castLevel);
            return;
        }
        else if(next->type == NODE_PTR){
            printQual(((struct astnode_ptr*)next)->qtype);
            printf("pointer to \n  ");
            printSpec(next->child, symtab, func, level+1, castLevel);
            return;
        }
        else if(next->type == NODE_FNCNDEC){
            printf("function returning\n  ");
            printSpec(next->child, symtab, func, level+1, castLevel);
            printTabs(func, level, castLevel);
            printArgs((struct astnode_fncndec*)next, symtab, func, level+1, castLevel);
            return;
        }
    }

    struct astnode_typespec *spec_node = (struct astnode_typespec*)next;
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
        char *structName, *structType;
        struct astnode_tag *tagNode = ((struct astnode_tag*)spec_node->type_specs->els[0]);
        if(tagNode->ident == NULL)
            structName = "(anonymous)";
        else
            structName = tagNode->ident->value.string_val;

        structType = tagNode->st_type == ENTRY_STAG ? "struct" : "union";

        printf("%s %s ", structType, structName);

        struct astnode_tag *structDef = NULL;
        if(tagNode->ident == NULL) {
            if(tagNode->complete)
                structDef = ((struct symtab_struct*)tagNode->container)->parentStruct;
            else
                fprintf(stderr, "%s:%d: Error: anonymous struct incomplete\n", currFile, currLine);
        }
        else
            structDef = symtabLookup(symtab, TAG, structName, false, -1).tag;

        if(structDef == NULL || !structDef->complete)
            printf("(incomplete)");
        else
            printf("(defined at %s:%d)", structDef->file, structDef->line);
    }

    printf("\n");
}

void printTabs(bool func, long level, long castLevel){
    if(func)
        printf(" ");

    for(int i = 0; i < level; i++)
        printf(" ");

    for(int i = 0; i < castLevel; i++)
        printf(" ");
}

void printArgs(struct astnode_fncndec *fncn, struct symtab *symtab, bool func, long level, long castLevel){
    if(fncn->none){
        printf("  and taking no arguments\n");
    }
    else if(fncn->args == NULL || fncn->args->numVals == 0){
        printf("  and taking unknown arguments\n");
    }
    else {
        printf("  and taking the following arguments\n");
        for(int i = 0; i < fncn->args->numVals; i++){
            // arguments can only be entries
            struct symtab_entry_generic *arg = (struct symtab_entry_generic *)fncn->args->els[i];
            if(arg->child != NULL) {
                printf("  ");
                printSpec(arg->child, symtab, func, level, castLevel);
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
        fprintf(stderr, "%s:%d: Error: printStruct called with non-struct node\n", currFile, currLine);
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

    printf("} (size==%zu)\n\n", getStructSize(structNode, false));
}

void printFunc(struct astnode_fncndec *func){
    static int funcIdx = 1;
    printf("AST Dump for function\n");
    dumpStatements(currTab->stmtList, 1);
    printf("\n");

    if(doGenQuad)
        genQuads(currTab->stmtList, NULL, funcIdx, func);

    funcIdx++;
}

void dumpStatements(struct astnode_lst *stmtLst, int level){
    printTabs(false, level, 0);
    printf("LIST {\n");

    for(int i = 0; i < stmtLst->numVals; i++){
        if(stmtLst->els[i]->type == NODE_LST){
            //This is a compound statement, recurse
            dumpStatements((struct astnode_lst*)stmtLst->els[i], level + 1);
        }
        else
            printAst(stmtLst->els[i], level + 1, false);
    }

    printTabs(false, level, 0);
    printf("}\n");
}
