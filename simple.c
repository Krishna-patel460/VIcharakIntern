#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>



typedef enum {
    TK_EOF,
    TK_INT,
    TK_IF,
    TK_IDENTIFIER,
    TK_NUMBER,
    TK_ASSIGN, 
    TK_PLUS,  
    TK_MINUS,  
    TK_MUL,   
    TK_DIV,    
    TK_LPAREN, 
    TK_RPAREN, 
    TK_LBRACE,
    TK_RBRACE, 
    TK_SEMI,  
    TK_UNKNOWN
} TokenType;

typedef struct {
    TokenType type;
    char text[128];
    int number; 
} Token;

FILE *infile;
int curc = ' ';

void nextch() { curc = fgetc(infile); }

void skip_ws() {
    while (curc != EOF && isspace(curc)) nextch();
}

Token get_token() {
    skip_ws();
    Token t; t.type = TK_EOF; t.text[0] = '\0'; t.number = 0;
    if (curc == EOF || curc == -1) { t.type = TK_EOF; return t; }
    if (isalpha(curc) || curc == '_') {
        int i = 0;
        while (isalnum(curc) || curc == '_') {
            if (i < 127) t.text[i++] = curc;
            nextch();
        }
        t.text[i] = '\0';
        if (strcmp(t.text, "int") == 0) t.type = TK_INT;
        else if (strcmp(t.text, "if") == 0) t.type = TK_IF;
        else t.type = TK_IDENTIFIER;
        return t;
    }
    if (isdigit(curc)) {
        int val = 0;
        while (isdigit(curc)) {
            val = val * 10 + (curc - '0');
            nextch();
        }
        t.type = TK_NUMBER;
        t.number = val & 0xFF;
        sprintf(t.text, "%d", t.number);
        return t;
    }
    
    switch (curc) {
        case '=': t.type = TK_ASSIGN; t.text[0] = '='; t.text[1] = '\0'; nextch(); return t;
        case '+': t.type = TK_PLUS; t.text[0] = '+'; t.text[1] = '\0'; nextch(); return t;
        case '-': t.type = TK_MINUS; t.text[0] = '-'; t.text[1] = '\0'; nextch(); return t;
        case '*': t.type = TK_MUL; t.text[0] = '*'; t.text[1] = '\0'; nextch(); return t;
        case '/': t.type = TK_DIV; t.text[0] = '/'; t.text[1] = '\0'; nextch(); return t;
        case '(' : t.type = TK_LPAREN; t.text[0] = '('; t.text[1] = '\0'; nextch(); return t;
        case ')' : t.type = TK_RPAREN; t.text[0] = ')'; t.text[1] = '\0'; nextch(); return t;
        case '{' : t.type = TK_LBRACE; t.text[0] = '{'; t.text[1] = '\0'; nextch(); return t;
        case '}' : t.type = TK_RBRACE; t.text[0] = '}'; t.text[1] = '\0'; nextch(); return t;
        case ';' : t.type = TK_SEMI; t.text[0] = ';'; t.text[1] = '\0'; nextch(); return t;
        default:
            t.type = TK_UNKNOWN;
            t.text[0] = (char)curc;
            t.text[1] = '\0';
            nextch();
            return t;
    }
}


Token curTok;
void advance() { curTok = get_token(); }



typedef enum { AST_NUM, AST_IDENT, AST_BINOP, AST_ASSIGN, AST_DECL, AST_IF, AST_BLOCK } AstType;

typedef struct Ast {
    AstType type;
    
    int value;           
    char name[64];      
    char op;             
    struct Ast *left, *right; 
    struct Ast **stmts;  
    int nstmts;
} Ast;

Ast *make_num(int v) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_NUM; a->value = v; return a;
}
Ast *make_ident(const char *s) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_IDENT; strncpy(a->name, s, 63); return a;
}
Ast *make_binop(char op, Ast *l, Ast *r) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_BINOP; a->op = op; a->left = l; a->right = r; return a;
}
Ast *make_assign(Ast *lhs, Ast *rhs) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_ASSIGN; a->left = lhs; a->right = rhs; return a;
}
Ast *make_decl(const char *name, Ast *init) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_DECL; strncpy(a->name, name, 63); a->right = init; return a;
}
Ast *make_if(Ast *cond, Ast *block) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_IF; a->left = cond; a->right = block; return a;
}
Ast *make_block(Ast **stmts, int n) {
    Ast *a = calloc(1, sizeof(Ast)); a->type = AST_BLOCK; a->stmts = stmts; a->nstmts = n; return a;
}



void expect(TokenType t) {
    if (curTok.type != t) {
        fprintf(stderr, "Parse error: expected token %d but got %d (text=%s)\n", t, curTok.type, curTok.text);
        exit(1);
    }
    advance();
}


Ast *parse_statement();

Ast *parse_primary() {
    if (curTok.type == TK_NUMBER) {
        Ast *n = make_num(curTok.number); advance(); return n;
    } else if (curTok.type == TK_IDENTIFIER) {
        Ast *id = make_ident(curTok.text); advance(); return id;
    } else if (curTok.type == TK_LPAREN) {
        advance();
        Ast *e = parse_statement(); 
        expect(TK_RPAREN);
        return e;
    } else {
        fprintf(stderr, "Parse error: primary unexpected token %d (%s)\n", curTok.type, curTok.text);
        exit(1);
    }
}


Ast *parse_expr();

Ast *parse_factor() { return parse_primary(); }

Ast *parse_term() {
    Ast *node = parse_factor();
    while (curTok.type == TK_MUL || curTok.type == TK_DIV) {
        char op = (curTok.type == TK_MUL ? '*' : '/');
        advance();
        Ast *rhs = parse_factor();
        node = make_binop(op, node, rhs);
    }
    return node;
}

Ast *parse_expr() {
    Ast *node = parse_term();
    while (curTok.type == TK_PLUS || curTok.type == TK_MINUS) {
        char op = (curTok.type == TK_PLUS ? '+' : '-');
        advance();
        Ast *rhs = parse_term();
        node = make_binop(op, node, rhs);
    }
    return node;
}

Ast *parse_statement() {
    if (curTok.type == TK_INT) {
        advance();
        if (curTok.type != TK_IDENTIFIER) { fprintf(stderr, "Expected identifier after int\n"); exit(1); }
        char name[64]; strncpy(name, curTok.text, 63);
        advance();
        Ast *init = NULL;
        if (curTok.type == TK_ASSIGN) {
            advance();
            init = parse_expr();
        }
        expect(TK_SEMI);
        return make_decl(name, init);
    } else if (curTok.type == TK_IF) {
        advance();
        expect(TK_LPAREN);
        Ast *cond = parse_expr();
        expect(TK_RPAREN);
        if (curTok.type != TK_LBRACE) { fprintf(stderr, "Expected '{' after if\n"); exit(1); }
        advance();
        Ast **stmts = NULL; int cap=0, n=0;
        while (curTok.type != TK_RBRACE && curTok.type != TK_EOF) {
            Ast *s = parse_statement();
            if (n >= cap) { cap = cap ? cap*2 : 8; stmts = realloc(stmts, cap * sizeof(Ast*)); }
            stmts[n++] = s;
        }
        expect(TK_RBRACE);
        Ast *block = make_block(stmts, n);
        return make_if(cond, block);
    } else if (curTok.type == TK_IDENTIFIER) {
        Ast *lhs = make_ident(curTok.text);
        advance();
        if (curTok.type == TK_ASSIGN) {
            advance();
            Ast *rhs = parse_expr();
            expect(TK_SEMI);
            return make_assign(lhs, rhs);
        } else {
            fprintf(stderr, "Unexpected identifier not followed by assignment\n");
            exit(1);
        }
    } else if (curTok.type == TK_EOF) {
        return NULL;
    } else {
        fprintf(stderr, "Parse error: unexpected token %d (%s)\n", curTok.type, curTok.text);
        exit(1);
    }
}


Ast **parse_program(int *out_n) {
    Ast **stmts = NULL; int cap=0, n=0;
    while (curTok.type != TK_EOF) {
        Ast *s = parse_statement();
        if (!s) break;
        if (n >= cap) { cap = cap ? cap*2 : 16; stmts = realloc(stmts, cap * sizeof(Ast*)); }
        stmts[n++] = s;
    }
    *out_n = n;
    return stmts;
}



typedef struct {
    char name[64];
    int addr;
} Sym;

Sym *symtab = NULL;
int symcap = 0, symn = 0;
int next_data_addr = 0x10; 

int sym_get_addr(const char *name) {
    for (int i=0;i<symn;i++) if (strcmp(symtab[i].name, name)==0) return symtab[i].addr;
    return -1;
}
int sym_add(const char *name) {
    int a = sym_get_addr(name);
    if (a != -1) return a;
    if (symn >= symcap) { symcap = symcap ? symcap*2 : 16; symtab = realloc(symtab, symcap * sizeof(Sym)); }
    strncpy(symtab[symn].name, name, 63);
    symtab[symn].addr = next_data_addr++;
    symn++;
    return symtab[symn-1].addr;
}



int label_counter = 0;
char *new_label(const char *prefix) {
    char *buf = malloc(64);
    sprintf(buf, "%s_%d", prefix, label_counter++);
    return buf;
}


void emit(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
}
int tmp_base = 0xF0; 
int tmp_next = 0;

int alloc_tmp() { return tmp_base + (tmp_next++); }

void gen_expr(Ast *node);


void gen_expr(Ast *node) {
    if (!node) return;
    if (node->type == AST_NUM) {
        emit("ldi A %d", node->value & 0xFF);
    } else if (node->type == AST_IDENT) {
        int addr = sym_get_addr(node->name);
        if (addr == -1) { fprintf(stderr, "Undefined variable %s\n", node->name); exit(1); }
        emit("mov A M %d", addr);
    } else if (node->type == AST_BINOP) {
        int tmp = alloc_tmp();
        gen_expr(node->left);          
        emit("mov M A %d", tmp);         
        gen_expr(node->right);           
        emit("mov B M %d", tmp);         
        int tmp2 = alloc_tmp();
        emit("mov M A %d", tmp2);       
        emit("mov A M %d", tmp);        
        emit("mov B M %d", tmp2);       
        switch (node->op) {
            case '+': emit("add"); break;
            case '-': emit("sub"); break;
            case '*':
                {
                    char *lbl_start = new_label("mul_start");
                    char *lbl_end   = new_label("mul_end");
                    int tmp_acc = alloc_tmp();
                    int tmp_left = alloc_tmp(); int tmp_right = alloc_tmp();
                    emit("mov M A %d", tmp_left); 
                    emit("mov M B %d", tmp_right); 
                    emit("ldi A 0");
                    emit("mov B M %d", tmp_right);
                    emit("%s:", lbl_start);
                    emit("cmp A B"); 
                    emit("; MUL not fully implemented; placeholder: result = 0");
                    emit("jmp %s", lbl_end);
                    emit("%s:", lbl_end);
                }
                break;
            case '/':
                emit("; DIV not implemented, placeholder 0");
                emit("ldi A 0");
                break;
            default:
                fprintf(stderr, "Unknown binop %c\n", node->op); exit(1);
        }
    } else {
        fprintf(stderr, "gen_expr: unsupported node type %d\n", node->type);
        exit(1);
    }
}


void gen_stmt(Ast *s) {
    if (!s) return;
    if (s->type == AST_DECL) {
        int addr = sym_add(s->name);
        if (s->right) {
            gen_expr(s->right); 
            emit("mov M A %d", addr);
        } else {
        
            emit("ldi A 0");
            emit("mov M A %d", addr);
        }
    } else if (s->type == AST_ASSIGN) {
        if (s->left->type != AST_IDENT) { fprintf(stderr, "Left side not identifier\n"); exit(1); }
        int addr = sym_get_addr(s->left->name);
        if (addr == -1) { fprintf(stderr, "Assignment to undeclared var %s\n", s->left->name); exit(1); }
        gen_expr(s->right);
        emit("mov M A %d", addr);
    } else if (s->type == AST_IF) {
        char *lbl_end = new_label("if_end");
        gen_expr(s->left); 
        emit("ldi B 0");
        emit("cmp"); 
        emit("jz %s", lbl_end);
        
        for (int i=0;i<s->right->nstmts;i++) gen_stmt(s->right->stmts[i]);
        emit("%s:", lbl_end);
    } else if (s->type == AST_BLOCK) {
        for (int i=0;i<s->nstmts;i++) gen_stmt(s->stmts[i]);
    } else {
        fprintf(stderr, "Unsupported statement type %d\n", s->type); exit(1);
    }
}


void gen_program(Ast **stmts, int n) {
    emit("; ---- data and code generated by simplec ----");
    emit("start:");
    for (int i=0;i<n;i++) gen_stmt(stmts[i]);
    emit("hlt");
    emit("; ---- symbol table ----");
    for (int i=0;i<symn;i++) {
        emit("; var %s at %d", symtab[i].name, symtab[i].addr);
    }
}



int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "Usage: %s file.sl\n", argv[0]); return 1; }
    infile = fopen(argv[1], "r");
    if (!infile) { perror("open"); return 1; }
    nextch();
    advance(); 
    int n;
    Ast **prog = parse_program(&n);
    gen_program(prog, n);
    fclose(infile);
    return 0;
}
