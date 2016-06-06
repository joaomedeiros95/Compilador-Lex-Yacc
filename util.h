#include "hash.h"
#include <regex.h>

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

int escopo_order = 0;
int label_order = 0;
extern int yylineno;
int hasMain = 0;/* 1 -> true, 0 -> false */
FILE *cFile;
int ultimaLinha = 0;
int debug = 0;

char* returnTypeString(int type);

struct TipoCompleto {
    char *string;
    int qtdRetornos;
    int tipo;
    int iValor;
    float fValor;
    char *sValor;
};

char* newString(int size) {
    char* s = malloc(size + sizeof(char*)*200);
    return s;
}

struct TipoCompleto* newTipoCompleto() {
    struct TipoCompleto* retorno = malloc(sizeof(struct TipoCompleto));
    retorno->string = "";
    retorno->qtdRetornos = 0;
    retorno->tipo = 0;
    retorno->iValor = -1;
    retorno->fValor = -1;
    retorno->sValor = "";
    return retorno;
}

int getVarType(char *variable) {
    char *tmp = strdup(variable);
    if (checkRegex(tmp, "([0-9]*\\.[0-9]+)")) {
            return 4;        
    } else if(checkRegex(tmp, "([0-9]+)")) {
            return 1;        
    } 

    struct DataItem *item = find_item(variable, escopo_order);
    
    if(item == NULL) {
            fprintf (stderr, "ERROR: Variável %s não declarada. Linha: %d\n", variable, yylineno);
            exit(0);
    }
    
    return item->data->type;
}

int getArrayX(char *variable) {
    char *tmp = strdup(variable);
    if (checkRegex(tmp, "([0-9]*\\.[0-9]+)")) {
            return 4;        
    } else if(checkRegex(tmp, "([0-9]+)")) {
            return 1;        
    } 

    struct DataItem *item = find_item(variable, escopo_order);
    
    if(item == NULL) {
            fprintf (stderr, "ERROR: Variável %s não declarada. Linha: %d\n", variable, yylineno);
            exit(0);
    }
    
    return item->data->tamanhoX;
}

int getArrayY(char *variable) {
    char *tmp = strdup(variable);
    if (checkRegex(tmp, "([0-9]*\\.[0-9]+)")) {
            return 4;        
    } else if(checkRegex(tmp, "([0-9]+)")) {
            return 1;        
    } 

    struct DataItem *item = find_item(variable, escopo_order);
    
    if(item == NULL) {
            fprintf (stderr, "ERROR: Variável %s não declarada. Linha: %d\n", variable, yylineno);
            exit(0);
    }
    
    return item->data->tamanhoY;
}

int expressionType(int typeone, int typetwo) {
    if(typeone == typetwo) {
            return typeone;
    } else {
            if((typeone == 1 && typetwo == 4) || (typeone == 4 && typetwo == 1)) {
                    return 4;
            }
    }
}

void checktype(int typeone, int typetwo) {
    if(typeone == 0 || typetwo == 0) {
            fprintf (stderr, "Um erro inesperado ocorreu! Linha: %d\n", yylineno);
            exit(0);
    }
    
    if((typeone == 1 && typetwo == 3) || (typeone == 3 && typetwo == 1)) {
            fprintf (stderr, "ERROR: String e Int não podem ser operados/atribuidos. Linha: %d\n", yylineno);
            exit(0);
    }
    if((typeone == 1 && typetwo == 2) || (typeone == 2 && typetwo == 1)) {
            fprintf (stderr, "ERROR: Char e Int não podem ser operados/atribuidos. Linha: %d\n", yylineno);
            exit(0);
    }
    if((typeone == 4 && typetwo == 3) || (typeone == 3 && typetwo == 4)) {
            fprintf (stderr, "ERROR: String e Float não podem ser operados/atribuidos. Linha: %d\n", yylineno);
            exit(0);
    }
    if((typeone == 4 && typetwo == 2) || (typeone == 2 && typetwo == 4)) {
            fprintf (stderr, "ERROR: Char e Float não podem ser operados/atribuidos. Linha: %d\n", yylineno);
            exit(0);
    }
}

void checktypeattr(int type, char *var) {
    int vartype = getVarType(var);
    checktype(type, vartype);
}

void checktypeop(char *varone, char *vartwo) {
    int typeone = getVarType(varone);
    int typetwo = getVarType(vartwo);
    checktype(typeone, typetwo);
}

void checkRetorno(char *metodo, int quantidade) {
    //printf("%s quantidade %d\n", metodo, quantidade);
    if(quantidade < 1) {
        fprintf (stderr, "Retorno da função %s não encontrado ou não presente em todos os fluxos de execução! Linha: %d\n", metodo, yylineno);
        exit(0);
    }
}

void checkRetornoTipo(int metodotype, int retornotype) {
    if(metodotype == 0 || retornotype == 0) {
        fprintf (stderr, "Um erro inesperado ocorreu! Linha: %d\n", yylineno);
        exit(0);
    }
    
    if(metodotype != retornotype) {
        fprintf (stderr, "ERROR: O tipo do método é %s mas foi retornado %s! Linha: %d\n", returnTypeString(metodotype), returnTypeString(retornotype), yylineno);
        exit(0);
    }
    
}

void verifyDeclaration(char *nome) {
    struct DataItem *item = find_item(nome, escopo_order);
    
    if(item == NULL) {
            print_itens();
            fprintf (stderr, "ERROR: Variável %s no escopo %d não declarada. Linha: %d\n", nome, escopo_order, yylineno);
            exit(0);
    }
}

void makeVar(char *tipo, char *nome) {
    int type = returnType(tipo);
    struct Item *info;
    info = malloc(sizeof(struct Item));
    info->type = type;
    info->tamanhoX = 0;
    info->tamanhoY = 0;
    info->parametros = NULL;
    
    char escopo_ident[3] = ".N\0"; 
    char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*)));
    sprintf(escopo,"%s%d",escopo_ident,escopo_order); 
    
    char *key = malloc(sizeof(nome) + sizeof(escopo) + sizeof(char*)); 
    sprintf(key,"%s%s",nome,escopo); 
    
    add_item(key, info);
}

void makeParam(char *tipo, char *nome) {
    int type = returnType(tipo);
    struct Item *info;
    info = malloc(sizeof(struct Item));
    info->type = type;
    info->tamanhoX = 0;
    info->tamanhoY = 0;
    info->parametros = NULL;
    
    char escopo_ident[3] = ".N\0"; 
    char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*)));
    sprintf(escopo,"%s%d",escopo_ident,escopo_order + 1); 
    
    char *key = malloc(sizeof(nome) + sizeof(escopo) + sizeof(char*)); 
    sprintf(key,"%s%s",nome,escopo); 
    
    add_item(key, info);
}


void makeArray(char *tipo, char *nome, int tamanhoX, int tamanhoY) {
    int type = returnType(tipo);
    struct Item *info;
    info = malloc(sizeof(struct Item));
    info->type = type;
    
    info->tamanhoX = 0;
    info->tamanhoY = 0;
    
    if(tamanhoX >= 0) {
        info->tamanhoX = tamanhoX;
    } if(tamanhoY >= 0) {
        info->tamanhoY = tamanhoY;
    }
    info->parametros = NULL;
    
    char escopo_ident[3] = ".N\0"; 
    char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*)));
    sprintf(escopo,"%s%d",escopo_ident,escopo_order); 
    
    char *key = malloc(sizeof(nome) + sizeof(escopo) + sizeof(char*)); 
    sprintf(key,"%s%s",nome,escopo); 
    
    add_item(key, info);
}

void makeFunc(char *tipo, char *nome, char *parametros) {
    int type = returnType(tipo);
    struct Item *info;
    info = malloc(sizeof(struct Item));
    info->type = type;
    info->tamanhoX = 0;
    info->tamanhoY = 0;
    info->parametros = NULL;
    
    char escopo_ident[3] = ".N\0"; 
    char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*)));
    sprintf(escopo,"%s%d",escopo_ident,escopo_order); 
    
    char *key = malloc(sizeof(nome) + sizeof(escopo) + sizeof(char*)); 
    sprintf(key,"%s%s",nome,escopo); 
    
    add_item(key, info);
}

int returnType(char *type) {
    if(strcmp(type, "int") == 0) {
            return 1;
    } else if(strcmp(type, "char") == 0) {
            return 2;
    } else if(strcmp(type, "string") == 0) {
            return 3;
    } else if(strcmp(type, "float") == 0) {
            return 4;
    } else if(strcmp(type, "void") == 0) {
            return 5;
    }
    
    fprintf (stderr, "ERROR: Tipo informado (%s) não existe. Linha %d\n", type, yylineno);
    exit(0);
}

char* returnTypeString(int type) {
    if(type == 1) {
        char *retorno = newString(sizeof(char*) * 4);
        retorno = "int";
        return retorno;
    } else if(type == 2) {
        return "char";
    } else if(type == 3) {
        return "string";
    } else if(type == 4) {
        return "float";
    }
    
    fprintf (stderr, "ERROR: Um erro inesperado ocorreu. Linha %d\n", yylineno);
    exit(0);
}

void checkMain(char *tipo, char *nome){
    if(strcmp(nome, "main") == 0){
            if(strcmp(tipo, "int") != 0){
                    fprintf (stderr, "ERROR: O subprograma main deve retornar int.\n");
                    exit(0);
            }
    
            if(hasMain == 0){
                    hasMain = 1;
            }else{
                    fprintf (stderr, "ERROR: Programa deve conter apenas um subprograma com o nome MAIN.\n");
                    exit(0);
            }
    }
}

int checkRegex(const char *string, char *pattern) {
    int    status;
    regex_t    re;

    if (regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB) != 0) {
        return 0;      /* Report error. */
    }
    status = regexec(&re, string, (size_t) 0, NULL, 0);
    regfree(&re);
    if (status != 0) {
        return 0;      /* Report error. */
    }
    return 1;
}

char *montarWhile(char *expression, char *statements) {
    char *string = newString(sizeof(expression) + sizeof(statements) + (sizeof(char*) * 30));
    sprintf(string, "inicio%d:\n", label_order);
    sprintf(string, "%sif(%s){\n%s", string, expression, statements);
    sprintf(string, "%s\ngoto inicio%d;\n}", string, label_order);
    label_order++;
    
    return string;
}

char *montarFor(char *atribuicao, char *teste, char *incremento, char *statements) {
    char *string = newString(sizeof(atribuicao) + sizeof(teste) + sizeof(incremento) + sizeof(statements) + (sizeof(char*) * 20));
    sprintf(string, "%s\n", atribuicao);
    sprintf(string, "%sinicio%d:\n", string, label_order);
    sprintf(string, "%sif(%s){\n", string, teste);
    sprintf(string, "%s%s\n", string, statements);
    sprintf(string, "%s%s;\n", string, incremento);
    sprintf(string, "%sgoto inicio%d;\n}", string, label_order);
    label_order++;
    
    return string;
}

char *montarAttrString(char *id, char *attr, int comDeclaracao) {
    char *string = newString((4 * sizeof(id)) + (2 * sizeof(attr)) + (sizeof(char*) * 40));
    if(comDeclaracao) {  // Se o tipo não for vazio
        sprintf(string, "char *");
        sprintf(string, "%s%s = malloc(sizeof(%s));\n", string, id, attr);
    } else {
        sprintf(string, "if(%s != NULL) if(realloc(%s, sizeof(%s)) == NULL) { printf(\"Ocorreu um erro inesperado\"); exit(0); }\n", id, id, attr);
        sprintf(string, "%selse %s = malloc(sizeof(%s));\n", string, id, attr);
    }
    
    sprintf(string, "%ssprintf(%s, \"%%s\", %s);", string, id, attr);
    
    return string;
}

void checkAcessoVetor(int tamanho, int posicao) {
    if(tamanho >= 0 && posicao >= 0) {
        if(posicao >= tamanho) {
            printf ("WARNING: Você está tentando acessar uma posição inexistente do vetor na linha %d.\n", yylineno);
        }
    }
}

void checkAcessoMatriz(int tamanhoX, int tamanhoY, int posicaoX, int posicaoY) {
    if(tamanhoX >= 0 && posicaoX >= 0) {
        if(posicaoX >= tamanhoX) {
            printf ("WARNING: Você está tentando acessar uma posição inexistente da matriz na linha %d.\n", yylineno);
        }
    }
    if(tamanhoY >= 0 && posicaoY >= 0) {
        if(posicaoY >= tamanhoY) {
            printf ("WARNING: Você está tentando acessar uma posição inexistente da matriz na linha %d.\n", yylineno);
        }
    }
}

char *montarPrint(char *id, int type) {
    char *string = newString(sizeof(id) + (sizeof(char*) * 10));
    char *tipo = newString(sizeof(char));
    if(type == 1) {
        sprintf(tipo, "d");
    } else if(type == 2) {
        sprintf(tipo, "c");
    } else if(type == 3) {
        sprintf(tipo, "s");
    } else if(type == 4) {
        sprintf(tipo, "f");
    }
    sprintf(string, "printf(\"%%%s\", %s);", tipo, id);
    
    return string;
}

char *montarRead(char *id, int type) {
    char *string = newString(sizeof(id) + (sizeof(char*) * 10));
    char *tipo = newString(sizeof(char));
    if(type == 1) {
        sprintf(tipo, "d");
    } else if(type == 2) {
        sprintf(tipo, "c");
    } else if(type == 3) {
        sprintf(tipo, "s");
        sprintf(string, "scanf(\"%%%s\", %s);", tipo, id);
        return string;
    } else if(type == 4) {
        sprintf(tipo, "f");
    }
    sprintf(string, "scanf(\"\\n%%%s\", &%s);", tipo, id);
    
    return string;
}

char *montarPrintString(char *texto) {
    char *string = newString(sizeof(texto) + (sizeof(char*) * 10));
    sprintf(string, "printf(%s);", texto);
    
    return string;
}

void debugInicio(char *message) {
    if(debug) {
        printf("%s iniciado com sucesso (%d)\n", message, yylineno);
    }
}

void debugFim(char *message) {
    if(debug) {
        printf("%s reduzido com sucesso (%d)\n", message, yylineno);
    }
}

void debugLinha() {
    if(debug) {
        printf("%d\n", yylineno);
    }
}

void debugMessage(char *message) {
    if(debug) {
        printf("%s\n", message);
    }
}