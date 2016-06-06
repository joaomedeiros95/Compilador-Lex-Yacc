%{
    #include <stdio.h>
    #include <string.h>
    #include <mcheck.h>
    #include "util.h"
    int yylex(void);
    int yyerror(char *s);
    
    int metodotipo = 0;
    
    extern char * yytext;
    extern FILE * yyin;
    
%}

%union {
    int    iValue;  /* integer value */
    char   cValue;  /* char value */
    char * sValue;  /* string value */
    float  fValue;  /* float value */
    struct TipoCompleto* tipoCompleto;
}

%token<sValue> CONST  TYPE  IF  ELSIF  ELSE  FOR  WHILE  SWITCH  CASE  DEFAULT  STOP  RETURN  PRINT  READ  FREE BUILD
%token<sValue> ALLOC  ID   STRING  OP_DIV  OP_TIMES  OP_MINUS  OP_PLUS  LEFT_PAREN  RIGHT_PAREN  LEFT_BRACKET  INC_OP  DEC_OP
%token<iValue> NUM  
%token<fValue> REAL
%token<sValue> ADD_ASSIGN  SUB_ASSIGN  MUL_ASSIGN  DIV_ASSIGN
%token<sValue> RIGHT_BRACKET  LEFT_BRACE  RIGHT_BRACE  SEMICOLON  COMMA  OP_REST  OP_AND  OP_OR  OP_NOT  OP_NOT_EQUAL  OP_ATR  OP_EQUAL  OP_LESS  OP_LESS_EQUAL
%token<sValue> OP_GREATHER  OP_GREATHER_EQUAL  COLON
%left OP_PLUS OP_MINUS
%left OP_TIMES OP_DIV OP_REST
%left OP_EQUAL OP_LESS_EQUAL OP_GREATHER_EQUAL ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN
%left INC_OP DEC_OP OP_GREATHER OP_LESS
%start program

%type <tipoCompleto> statement expression statementlist program subprogramList subprogram paramList param argList block literalList
%type <tipoCompleto> varDeclar funcCall return attr literalid expr_com_attr print idnum arrayaccess read

%%

program:
        subprogramList                          {}
        ;

subprogramList:
        subprogram                      { $$ = $1; }
        | subprogram subprogramList     { $$ = $1; }
        ;

subprogram:
        TYPE ID LEFT_PAREN RIGHT_PAREN {metodotipo = returnType($1); makeFunc($1, $2, NULL);} block    {
                                                        debugInicio("subprogram");
                                                        checkMain($1, $2);
                                                        if(strcmp($1, "void")){checkRetorno($2, $6->qtdRetornos);}
                                                        char *tmp = newString(sizeof($1) + sizeof($2) + sizeof($3) + sizeof($4) + sizeof($6->string) + 5 * sizeof(char*)); 
                                                        sprintf(tmp, "%s %s %s %s %s", $1, $2, $3, $4, $6->string); 
                                                        fputs(tmp, cFile); 
                                                }
        | TYPE ID LEFT_PAREN {metodotipo = returnType($1); makeFunc($1, $2, NULL);} paramList RIGHT_PAREN block { 
                                                                                                $$ = newTipoCompleto();
                                                                                                checkMain($1, $2);
                                                                                                if(strcmp($1, "void")){checkRetorno($2, $7->qtdRetornos);}
                                                                                                char *tmp = newString(sizeof($1) + sizeof($2) + sizeof($5->string) + sizeof($7->string) + 5 * sizeof(char*)); 
                                                                                                sprintf(tmp, "%s %s (%s)%s", $1, $2, $5->string, $7->string);
                                                                                                fputs(tmp, cFile); 
                                                                                        }
        ;

paramList:
        param   { $$ = $1; }
        | param COMMA paramList {
                                        $$ = newTipoCompleto();
                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*));
                                        sprintf($$->string, "%s,%s", $1->string, $3->string);
                                }       
        ;

param:
        TYPE ID {
                        makeParam($1, $2);
                        $$ = newTipoCompleto();
                        $$->string = newString(sizeof($1) + sizeof($2) + sizeof(char*));
                        sprintf($$->string, "%s %s", $1, $2);
                }
        ;

argList:
        ID                      {
                                        debugInicio("argList");
                                        $$ = newTipoCompleto();
                                        $$->string = newString(sizeof($1)); 
                                        sprintf($$->string, "%s", $1);
                                        debugFim("argList");
                                }
        | ID COMMA argList      {
                                        $$ = newTipoCompleto();
                                        $$->string = newString(sizeof($1) + sizeof($3->string) + sizeof(char*)); 
                                        sprintf($$->string, "%s,%s", $1, $3->string); 
                                }     
        ;       

block:
        LEFT_BRACE {escopo_order++;} statementlist RIGHT_BRACE  {
                                                                        debugInicio("block");
                                                                        delete_escopo(escopo_order);
                                                                        escopo_order--;
                                                                        $$ = newTipoCompleto();
                                                                        $$->string = newString(sizeof($3->string) + 4 * sizeof(char*));
                                                                        $$->qtdRetornos = $3->qtdRetornos;
                                                                        sprintf($$->string, "{\n%s\n}", $3->string); 
                                                                        debugFim("block");
                                                                }
        | LEFT_BRACE {escopo_order++;} RIGHT_BRACE { delete_escopo(escopo_order); escopo_order--; }
        ;


statementlist:
        statement       { $$ = $1; }
        | statement statementlist       { 
                                                debugInicio("statementlist");
                                                $$ = newTipoCompleto();
                                                $$->string = newString(sizeof($1->string) + sizeof($2->string) + sizeof(char*));
                                                $$->qtdRetornos = $1->qtdRetornos + $2->qtdRetornos;
                                                sprintf($$->string, "%s\n%s", $1->string, $2->string);
                                                debugFim("statementlist");
                                        }
        ;
        
attr: 
        ID OP_ATR expr_com_attr SEMICOLON       { 
                                                        debugInicio("attr");
                                                        verifyDeclaration($1); 
                                                        checktype(getVarType($1), $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        if(getVarType($1) == 3) {
                                                                $$->string = montarAttrString($1, $3->string, 0);
                                                        } else {
                                                                $$->string = newString(sizeof($1) + sizeof($3->string) + 2 * sizeof(char*));
                                                                sprintf($$->string, "%s=%s;", $1, $3->string); 
                                                        }
                                                        debugFim("attr");
                                                }
        | arrayaccess OP_ATR expr_com_attr SEMICOLON       { 
                                                        debugInicio("attr");
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        if($1->tipo == 3) {
                                                                $$->string = montarAttrString($1->string, $3->string, 0);
                                                        } else {
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + 2 * sizeof(char*));
                                                                sprintf($$->string, "%s=%s;", $1->string, $3->string); 
                                                        }
                                                        debugFim("attr");
                                                }
        ;

statement:
        expression SEMICOLON    {
                                        $$ = newTipoCompleto();
                                        $$->string = newString(sizeof($1->string) + sizeof(char*));
                                        sprintf($$->string, "%s;", $1->string); 
                                }
        | varDeclar     {$$ = $1;}
        | attr  {$$ = $1;}
        | FOR LEFT_PAREN attr expression SEMICOLON expression RIGHT_PAREN block {
                                                                                        $$ = newTipoCompleto();
                                                                                        $$->string = montarFor($3->string, $4->string, $6->string, $8->string);
                                                                                }
        | WHILE LEFT_PAREN expression RIGHT_PAREN block {
                                                                $$ = newTipoCompleto();
                                                                $$->string = montarWhile($3->string, $5->string);
                                                        }
        | IF LEFT_PAREN expression RIGHT_PAREN block    {
                                                                $$ = newTipoCompleto();
                                                                $$->string = newString(sizeof($1) + sizeof($3->string) + sizeof($5->string) + 3 * sizeof(char*));
                                                                $$->qtdRetornos = $5->qtdRetornos;
                                                                sprintf($$->string, "%s(%s) %s", $1, $3->string, $5->string); 
                                                        }
        | IF LEFT_PAREN expression RIGHT_PAREN block ELSE block {
                                                                        $$ = newTipoCompleto();  
                                                                        $$->string = newString(sizeof($1) + sizeof($3->string) + sizeof($5->string) + sizeof($6) + sizeof($7->string) + 6 * sizeof(char*));
                                                                        if($5->qtdRetornos >= 1 && $7->qtdRetornos >= 1) {
                                                                                $$->qtdRetornos = MIN($5->qtdRetornos, $7->qtdRetornos);
                                                                        } else {
                                                                                $$->qtdRetornos = 0;
                                                                        }
                                                                        sprintf($$->string, "%s(%s) %s %s %s", $1, $3->string, $5->string, $6, $7->string); 
                                                                }
        | SWITCH LEFT_PAREN expr_com_attr RIGHT_PAREN LEFT_BRACE cases RIGHT_BRACE { yyerror("O switch não foi implementado, desculpe!"); }
        | print         { $$ = $1; }
        | read          { $$ = $1; }
        | return        { $$ = $1; }
        ;
        
print:
        PRINT LEFT_PAREN STRING RIGHT_PAREN SEMICOLON   {
                                                                $$ = newTipoCompleto();
                                                                $$->string = montarPrintString($3); 
                                                        }
        | PRINT LEFT_PAREN ID RIGHT_PAREN SEMICOLON     {
                                                                verifyDeclaration($3); 
                                                                $$ = newTipoCompleto();
                                                                $$->string = montarPrint($3, getVarType($3)); 
                                                        }
        | PRINT LEFT_PAREN arrayaccess RIGHT_PAREN SEMICOLON     {
                                                                $$ = newTipoCompleto();
                                                                $$->string = montarPrint($3->string, $3->tipo); 
                                                        }                                                        
        ;
        
read:
        READ LEFT_PAREN ID RIGHT_PAREN SEMICOLON        {
                                                                $$ = newTipoCompleto();
                                                                $$->string = montarRead($3, getVarType($3));
                                                        }
        ;
        
return:
        RETURN expr_com_attr SEMICOLON  {
                                                debugInicio("return");
                                                checkRetornoTipo(metodotipo, $2->tipo);
                                                $$ = newTipoCompleto();
                                                $$->string = newString(sizeof($1) + sizeof($2->string) + 2 * sizeof(char*));
                                                $$->qtdRetornos = 1;
                                                sprintf($$->string, "%s %s;", $1, $2->string); 
                                                debugFim("return");
                                        }
        ;

cases:
        CASE NUM COLON statement STOP SEMICOLON cases     { }
        | DEFAULT COLON statement                         { }
        ;
        
expr_com_attr:
        literalid                                             { $$ = $1; }
        | arrayaccess                                         { $$ = $1; }
        | expression                                          { $$ = $1; }
        ;

expression:                                           
        expr_com_attr OP_PLUS expr_com_attr     { 
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string, "%s+%s", $1->string, $3->string);
                                                }
        | expr_com_attr OP_MINUS expr_com_attr  { 
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string, "%s-%s", $1->string, $3->string); 
                                                }
        | expr_com_attr OP_TIMES expr_com_attr  { 
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string , "%s*%s", $1->string, $3->string); 
                                                }
        | expr_com_attr OP_DIV expr_com_attr    { 
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*));  
                                                        sprintf($$->string , "%s/%s", $1->string, $3->string); 
                                                }
        | expr_com_attr OP_REST expr_com_attr   { 
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string, "%s%%%s", $1->string, $3->string); 
                                                }
        | expr_com_attr OP_EQUAL expr_com_attr  {
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string, "%s==%s", $1->string, $3->string); 
                                                }
        | expr_com_attr OP_LESS expr_com_attr   {
                                                        checktype($1->tipo, $3->tipo); 
                                                        $$ = newTipoCompleto();
                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                        sprintf($$->string, "%s<%s", $1->string, $3->string);   
                                                }
        | expr_com_attr OP_LESS_EQUAL expr_com_attr     {
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s<=%s", $1->string, $3->string);                 
                                                        }
        | expr_com_attr OP_GREATHER expr_com_attr       { 
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s>%s", $1->string, $3->string);   
                                                        }
        | expr_com_attr OP_GREATHER_EQUAL expr_com_attr         { 
                                                                        checktype($1->tipo, $3->tipo); 
                                                                        $$ = newTipoCompleto();
                                                                        $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                        sprintf($$->string, "%s>=%s", $1->string, $3->string);   
                                                                }
        | expr_com_attr ADD_ASSIGN expr_com_attr        { 
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s+=%s", $1->string, $3->string);   
                                                        }
        | expr_com_attr SUB_ASSIGN expr_com_attr        { 
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s-=%s", $1->string, $3->string);   
                                                        }
        | expr_com_attr MUL_ASSIGN expr_com_attr        { 
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo);
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s*=%s", $1->string, $3->string);   
                                                        }
        | expr_com_attr DIV_ASSIGN expr_com_attr        { 
                                                                checktype($1->tipo, $3->tipo); 
                                                                $$ = newTipoCompleto();
                                                                $$->tipo = expressionType($1->tipo, $3->tipo); 
                                                                $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*)); 
                                                                sprintf($$->string, "%s/=%s", $1->string, $3->string);   
                                                        }
        | expr_com_attr INC_OP                                  { 
                                                                        $$ = newTipoCompleto();
                                                                        $$->tipo = $1->tipo;
                                                                        $$->string = newString(sizeof($1->string) + sizeof(char*));
                                                                        sprintf($$->string, "%s++", $1->string);
                                                                }
        | expr_com_attr DEC_OP                                  { 
                                                                        $$ = newTipoCompleto();
                                                                        $$->tipo = $1->tipo; 
                                                                        $$->string = newString(sizeof($1->string) + sizeof(char*));
                                                                        sprintf($$->string, "%s--", $1->string);
                                                                }
        | LEFT_PAREN expr_com_attr RIGHT_PAREN                  { }
        | funcCall { $$ = $1; }
        ;
        
arrayaccess:
        ID LEFT_BRACKET idnum RIGHT_BRACKET {
                                                checkAcessoVetor(getArrayX($1), $3->iValor);
                                                $$ = newTipoCompleto();
                                                $$->tipo = getVarType($1);
                                                $$->string = newString(sizeof($1) + sizeof($3->string) + 3*sizeof(char*));
                                                sprintf($$->string, "%s[%s]", $1, $3->string);
                                        }
        | ID LEFT_BRACKET idnum RIGHT_BRACKET LEFT_BRACKET idnum RIGHT_BRACKET  {
                                                                                checkAcessoMatriz(getArrayX($1), getArrayY($1), $3->iValor, $6->iValor);
                                                                                $$ = newTipoCompleto();
                                                                                $$->tipo = getVarType($1);
                                                                                $$->string = newString(sizeof($1) + sizeof($3->string) + sizeof($6->string) + 6*sizeof(char*));
                                                                                sprintf($$->string, "%s[%s][%s]", $1, $3->string, $6->string);
                                                                        }
        ;
        
literalid: 
        idnum   { $$ = $1; }
        | STRING        { 
                                $$ = newTipoCompleto();
                                $$->tipo = 3; 
                                $$->sValor = $1; 
                                $$->string = newString(sizeof($1));
                                sprintf($$->string, "%s", $1);
                        }
        | REAL          { 
                                $$ = newTipoCompleto();
                                $$->tipo = 4; 
                                $$->fValor = $1; 
                                $$->string = newString(sizeof($1));
                                sprintf($$->string, "%f", $1);
                        }
        ;
        
idnum:
        NUM             { 
                                $$ = newTipoCompleto();
                                $$->tipo = 1; 
                                $$->iValor = $1;
                                $$->string = newString(sizeof($1));
                                sprintf($$->string, "%d", $1);
                        }
        | ID            { 
                                verifyDeclaration($1); 
                                $$ = newTipoCompleto();
                                $$->tipo = getVarType($1); 
                                $$->string = newString(sizeof($1));
                                sprintf($$->string, "%s", $1); 
                        }
        ;
                        
        
literalList:
        literalid { $$ = $1; }
        | literalid COMMA literalList     {
                                        debugInicio("literalList");
                                        $$ = newTipoCompleto();
                                        $$->string = newString(sizeof($1->string) + sizeof($3->string) + sizeof(char*));
                                        sprintf($$->string, "%s,%s", $1->string, $3->string);
                                        debugFim("literalList");
                                }
        ;

varDeclar:
        TYPE argList SEMICOLON                  { 
                                                        makeVar($1, $2->string); 
                                                        $$ = newTipoCompleto();
                                                        $$->string = newString(sizeof($1) + sizeof($2->string) + 2*sizeof(char*)); 
                                                        
                                                        char *tipo = newString(sizeof($1));
                                                        if(returnType($1) == 3) {
                                                                sprintf(tipo, "char *");
                                                        } else {
                                                                sprintf(tipo, "%s", $1);
                                                        }
                                                        sprintf($$->string, "%s %s;", tipo, $2->string); 
                                                }
        | TYPE ID OP_ATR expr_com_attr SEMICOLON        { 
                                                                makeVar($1, $2); 
                                                                checktype(returnType($1), $4->tipo); 
                                                                $$ = newTipoCompleto();
                                                                if(returnType($1) == 3) {
                                                                        $$->string = montarAttrString($2, $4->string, 1);
                                                                } else {
                                                                        $$->string = newString(sizeof($1) + sizeof($2) + sizeof($4->string) + 5*sizeof(char*)); 
                                                                        sprintf($$->string, "%s %s = %s;", $1, $2, $4->string); 
                                                                }
                                                        }
        | TYPE ID LEFT_BRACKET idnum RIGHT_BRACKET SEMICOLON      {
                                                                        debugInicio("varDeclar");
                                                                        makeArray($1, $2, $4->iValor, -1);
                                                                        $$ = newTipoCompleto();
                                                                        $$->string = newString(sizeof($1) + sizeof($2) + sizeof($4->string) + 4*sizeof(char*)); 
                                                                        sprintf($$->string, "%s %s[%s];", $1, $2, $4->string);                         
                                                                        debugFim("varDeclar");
                                                                } /* Vetor */ 
        | TYPE ID LEFT_BRACKET idnum RIGHT_BRACKET OP_ATR LEFT_BRACE literalList RIGHT_BRACE SEMICOLON        {
                                                                                                                debugInicio("varDeclar");
                                                                                                                makeArray($1, $2, $4->iValor, -1);
                                                                                                                $$ = newTipoCompleto();
                                                                                                                $$->string = newString(sizeof($1) + sizeof($2) + sizeof($4->string) + sizeof($8->string) + 7*sizeof(char*)); 
                                                                                                                sprintf($$->string, "%s %s[%s]={%s};", $1, $2, $4->string, $8->string);
                                                                                                                debugFim("varDeclar");
                                                                                                        } /* Vetor constante */
        | TYPE ID LEFT_BRACKET idnum RIGHT_BRACKET LEFT_BRACKET idnum RIGHT_BRACKET SEMICOLON       {
                                                                                                        debugInicio("varDeclar");
                                                                                                        makeArray($1, $2, $4->iValor, $7->iValor);
                                                                                                        $$ = newTipoCompleto();
                                                                                                        $$->string = newString(sizeof($1) + sizeof($2) + sizeof($4->string) + sizeof($7->string) + 6*sizeof(char*)); 
                                                                                                        sprintf($$->string, "%s %s[%s][%s];", $1, $2, $4->string, $7->string);
                                                                                                        debugFim("varDeclar");
                                                                                                } /* Matriz */
        | TYPE ID LEFT_BRACKET idnum RIGHT_BRACKET LEFT_BRACKET idnum RIGHT_BRACKET OP_ATR LEFT_BRACE literalList RIGHT_BRACE SEMICOLON         {
                                                                                                                                                debugInicio("varDeclar");
                                                                                                                                                makeArray($1, $2, $4->iValor, $7->iValor);
                                                                                                                                                $$ = newTipoCompleto();
                                                                                                                                                $$->string = newString(sizeof($1) + sizeof($2) + sizeof($4->string) + sizeof($7->string) + sizeof($11->string) + 8*sizeof(char*)); 
                                                                                                                                                sprintf($$->string, "%s %s[%s][%s]={%s};", $1, $2, $4->string, $7->string, $11->string);
                                                                                                                                                debugFim("varDeclar");
                                                                                                                                        } /* Matriz estática */
        | BUILD ID LEFT_BRACE varDeclarList RIGHT_BRACE SEMICOLON       {
                                                                        debugInicio("varDeclar");
                                                                        /* TODO: Implementar tipo criado pelo usuário */
                                                                        debugFim("varDeclar");
                                                                        }
        ;
        
varDeclarList:
        varDeclar
        | varDeclar varDeclarList {}
        ;

funcCall:
        ID LEFT_PAREN RIGHT_PAREN       { 
                                                $$ = newTipoCompleto();
                                                $$->string = newString(sizeof($1) + sizeof(char*)*2);
                                                $$->tipo = getVarType($1);
                                                sprintf($$->string, "%s()", $1);
                                        }
        | ID LEFT_PAREN argList RIGHT_PAREN     {
                                                        $$ = newTipoCompleto();
                                                        $$->string = newString(sizeof($1) + sizeof($3->string) + sizeof(char*)*2);
                                                        $$->tipo = getVarType($1);
                                                        sprintf($$->string, "%s(%s)", $1, $3->string);
                                                }
        ;

%%

int yyerror(char *s) {
    fprintf (stderr, "%d: %s at '%s'\n", yylineno, s, yytext);
    return 0;
}

int main(int argc, char *argv[]) {
        if(argc >= 3) {
                debug = atoi(argv[2]);
        }
        yyin = fopen(argv[1], "r");
        
        if(debug) {
                printf("======== UTILIZANDO DEBUG ========\n");
        }
        
        char *filename = malloc(sizeof(argv[1]) + 2 * sizeof(char*));
        sprintf(filename, "%s%s", argv[1], ".c");
        remove(filename);
        cFile = fopen(filename, "a");
        fputs("/*Parsed by MOFI Language - 2016\nMade by: João Eduardo Medeiros\nPedro Silva\nDanilo Damasceno\nAlison\nBrenda*/\n#include <stdio.h>\n#include<string.h>\n#include<stdlib.h>\n", cFile);
        
        yyparse();
        if(yywrap()){
                if(!hasMain){
                        fprintf (stderr, "ERROR: Programa deve conter um subprograma com o nome main.\n");
                }
                printf("Parsing completo!\n");
        } 
        
        fclose(yyin);
        fclose(cFile);
        return 0;
}
