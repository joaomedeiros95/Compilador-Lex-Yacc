#!/bin/bash
yacc -d -v parser.y
flex lex.l 
cc -g lex.yy.c y.tab.c
./a.out programaTeste
gcc programaTeste.c -o pTeste
./pTeste