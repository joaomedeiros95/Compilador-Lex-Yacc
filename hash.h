#include "uthash.h"
#include "string.h"
#include <stdio.h>
#include <stdlib.h>

#define SIZE 100
#define STRING_SIZE 50

struct DataItem {
   char key[STRING_SIZE];
   struct Item *data;   
   UT_hash_handle hh;
};

struct Item {
    int type;
    int tamanhoX;   //Quantidade de elementos no eixo x do vetor/matriz;
    int tamanhoY;   //Quantidade de elementos no eixo y do vetor/matriz;
    struct DataItem *parametros;
};

struct DataItem *itens = NULL;

void add_item(char* chave, struct Item *dados) {
   struct DataItem *item;
   
   HASH_FIND_STR(itens, chave, item);
   
   if(item == NULL) {
       item = malloc(sizeof(struct DataItem));
       strcpy(item->key, chave);
       item->data = dados;
      
       HASH_ADD_STR(itens, key, item);   /*nome do campo da key*/
   } else {
       char delimiter[2] = ".N";
       
       char *name = strsep(&chave, delimiter);
       char *escopo = chave;
       fprintf (stderr, "ERROR: Variável \"%s\" presente no escopo %s já declarada!\n", name, escopo);
       exit(0);
   }
}

struct DataItem *find_item(char* nome, int escopo_o) {
   struct DataItem *item;
   char escopo_ident[3] = ".N\0";
   
   while(escopo_o >= 0) {
        char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*))); 
        sprintf(escopo, "%s", escopo_ident); 
        sprintf(escopo,"%s%d",escopo,escopo_o); 
        
        char *key = malloc(sizeof(nome) + sizeof(escopo) + sizeof(char*)); 
        sprintf(key, "%s", nome); 
        sprintf(key, "%s%s", key, escopo); 
       
        HASH_FIND_STR(itens, key, item);
       
        if(item != NULL) {
            return item;
        }
       
        escopo_o--;
   }
   
   return NULL;
}

void delete_item(struct DataItem *item) {
    HASH_DEL(itens, item);
    free(item->data->parametros);
    free(item->data);
    free(item);
}

void delete_escopo(int escopo_o) {
    char escopo_ident[3] = ".N\0";
    char *escopo = malloc(sizeof(escopo_ident) + (2 * sizeof(char*))); 
    sprintf(escopo, "%s", escopo_ident); 
    sprintf(escopo,"%s%d",escopo,escopo_o);
    
    struct DataItem *current_item, *tmp;

    HASH_ITER(hh, itens, current_item, tmp) {
        if(strstr(current_item->key, escopo) != NULL)  {
            // printf("Deveria ter deletado %s... TODO: Implementar\n", current_item->key);
            delete_item(current_item); 
        }
    }
}

void delete_all() {
  struct DataItem *current_item, *tmp;

  HASH_ITER(hh, itens, current_item, tmp) {
    delete_item(current_item); 
  }
}

void print_itens() {
    struct DataItem *s;

    for(s=itens; s != NULL; s=(struct DataItem*)(s->hh.next)) {
        printf("chave: %s\t", s->key);
        if(s->data != NULL) {
           printf("tipo: %d\t", s->data->type);
           printf("tamanhoX: %d\t", s->data->tamanhoX);
           printf("tamanhoY: %d\t", s->data->tamanhoY);
           printf("parametros:");
        }
        printf("\n");
    }
}