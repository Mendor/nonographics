/*  
    HERE BE DRAGONS!

    Big thanks to LinAmp for co-writing.

    The only function of this NIF is generation of all potential line
    solutions based on its data. Exported function — nono:seqs/2
*/

#include <stdio.h>
#include <string.h>
#include "erl_nif.h"

void update_list(ErlNifEnv* env, unsigned int *nums, ERL_NIF_TERM tail, unsigned short int i);

unsigned long int maxlines;
char** buf = NULL;

char* current = NULL;

unsigned int k;

unsigned int blocks;
unsigned int line;
unsigned int* vars = NULL;

void do_dump(int char_seek, int i_seek)
{
    int i;
    if (i_seek == blocks)
    {
        for (i = char_seek; i <= line; i++)
            current[i] = '.';
        current[line] = 0;
        buf[k][0] = 0;
        strncpy(buf[k], current, line + 1);
        k++;
        // printf("char_seek=%d  i_seek=%d  k=%d  maxlines=%d  blocks=%d  line=%d  current=%s vars[i_seek-1]=%d\r\n",
        //     char_seek, i_seek, k, maxlines, blocks, line, current, vars[i_seek-1]);
        return;
    }
    if (line < (char_seek + vars[i_seek]))
        return;
    current[char_seek] = '.';
    do_dump(char_seek + 1, i_seek);
    for (i = char_seek; i < (char_seek + vars[i_seek]); i++)
        current[i] = 'X';
    current[i] = '.';
    do_dump(i + 1, i_seek + 1);
    return;
}

static ERL_NIF_TERM seqs_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int i;
    // clearance
    if (vars != NULL)
        free(vars);
    if (buf != NULL)
    {
        for (i = 0; i < blocks; i++)
            free(buf[i]);
        free(buf);
    }
    if (current != NULL)
        free(current);

    // read input parameters
    if (!enif_get_uint(env, argv[1], &line)) {
        return enif_make_badarg(env);
    }
    enif_get_list_length(env, argv[0], &blocks);
    vars = (unsigned int*)malloc(sizeof(unsigned int) * blocks);
    if (vars == NULL)
        printf("[ERR] vars allocation faled\n");
    ERL_NIF_TERM item;
    ERL_NIF_TERM list = argv[0];
    for (i = 0; i < blocks; i++) {
        if (!enif_get_list_cell(env, list, &item, &list)) {
            return enif_make_badarg(env);
        }
        enif_get_uint(env, item, &vars[i]);
    }

    int c;
    // combinations counting
    c = line + 1;
    maxlines = 1;
    k = 0;
    for (i = 0; i < blocks; i++) c -= vars[i];
    for (i = 1;i < (c - blocks + 1); i++) maxlines = maxlines * (c - i + 1) / i;
    // array allocation
    buf = (char**)malloc(sizeof(char*) * maxlines);
    for (i = 0; i < maxlines; i++)
        buf[i] = (char*)malloc(sizeof(char) * (line + 2));
    // go, go, go
    current = (char*)malloc(sizeof(char) * (line + 2));
    do_dump(0, 0);

    ERL_NIF_TERM estrings[maxlines];
    for (i = 0; i < maxlines; i++) {
        estrings[i] = enif_make_string(env, buf[i], ERL_NIF_LATIN1);
    }
    return enif_make_list_from_array(env, estrings, maxlines);
}

static ErlNifFunc nono_funcs[] = {
    {"seqs", 2, seqs_nif}
};

ERL_NIF_INIT(nono,nono_funcs,NULL,NULL,NULL,NULL)
