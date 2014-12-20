#include <stdio.h>
#include <gmp.h>
#include "io.h"

int main(int argc, char *argv[]){
    mpz_t fn;
    mpz_init(fn);
    mpz_fib_ui(fn,getNum(argc,argv));
    gmp_printf("%Zd\n",fn);
    mpz_clear(fn);
    return 0;
}
