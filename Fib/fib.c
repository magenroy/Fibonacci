#include <stdio.h>
#include "io.h"

int main(int argc, char *argv[]){
    unsigned long n = getNum(argc,argv);
    
    unsigned long long a,b;
    a = 0ULL;
    b = 1ULL;
    unsigned int i;
    for (i = 0U; i < n; i++){
	b = b + a;
	a = b - a;
    }

    printf("%llu\n",a);

    return 0;
}
