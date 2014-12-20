#include <stdio.h>

unsigned long int getNum(int argc, char *argv[]){
    unsigned long int n;
    if (argc - 1){
	n = atoi(argv[1]);
    }
    else {
	printf("n=");
	scanf("%u",&n);
    }
    return n;
}
