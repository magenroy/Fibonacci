all:
	(cd Fib; gcc -Ofast io.h fib.c -o ../fib)
	(cd Fib; gcc -Ofast io.h fibBig.c -lgmp -o ../fibBig)
	(cd GFib; ghc -O2 FibIO.hs -o ../gfib)
