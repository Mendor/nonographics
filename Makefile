all:
	gcc -Wall -fPIC -shared -o nono.so nono.c -I /usr/lib/erlang/usr/include
	erlc -o nono.beam nono.erl

clean:
	rm nono.beam nono.so test.o
