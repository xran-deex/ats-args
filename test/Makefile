
all: main main2

test: main

main: main.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o main main.dats 

main2: main2.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o main2 main2.dats 

clean:
	$(RM) main
	$(RM) main2

.SILENT: clean