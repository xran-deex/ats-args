
test: main

main: main.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o main main.dats 

clean:
	$(RM) main

.SILENT: clean