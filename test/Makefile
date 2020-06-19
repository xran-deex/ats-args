
LIBS = -L ../target -lats-args

all: main main2 main3

test: main

main: main.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS)

main2: main2.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS)

main3: main3.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS)

clean:
	$(RM) main
	$(RM) main2
	$(RM) main3

.SILENT: clean