
LIBS = -L ../target -lats-args

all: main main2 main3

test: main

main: main.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS) -cleanaft

main2: main2.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS) -cleanaft

main3: main3.dats
	patscc -IATS .. -IATS ../node_modules -DATS_MEMALLOC_LIBC -g -o $(@F) $< $(LIBS) -cleanaft

RMF = rm -f

clean:
	$(RMF) main
	$(RMF) main2
	$(RMF) main3

.SILENT: clean