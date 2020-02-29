ATSHOMEQ=$(PATSHOME)
export PATSRELOCROOT=$(HOME)/ATS
ATSCC=$(ATSHOMEQ)/bin/patscc
ATSOPT=$(ATSHOMEQ)/bin/patsopt
ATSCCFLAGS=-DATS_MEMALLOC_LIBC -D_DEFAULT_SOURCE -g -IATS node_modules
LIBS=
ifdef ATSLIB
	LIBS := -L $(PATSHOME)/ccomp/atslib/lib -latslib
endif
ifdef PTHREAD
	LIBS := -lpthread
endif
APP     = main
EXEDIR  = target
TESTDIR = test
LIBDIR  = lib
LIB     = libatsargs.so
ARCHIVE = libatsargs.a
SRCDIR  = src
OBJDIR  = .build
vpath %.dats src
vpath %.dats src/DATS
vpath %.sats src/SATS
dir_guard=@mkdir -p $(@D)
SRCS    := $(shell find $(SRCDIR) -name '*.dats' -type f -exec basename {} \;)
OBJS    := $(patsubst %.dats,$(OBJDIR)/%.o,$(SRCS))
.PHONY: clean setup
all: lib archive
lib: $(LIBDIR)/$(LIB)
archive: $(LIBDIR)/$(ARCHIVE)
$(LIBDIR)/$(LIB): $(OBJS) 
	$(dir_guard)
	$(ATSCC) $(ATSCCFLAGS) -shared -o $@ $(OBJS) $(LIBS)
$(LIBDIR)/$(ARCHIVE): $(OBJS)
	$(dir_guard)
	ar -cvq $@ $(OBJS)
.SECONDEXPANSION:
$(OBJDIR)/%.o: %.dats $$(wildcard src/SATS/$$*.sats) node_modules/ats-result
	$(dir_guard)
	$(ATSCC) $(ATSCCFLAGS) -fpic -c $< -o $(OBJDIR)/$(@F) -cleanaft
RMF=rm -f
clean: 
	$(RMF) $(EXEDIR)/$(APP)
	$(RMF) $(LIBDIR)/$(LIB)
	$(RMF) $(LIBDIR)/$(ARCHIVE)
	$(RMF) $(OBJS)
	$(RMF) main
run: $(EXEDIR)/$(APP)
	./$(EXEDIR)/$(APP)
test: $(EXEDIR)/$(APP)
$(EXEDIR)/%: $(TESTDIR)/%.dats $(LIBDIR)/$(ARCHIVE)
	$(dir_guard)
	PATSRELOCROOT=$(PWD) $(ATSCC) $(ATSCCFLAGS) -o $@ $< -cleanaft $(LIBDIR)/$(ARCHIVE) ./node_modules/ats-result/lib/libatsresult.a
	$(EXEDIR)/$(APP)
installdeps: node_modules
node_modules: node_modules/ats-result
node_modules/ats-result:
	$(dir_guard)
	git clone http://ubuntu-netfu:3000/randy.valis/ats-result node_modules/ats-result
	make -C node_modules/ats-result
.SILENT: run 
