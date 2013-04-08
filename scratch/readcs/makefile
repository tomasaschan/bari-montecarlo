FC = mpif90
FFLAGS=-O1 -g
BINARIES = $(RUNNER)
RUNNER = tester
CMD = ./$(RUNNER) < $(RUNNER).in > $(RUNNER).out
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all --main-stacksize=20000000

install: $(RUNNER)

all: $(BINARIES)

tester: read_cross-section_data.o

run: $(RUNNER)
	./tester

clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out
