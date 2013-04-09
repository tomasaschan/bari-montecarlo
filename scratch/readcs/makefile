FC = mpif90
FFLAGS=-O0 -g
BINARIES = $(RUNNER)
RUNNER = tester
CMD = ./$(RUNNER) < $(RUNNER).in > $(RUNNER).out
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all --main-stacksize=20000000

install: $(RUNNER)

all: $(BINARIES)

tester: read_cross-section_data.o interpolate_cs_data.o

run: $(RUNNER)
	$(CMD)
	grep tester.out -e \#

memcheck: $(RUNNER)
	valgrind $(CMD)

plot:
	gnuplot interpolation.gpt

showplot:
	eog interpolation.png 2> /dev/null &

clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

