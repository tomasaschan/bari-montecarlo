FC = mpif77
FFLAGS=-Wall -g# -fcray-pointer
RUNNER = runner
INARGS = < input.in
OUTARGS = > output.out
CMD = ./$(RUNNER) $(INARGS) $(OUTARGS)
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp

$(RUNNER): distribution.o helpers.o

all: $(RUNNER)

run: all
	mpirun -np 1 $(CMD)

runp: all
	mpirun -np 8 $(CMD)
	
clean: cleanout
	rm -f *~ *.o .fuse_* $(RUNNER)

cleanout:
	rm -f *.out *.png

memcheck: all
	valgrind $(VALGRINDOPTS) $(CMD)

memcheckp: all
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plot:
	gnuplot plotting.gpt
