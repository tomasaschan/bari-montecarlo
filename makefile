FC = mpif77
FFLAGS=-Wall -g# -fcray-pointer
BINARIES = $(RUNNER) collisiontime onepart
RUNNER = histogram
CMD = ./$(RUNNER) < $(RUNNER).in# > $(RUNNER).out
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

install: all

all: $(BINARIES)

histogram: helpers.o onepart.o

run: $(RUNNER)
	mpirun -np 1 $(CMD)

runp: $(RUNNER)
	mpirun -np 8 $(CMD)
	
clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out *.png

memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) $(CMD)

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plot: 
	gnuplot $(RUNNER).gpt


collisiontime: helpers.o

runcoll: collisiontime
	./collisiontime > collisiontime.out

collplot:
	gnuplot collisiontime.gpt


list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto
