FC = mpif77
FFLAGS=-O0 -g -Wall
BINARIES = $(RUNNER) cstest
RUNNER = histogram
tstamp = $(shell date '+%Y-%m-%d-%H-%M-%S')
OUTDIR = outdata
OUTFILE = $(OUTDIR)/$(RUNNER)_$(tstamp).out
CMD = ./$(RUNNER) < $(RUNNER).in > $(OUTFILE)
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

install: $(RUNNER)

all: $(BINARIES)

histogram: physics.o single_particle.o random.o handle_collision.o io.o mpi.o interpolation.o 

run: $(RUNNER)
	mpirun -np 1 $(CMD)

runp: $(RUNNER)
	mpirun -np 8 $(CMD); alert
	grep $(OUTFILE) -e \#

runvp: $(RUNNER)
	mpirun -np $(NPROC) $(CMD)
	
clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) ./$(RUNNER) < $(RUNNER).in

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plot:
	./histogram.sh "$(OUTDIR)/`ls outdata | tail -n 1`" "$(OUTDIR)/cstest.out"
	
showplots:
	eog *.png 2> /dev/null &

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto

cstest: io.o interpolation.o mpi.o physics.o random.o

runt: cstest
	mpirun -np 1 ./cstest < $(RUNNER).in > $(OUTDIR)/cstest.out

memcheckt: cstest
	valgrind $(VALGRINDOPTS) ./cstest < $(RUNNER).in