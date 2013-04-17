FC = mpif90
FWARNINGS = -Wall -Warray-bounds 
FOPTS = -ffixed-line-length-none -fbounds-check 
FFLAGS=-O0 -g $(FWARNINGS) $(FOPTS)
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

tstamp = $(shell date '+%Y-%m-%d-%H-%M-%S')
OUTDIR = outdata

RUNNER = run_simulation
TESTER = 

BINARIES = $(RUNNER) $(TESTER)

INFILE = input.in
OUTFILE = $(OUTDIR)/$(RUNNER)_$(tstamp).out
CMDOUT =  > $(OUTFILE)
CMD = ./$(RUNNER) < $(INFILE) $(CMDOUT)

TESTOUT = > $(TESTER).out
TESTCMD = ./$(TESTER) < $(INFILE)

MODULES1 = physics.o single_particle.o random.o histogram.o
MODULES2 = io.o mpi.o interpolation.o 
MODULES = $(MODULES1) $(MODULES2)

# Compile commands

install: $(RUNNER)

all: $(MODULES) $(BINARIES)

$(RUNNER): $(MODULES) runner.f
	$(FC) $(FFLAGS) -o $@ $^

# Dependencies

interpolation.o: io.o 
#mpi.o

io.o: mpi.o


random.o: mpi.o

single_particle.o: physics.o histogram.o

handle_collision.o: random.o

physics.o: random.o interpolation.o

# Miscellaneous helpers

clean:
	rm -f *~ .fuse_* *.o *.mod $(BINARIES) *.out

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto

run: $(RUNNER)
	mpirun -np 1 $(CMD)
	grep $(OUTFILE) -e \#

runp: $(RUNNER)
	mpirun -np 8 $(CMD)
	grep $(OUTFILE) -e \#

runvp: $(RUNNER)
	mpirun -np $(NPROC) $(CMD)
	
memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) ./$(RUNNER) < $(RUNNER).in

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plotlast:
	./plot.sh "$(OUTDIR)/`ls outdata | tail -n 1`"
	
showplots:
	eog *.png 2> /dev/null &

