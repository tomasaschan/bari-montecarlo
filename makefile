FC = mpif90
FWARNINGS = -Wall -Warray-bounds 
FOPTS = -ffixed-line-length-none -fbounds-check 
FFLAGS=-O3 -g $(FWARNINGS) $(FOPTS)
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

MODULES1 = precision.o mpi.o io.o random.o interpolation.o physics.o
MODULES2 = histogram.o single_particle.o
MODULES = $(MODULES1) $(MODULES2)

# Compile commands

install: $(RUNNER)

all: $(MODULES) $(BINARIES)

$(RUNNER): $(MODULES) runner.f
	$(FC) $(FFLAGS) -o $@ $^

# Dependencies

mpi.o, histogram.o, io.o, random.o, interpolation.o, physics.o, single_particle.o: precision.o

$(RUNNER), interpolation.o: io.o 

io.o, random.o: mpi.o

single_particle.o: physics.o histogram.o

# Miscellaneous helpers

clean:
	rm -f *~ .fuse_* *.o *.mod $(BINARIES) *.out
removealloutput:
	rm -f $(OUTDIR)/*

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto

run: $(RUNNER)
	mpirun -np 1 $(CMD)
	grep $(OUTFILE) -e \#

runp: $(RUNNER)
	mpirun -np 4 $(CMD)
	grep $(OUTFILE) -e \#

runvp: $(RUNNER)
	mpirun -np $(NPROC) $(CMD)

debug: $(RUNNER)
	mpirun -np 2 xterm -e gdb $(CMD) &

memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) $(CMD)

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plot:
	gnuplot plot.gpt

plotfences:
	./plot-fences.sh "$(OUTDIR)/`ls outdata | tail -n 1`"
	
showplots:
	eog *.png 2> /dev/null &

showmsgs:
	grep "$(OUTDIR)/`ls $(OUTDIR) | tail -n 1`" -e \#


quicktest: 

