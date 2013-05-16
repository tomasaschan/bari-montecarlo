FC = mpif90
FWARNINGS = -Wall -Warray-bounds 
FOPTS = -ffixed-line-length-none -fbounds-check 
FFLAGS=-O0 -g $(FWARNINGS) $(FOPTS)
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

tstamp = $(shell date '+%Y-%m-%d-%H-%M-%S')
OUTDIR = outdata

RUNNER = run_simulation
TESTER = quicktest

BINARIES = $(RUNNER) $(TESTER) indatatester

INFILE = input.in
OUTFILE = $(OUTDIR)/$(RUNNER)_$(tstamp).out
CMDOUT =  > $(OUTFILE)
CMD = ./$(RUNNER) < $(INFILE) $(CMDOUT)

TESTOUT = > $(TESTER).out
TESTCMD = ./$(TESTER) < $(INFILE) $(TESTOUT)


MODULES = mpi.o io.o random.o interpolation.o physics.o eedf.o single_particle.o ratecoeffs.o populations.o
ALLMODULES = precision.o $(MODULES)

# Compile commands

install: $(RUNNER)

all: $(MODULES) $(BINARIES)

$(RUNNER): $(MODULES) runner.f
	$(FC) $(FFLAGS) -o $@ $^

# Dependencies

$(MODULES): precision.o

$(RUNNER), interpolation.o: io.o 

io.o, random.o: mpi.o

single_particle.o, interpolation.o: physics.o eedf.o

physics.o: random.o

quicktest: $(ALLMODULES)

indatatester: $(ALLMODULES)


# Miscellaneous helpers

clean:
	rm -f *~ .fuse_* *.o *.mod $(BINARIES) *.out
removealloutput:
	rm -rf $(OUTDIR)/*

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto

# Run

run: $(RUNNER)
	mpirun -np 1 $(CMD)
	grep $(OUTFILE) -e \#

runp: $(RUNNER)
	mpirun -np 4 $(CMD)
	grep $(OUTFILE) -e \#

runvp: $(RUNNER)
	mpirun -np $(NPROC) $(CMD)

runt: $(TESTER)
	mpirun -np 1 $(TESTCMD)

runtp: $(TESTER)
	mpirun -np 2 $(TESTCMD)

testind: indatatester
	./indatatester < $(INFILE)

# Debug

debug: $(RUNNER)
	gdb $(RUNNER)

debugp: $(RUNNER)
	mpirun -np 2 xterm -e gdb $(CMD) &

memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) $(CMD)

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

# Plot and show

plots: ploteedfevolution plotratecoeffs plotpopulations plotratecoeffsratios
	mkdir -p "plots/$(tstamp)"
	mv *.png "plots/$(tstamp)"

ploteedf:
	gnuplot plot-eedf.gp

ploteedfevolution:
	gnuplot plot-eedf-evolution.gp

plotratecoeffs:
	gnuplot plot-ratecoeffs.gp

plotratecoeffsratios:
	gnuplot plot-ratequote.gp

plotpopulations:
	gnuplot plot-populations.gp

showplots:
	eog "plots/`ls plots | tail -n 2 | head -n 1`/"*.png 2> /dev/null &

showmsgs:
	grep "$(OUTDIR)/`ls $(OUTDIR) | tail -n 1`" -e \#

showlastoutput:
	cat "$(OUTDIR)/`ls $(OUTDIR) | tail -n 1`"

sanitycheck:
	rm -rf sanitycheck.png sanity.out
	make showlastoutput | awk '/^eedf.*E\+04/' > sanity.out
	gnuplot plot-sanitycheck.gp
	eog sanitycheck.png
	