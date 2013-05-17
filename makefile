# Compiler options
FC 			:= 	mpif90
FFLAGS		:=	-O3 -g -Wall -Warray-bounds -ffixed-line-length-none -fbounds-check 
VPATH		:=	src
BINDIR		:=	bin

# Information about this run
INFILE 		:= 	input.in


# Options for memory checking
# VALGRINDOPTS = --gen-suppressions=all

# All modules
OBJS		:= 	$(BINDIR)/precision.o $(BINDIR)/mpi.o $(BINDIR)/io.o $(BINDIR)/random.o $(BINDIR)/physics.o $(BINDIR)/interpolation.o $(BINDIR)/eedf.o $(BINDIR)/ratecoeffs.o $(BINDIR)/single_particle.o $(BINDIR)/populations.o 

# Default rule
all: runner | $(BINDIR)

# Set some make specials
.SUFFIXES:
.SUFFIXES: .f .o .mod 

.PHONY: setid getid

# Build rules

$(BINDIR)/%.o: $(VPATH)/%.f | $(BINDIR)
	$(FC) $(FFLAGS) -c $^ -o $@

runner: $(OBJS)

# Running the program

run: runner | setid outdir
	@cp $(INFILE) $(OUTDIR)
	@echo -n "Running simulation..."
	@mpirun -n 1 $(CMD)
	@echo "done!"
	@cat $(OUTFILE) | grep -e \# | sed 's/\# //'

# Plotting data
plot: ploteedfevolution plotratecoeffs plotratequotient plotpopulations

ploteedfevolution: getid
	cd scripts; gnuplot -e "srcdir='$(OUTDIR)'" plot-eedf-evolution.gp 

plotratecoeffs: getid
	cd scripts; gnuplot -e "srcdir='$(OUTDIR)'" plot-ratecoeffs.gp

plotratequotient: getid
	cd scripts; gnuplot -e "srcdir='$(OUTDIR)'" plot-ratequote.gp

plotpopulations: getid
	cd scripts; gnuplot -e "srcdir='$(OUTDIR)'" plot-populations.gp

plotcrossections:
	gnuplot scripts/plot-cross-sections.gp

showplots: getid
	eog "$(OUTDIR)/"*.png 2> /dev/null &

# Helpers
setid: FORCE
	$(eval run_id 	:= 	$(shell date '+%Y-%m-%d-%H-%M-%S'))
	$(eval OUTDIR 	:= 	out/$(run_id))
	$(eval OUTFILE 	:= 	$(OUTDIR)/simulation.out)
	$(eval CMD		:=	./runner < $(INFILE) > $(OUTFILE))
	@echo "Outdata in: $(OUTDIR)/"

getid: FORCE
	$(eval run_id := $(shell ls out | tail -n 1))
	$(eval OUTDIR := out/$(run_id))
	$(eval OUTFILE 	:= 	$(OUTDIR)/simulation.out)
	$(eval CMD		:=	./runner < $(INFILE) > $(OUTFILE))
	@echo "Looking in $(OUTDIR)/"

outdir:
	@mkdir -p $(OUTDIR)

$(OUTDIR): getid
	mkdir -p $(OUTDIR)

$(BINDIR):
	@mkdir -p $(BINDIR)

clean:
	@echo -n "Cleaning..."
	@rm -rf $(BINDIR) *.mod
	@echo "done!"

cleanout:
	@echo -n "Cleaning outdata..."
	@rm -rf out
	@echo "done!"

FORCE:
	@true