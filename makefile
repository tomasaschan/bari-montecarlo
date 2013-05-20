# Compiler options
FC 			:= 	mpif90
BINDIR		:=	bin
VPATH		:=	src
FFLAGS		:=	-O3 -g -Wall -Warray-bounds -ffixed-line-length-none -fbounds-check -J$(BINDIR) #-I$(BINDIR)

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

$(BINDIR)/%.mod: 

runner: $(OBJS)

# Running the program

run: runner | setid outdir
	@cp $(INFILE) $(OUTDIR)
	@echo -n "Running simulation..."
	@mpirun -n 1 $(CMD)
	@echo "done!"
	@cat $(OUTFILE) | grep -e \# | sed 's/\# //'

# Processing data
plot: ploteedfevolution plotratecoeffs plotratequotient plotpopulations

ploteedfevolution: getid
	gnuplot -e "srcdir='$(OUTDIR)'" scripts/plot-eedf-evolution.gp 

plotratecoeffs: getid
	gnuplot -e "srcdir='$(OUTDIR)'" scripts/plot-ratecoeffs.gp

plotratequotient: getid
	gnuplot -e "srcdir='$(OUTDIR)'" scripts/plot-ratequote.gp

plotpopulations: getid
	gnuplot -e "srcdir='$(OUTDIR)'" scripts/plot-populations.gp

plotcrossections:
	@mkdir -p out
	gnuplot scripts/plot-cross-sections.gp

showplots: getid
	eog "$(OUTDIR)/"*.png 2> /dev/null &

cherrypick: getid
	@echo -n "Cherry-picking data..."
	@scripts/cherry-pick-data.sh $(run_id)
	@echo "done!"

# Helpers
setid: FORCE
	$(eval run_id 	:= 	$(shell date '+%Y-%m-%d-%H-%M-%S'))
	$(eval OUTDIR 	:= 	out/$(run_id))
	$(eval OUTFILE 	:= 	$(OUTDIR)/simulation.out)
	$(eval CMD		:=	./runner < $(INFILE) > $(OUTFILE))
	@echo "Outdata in: $(OUTDIR)/"

getid: FORCE
	$(eval run_id := $(shell ls -t out | head -n 1))
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
	@rm -rf $(BINDIR) *.mod runner
	@echo "done!"

cleanout:
	@echo -n "Cleaning outdata..."
	@rm -rf out
	@echo "done!"

FORCE:
	@true