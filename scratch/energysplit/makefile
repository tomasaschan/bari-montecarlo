FC = mpif77
FFLAGS=-O3 -g
BINARIES = $(RUNNER) 
RUNNER = energysplit
CMD = ./$(RUNNER) < $(RUNNER).in > $(RUNNER).out
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

install: $(RUNNER)

all: $(BINARIES)

$(RUNNER): helpers.o 

run: $(RUNNER)
	./$(RUNNER) > $(RUNNER).out
	
clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

plot: 
#	gnuplot $(RUNNER).gpt
	gnuplot energysplit.gpt

showplot:
	eog energysplit.png 2> /dev/null &

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto