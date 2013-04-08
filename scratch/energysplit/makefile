FC = mpif77
FFLAGS=-O3 -g
BINARIES = $(RUNNER)
RUNNER = energysplit
CMD = ./$(RUNNER) > $(RUNNER).out

install: $(RUNNER)

all: $(BINARIES)

energysplit: helpers.o

run: $(RUNNER)
	mpirun -np 1 $(CMD)

clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

plot: 
	gnuplot $(RUNNER).gpt

showplots:
	eog *.png 2> /dev/null &

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto
