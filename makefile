FC = mpif77
FFLAGS=-O3 -g
BINARIES = $(RUNNER)
RUNNER = histogram
CMD = ./$(RUNNER) < $(RUNNER).in > $(RUNNER).out
VALGRINDOPTS = --suppressions=/usr/share/openmpi/openmpi-valgrind.supp --gen-suppressions=all

install: $(RUNNER)

all: $(BINARIES)

histogram: single_particle.o physics.o random.o handle_collision_e.o io.o

run: $(RUNNER)
	mpirun -np 1 $(CMD)

runp: $(RUNNER)
	mpirun -np 8 $(CMD)
	cat $(RUNNER).out | grep \#

runvp: $(RUNNER)
	mpirun -np $(NPROC) $(CMD)
	
clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

memcheck: $(RUNNER)
	valgrind $(VALGRINDOPTS) $(CMD)

memcheckp: $(RUNNER)
	mpirun -np 2 valgrind $(VALGRINDOPTS) $(CMD)

plot: 
#	gnuplot $(RUNNER).gpt
	./histogram-plot.sh histogram.out

showplots:
	eog *.png 2> /dev/null &

collisiontime: helpers.o

runcoll: collisiontime
	./collisiontime > collisiontime.out

collplot:
	gnuplot collisiontime.gpt


list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto

rotationtest: helpers.o

measurespeedup:
	
testrandomvN: helpers.o