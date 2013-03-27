FC = mpif77 
FFLAGS=-Wall 
RUNNER = runner
INARGS = 
#< input.in

all: $(RUNNER)

run: all
	mpirun -np 8 $(RUNNER) $(INARGS)

runserial: all
	mpirun -np 1 $(RUNNER) $(INARGS)
	
clean:
	rm -f *~ *.o $(RUNNER)

cleanout: clean
	rm -f *.out

show:
	$(FC) $(FFLAGS) -show $(RUNNER).f -o $(RUNNER)

