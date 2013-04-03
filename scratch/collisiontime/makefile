FC = mpif77
FFLAGS = -Wall -g
EXE = collissiontime2

$(EXE): helpers.o

run: $(EXE)
	./$(EXE) > $(EXE).out

plot:
	gnuplot $(EXE).gpt

clean: cleanout
	rm -f *~ *.o .fuse_* $(EXE)

cleanout:
	rm -f *.out *.png


