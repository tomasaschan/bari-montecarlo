FC = mpif77
FFLAGS=-O3 -g
BINARIES = savino savino2 savino3

clean: cleanout
	rm -f *~ *.o .fuse_* $(BINARIES)

cleanout:
	rm -f *.out

list:
	clear
	ls -l --sort=extension --group-directories-first --color=auto
