Reasearch project in Bari, spring 2013
===============

During March-May of 2013 I'm working on a research project in Bari, Italy, creating a Monte-Carlo simulation of electron beams in a collisional medium. This is the code.

The project will be developed in FORTRAN77 using OpenMPI for parallelization.

## Prerequisites

The program requires the following to install and run:

  * A Fortran compiler, e.g. [GNU Fortran](http://gcc.gnu.org/fortran/)
  * An implementation of the MPI Framework, e.g. [Open MPI](http://www.open-mpi.org/)
  * Make, e.g. [GNU Make](http://www.gnu.org/software/make/)

For the postprocessing scripts to function, the following is also required:

  * [gnuplot](http://www.gnuplot.info/)

## Installation and execution

### Installation

When all the prerequisites are in place, the program is installed by running

    $ make install

or simply

    $ make

This compiles all the code and creates an executable binary `histogram` in the project root. 

### Execution

To execute, run with

    $ mpirun -np <number of processes> ./histogram < histogram.in

The program writes to `stdout`. It is recommended to redirect the output to a file for processing - a few shortcuts to do so are available as `make` commands:

    $ make run

runs the program in serial mode, with only one process, and 

    $ make runp

runs the program in parallell mode, with 8 processes (a suitable configuration for most modern laptops with quadcore processors and virtualization to octocore). It is also possible to set the environment variable `NPROC` to any number of processes, and run the program via

    $ make runvp

### Program input

The program reads a number of parameters from `stdin` - the default in the makefile is to read them from the file `histogram.in`. They are, in order (one item per line):

  1. The number of primaries to simulate (real, e.g. 1e6)
  2. The time, in seconds, for which to run the simulation (real, e.g. 10e-9 for 10 ns)
  3. The time step, in seconds. Data is output for each time step (real, e.g. 1e-9 for data every nanosecond)
  4. The initial energy of the primaries, in electronvolts (real, e.g. 1e3 for 1keV)
  5. The data files from which the cross-sectional data is read.

## Postprocessing

The data is plotted in [gnuplot](http://www.gnuplot.info/), using the shellscript `histogram.sh` which takes as a command line argument the name of the data file to plot.

A shortcut for this is also available through

    $ make plot

and the plots can be shown via e.g. `eog *.png`, or conveniently

    $ make showplots