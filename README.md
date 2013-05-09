Reasearch project in Bari, spring 2013
===============

During March-May of 2013 I'm working on a research project in Bari, Italy, creating a Monte-Carlo simulation of electron beams in a collisional medium. This is the code.

The project will be developed in FORTRAN77 using OpenMPI for parallelization.

## Prerequisites

The program requires the following to install and run:

  * A Fortran compiler, e.g. [GNU Fortran](http://gcc.gnu.org/fortran/)
  * An implementation of the MPI Framework, e.g. [Open MPI](http://www.open-mpi.org/) or [MPICH](http://www.mpich.org/). 
  * Make, e.g. [GNU Make](http://www.gnu.org/software/make/)

For the postprocessing scripts to work, [gnuplot](http://www.gnuplot.info/) is also required.

To install these in Ubuntu, run `apt-get install gfortran mpich2 make gnuplot`

## Installation and execution

### Installation

When all the prerequisites are in place, the program is installed by running

    $ make install

or simply

    $ make

This compiles all the code and creates an executable binary `run_simulation` in the project root. 

### Execution

To execute, run with

    $ mpirun -np <number of processes> ./run_simulation < input.in

The program writes to `stdout`. It is recommended to redirect the output to a file for processing - a few shortcuts to do so are available as `make` commands:

    $ make run

runs the program in serial mode, with only one process, and 

    $ make runp

runs the program in parallell mode, with 8 processes (a suitable configuration for most modern laptops with quadcore processors and virtualization to octocore). Serial mode is **not** recommended for more than ~1000 runs.

It is also possible to set the environment variable `NPROC` to any number of processes, and run the program via

    $ make runvp

which is useful e.g. for benchmarking and performance review.
### Program input

The program reads a number of parameters from `stdin` - the default in the makefile is to read them from the file `input.in`. They are, in order (one list item per line):

  1. The number of primaries to simulate (real, e.g. `1e6`)

  1. The time, in seconds, for which to run the simulation (real, e.g. `10e-9` or `1e-8` for 10 ns)

  1. The time step, in seconds. Data is output for each time step (real, e.g. `1e-9` for data every nanosecond)

  1. The initial energy of the primaries, in electronvolts (real, e.g. `1e3` for 1keV, or `200` for 200 eV)

  1. The number of collisional processes to be considered, e.g. `2`. This input value determines how the rest of the input file is interpreted, so the number must agree with the number of file names, boundary energies etc.

  1. The data files from which data for the differential cross-sections is read. This must be exactly three (3) single-quote enclosed strings, indicating relative paths to the data files, in the following order (indicating the collision product): N2+, N2+(b) and N2(c). For example, `'N2+.dat' 'N2+B.dat' 'N2C.dat'`.

  The first line of the data file should contain three whitespace-separated values. The first two the lower and upper boundaries of the energy range for which this collisional process should be considered - the cross-section will be considered 0 outside this range. The third value is a 1 if the process spawns a secondary electron, and 0 otherwise.

  The rest of the data files should contain two values per line; the first value is the energy in electronvolts, and the second is the differential cross-section in cm<sup>2</sup>; all cross-section values are multiplied by 10<sup>-4</sup> to convert to m<sup>2</sup> when read into memory.

Putting it together, this is an example of a valid input file (and in fact the default input for this project):

    1e6
    10e-9
    2e-9
    1e3
    3
    "data/N2+.dat" "data/N2+B.dat" "data/N2C.dat"

## Postprocessing

The data is plotted in [gnuplot](http://www.gnuplot.info/), using various plotting scripts. Look at them individually to see how they work.

A few shortcuts for plottin are also available through

    $ make ploteedf
    $ make plotratecoeffs

and the plots can be shown via e.g. `eog *.png`, or conveniently

    $ make showplots
