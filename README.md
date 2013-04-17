Reasearch project in Bari, spring 2013
===============

During March-May of 2013 I'm working on a research project in Bari, Italy, creating a Monte-Carlo simulation of electron beams in a collisional medium. This is the code.

The project will be developed in FORTRAN77 using OpenMPI for parallelization.

## Prerequisites

The program requires the following to install and run:

  * A Fortran compiler, e.g. [GNU Fortran](http://gcc.gnu.org/fortran/)
  * An implementation of the MPI Framework, e.g. [Open MPI](http://www.open-mpi.org/)
  * Make, e.g. [GNU Make](http://www.gnu.org/software/make/)

For the postprocessing scripts to work, [gnuplot](http://www.gnuplot.info/) is also required.

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

  The data files should contain two values per line; the first value is the energy in electronvolts, and the second is the differential cross-section in cm<sup>2</sup>; all cross-section values are multiplied by 10<sup>-4</sup> to convert to m<sup>2</sup> when read into memory.

  1. The lower boundaries for the interpolation, for the respective data set, in electronvolts. Note that the ordering must be the same as for the data files.

  If this boundary is outside the data set, a linear extrapolation using the two leftmost data points is used. All cross-section values outside of this boundary are assumed to be zero; it is therefore recommended to use the boundary frequencies for each reaction as the lower limit.

  Example: `11 10.2 10.5`.
  
  1. The upper boundaries for the interpolation, in electron volts. If this boundary is outside the data set, see above. It is recommended that this boundary is strictly larger than the initial energy of the primaries, for all data files. If not at least one data series is extended beyond the initial energy of the primaries.

  1. The energy loss in the process corresponding to the cross-sections, i.e. ionization or excitaiton energies, in electronvolts. Example: `14.5341 8.134 6.3245`

Putting it together, this is an example of a valid input file (and in fact the default input for this project):

    1e6
    10e-9
    2e-9
    1e3
    2
    "data/nist_clean_scaled.dat" "data/nist_clean_scaled.dat"
    15.58 15.58
    1e3 1.1e3
    14.5341 14.5341



## Postprocessing

The data is plotted in [gnuplot](http://www.gnuplot.info/), using the shellscript `plot.sh` which takes as a command line argument the name of the data file to plot. The shellscript processes the data file with an awk script 

A shortcut for this is also available through

    $ make plotlast

and the plots can be shown via e.g. `eog *.png`, or conveniently

    $ make showplots