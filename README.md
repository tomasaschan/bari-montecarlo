# Reasearch project in Bari, spring 2013


During March-May of 2013 I'm working on a research project in Bari, Italy, creating a Monte-Carlo simulation of electron beams in a collisional medium. This is the code.

The code is developed in Fortran, licensed under a modified version of the MIT license - see `LICENSE.md`.

## Contents of this guide

1. Running the program

  1. Execution
  1. Program input
  1. Program output

1. Obtaining the source code

## Running the program

### Execution

The download provides two script files, `simulate.sh` for Linux-based systems and `simulate.bat` for Windows. Running the program is as easy as running the appropriate script.

However, some detail about the program input and output is in place:

### Program input

The program reads a number of parameters from `stdin`. In the default setup, this is redirected to the file `input.in`, but it's absolutely possible to run enter the information manually as long as the given information conforms with the format.  
  The program then also reads data for collisional cross-sections from a number of other files.

**The structure of `input.in`**


1. First, a few simple parameters for the simulation:

  1. The number of simulated particles at `t=0` (e.g. `1e6`)

  1. The time, in seconds, for which to run the simulation (e.g. `10e-9` or `1e-8` for 10 ns)

  1. The time step, in seconds. EEDF data is output for each time step (real, e.g. `1e-9` for data every nanosecond), and the rate coefficients are evaluated at each time step; thus this is the resolution of the rate coefficients used for calculating populations densities of various excitaitons.

  1. The initial energy of the primaries, in electronvolts (`1e3` for 1 keV, or `200` for 200 eV)

  1. À mathematical expression describing the electron density as a function of time. The function supports syntax as described at the bottom of [this page](http://fparser.sourceforge.net/), with the only available variable being `t` for the time in seconds.

1. Next, information about the collisional processes which should be considered.  
**Note:** The first of these lines determines how the rest of the file is interpreted by the program, but **no verification is done at execution time**. If the input file is not consistent, the program might throw an error or return incorrect results - the behavior is undefined.

  1. The number of collisional processes to be considered, and the number of species for which population densities should be calculated (e.g. `5 2`)

  1. The parameter `A` for each species for which populations should be calculated, in s<sup>-1</sup> (e.g. `2.74e7 1.53e7`)

  1. The parameter `Q` for each species for which populations should be calculated,
  in cm<sup>3</sup>s<sup>-1</sup> (e.g. `3.67e-11 8.84e-10`)

1. Finally, a list of the data files from which data for the differential cross-sections is read, one file per line with paths relative to the executable, enclosed in quotes.  
 **Note: The ordering is important!** The files should be listed in an order such that the first `n` files correspond to the processes for which populations should be calculated, given in the same order as the parameters `A` and `Q` above, and other processes follow.

1. The file **must** end with (at least) one empty line

Putting it together, this is an example of a valid input file which considers three collisional processes and calculates populations of the species here labelled N2B and N2C:

    1e6
    10e-9
    2e-9
    1e3
    3 2
    2.74e7 1.53e7
    3.67e-11 8.84e-10
    "data/N2C.dat"
    "data/N2B.dat"
    "data/N2.dat" 

**The structure of the data files**

  The first line of the data file should contain three whitespace-separated values. The first two the lower and upper boundaries of the energy range for which this collisional process should be considered - the cross-section will be considered 0 outside this range. The third value is a 1 if the process spawns a secondary electron, and 0 otherwise.

  The rest of the data files should contain two values per line; the first value is the energy in electronvolts, and the second is the differential cross-section in cm<sup>2</sup>.

  The first few lines of an example datafile looks like this:
 
     15.581    1e3 1
     15.58     0.000E-16
     16.00     0.013E-16
     16.50     0.030E-16

  For this file, the cross-section will be interpolated between the given data points between `15.581` and `1000` eV. Each time a collision of this type occurs, a secondary is produced (signified by the `1` at the end of the first line) which will share the available energy with the primary.

### Program output

The program outputs some diagnostic data to `stdout`, which is by default redirected to a file called `simulation.out`. When opened in a text editor, this file contains some information about the run, such as the experimental parameters and the time it took to run the program.

The data is output to three different files:

* `eedf.dat` contains data for the electron energy distribution function. It has three columns, listing the time, energy and value of the eedf. The time is given in seconds, the energy in electronvolts, and the eedf normalized such that its integral is 1 for any given time.

  The following `Matlab` code plots the eedf at the time 10 ns:

        data = load('eedf.dat');
        rows = data(:,1)==10e-9; % a logical matrix which selects the correct rows
        eedf = data(rows, 2:3);
        plot(eedf(:,1), eedf(:,2))

* `rate.dat` contains data for the rate coefficients for each species in the simulation, with the time in seconds the first column and the rate coefficients in cm<sup>3</sup>s<sup>-1</sup> in the following columns, *in the same order as in the input file*.

  The following `Matlab` script plots the ratio of rate coefficients for species `N2B/N2C`, given the input file above:

        data = load('rate.dat');
        times = data(:,1);
        ratios = data(:,3)./data(:,2) % second column is N2C, since that was first in input.in
        plot(times, ratios);

* `pops.dat` contains the data for the populations for each species, with the time in seconds in the first column and the population density in cm<sup>-3</sup> in the following columns, *in the same order as in the input file*.

  The following `Matlab` script plots the population of the `N2B` species versus time:

        data = load('pops.dat')
        plot(data(:,1), data(:,3))

## Obtaining the source code

The source code for this project [is available on github.com](https://github.com/tlycken/bari-montecarlo/). Don't forget to [read the license information](https://github.com/tlycken/bari-montecarlo/blob/master/LICENSE.md) ;)