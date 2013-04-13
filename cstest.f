      program cstest
        implicit none
        include 'mpif.h'


        ! DECLARE VARIABLES

        ! Parameters for simulation (indata)
        integer Nruns
        real*8 tfin, dt, eI, e0
        
        ! MPI related stuff
        integer rnk, nproc
      
        ! Data for collisional cross-sections
        integer Ninterp
        parameter(Ninterp=int(1e5))
        real*8 cs_data(Ninterp,2)
        character*30 fname
        real*8 x0, x1
        common /cs/ cs_data, x0, x1
        
        real*8 current_energy, end_energy, denergy, cs, v, nrct
        real*8 cross_section, nonrandom_collision_time, velocity

        ! INITIALIZE SIMULATION
        
        ! MPI framework
        call init_mpi(rnk, nproc)
        ! Read data from stdin and share to all processes
        call read_program_input(Nruns, tfin, dt, e0, eI, 
     +                          fname, x0, x1, rnk)
        ! Interpolate cross-section data, and share with all processes
        call get_interpolation(fname,Ninterp,cs_data,x0,x1,rnk)

        current_energy = 0
        end_energy = x1
        denergy = 1e-1

        do while (current_energy .lt. end_energy)
            cs = cross_section(current_energy)
            v = velocity(current_energy)
            nrct = nonrandom_collision_time(current_energy)
          print *, current_energy, cross_section(current_energy), nrct
          current_energy = current_energy + denergy
        enddo

        call finalize_mpi()
      end