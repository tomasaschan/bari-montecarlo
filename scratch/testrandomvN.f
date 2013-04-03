      program testrvN
        implicit none

        real*8 random_von_Neumann, r, bins(300)

        integer i, Nsamples
        parameter(Nsamples=1000000)

        do i=1, 300
          bins(i) = 0
        enddo

        do i=1, Nsamples
          r = random_von_Neumann()*300
          bins(int(r)) = bins(int(r)) + 1
        enddo

        do i=1,300
          print *, i, bins(i)/Nsamples
        enddo
      end