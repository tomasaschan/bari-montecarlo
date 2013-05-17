      module reactions
        use precision
        use io, only : herer
          
        implicit none

        private

        real(rkind), allocatable, public  :: reaction_counts(:,:)
        real(rkind) :: de
        integer(lkind), parameter, public :: Nreactbins = int(1e3)

        public :: single_run
        public :: init_reactions
        public :: print_reaction_counts
        public :: cleanup_reactions

      contains

        subroutine init_reactions(e0)
          use physics, only : NCollProc

          implicit none

          real(rkind) e0

          allocate(reaction_counts(Nreactbins, NCollProc))
          reaction_counts = 0

          de = e0/real(Nreactbins,rkind)         
        end subroutine

        recursive subroutine single_run(e0)
          use physics, only : cs_min, products, secondary_energy
          
          implicit none

          real(rkind), intent(in) :: e0
          real(rkind)             :: e, emin, e2
          integer(ikind)          :: ip

          e = e0
          emin = minval(cs_min)

          do while (e .ge. emin)
            ip = choose_collision_process(e)
            
            ! test for null collisions
            if (ip .gt. 0) then
              ! note collision in protocol
              call increment_reaction_count(e, ip)
              
              ! reduce particle energy
              e = e-cs_min(ip)

              ! ionizing collision?
              if (products(ip) .gt. 0) then
                ! split energy between two particles
                e2 = secondary_energy(e)
                e = e - e2
                ! initiate new run with secondary electron
                call single_run(e2)
              end if ! ionizing
            end if ! null collision

          end do
        end subroutine single_run

        subroutine increment_reaction_count(e, ip)
          implicit none

          real(rkind) e
          integer ip, ie

          ie = int(e/de)
          if (ie.eq.0) then
            ie = 1
          end if

          reaction_counts(ie,ip) = reaction_counts(ie,ip) + 1 
        end subroutine increment_reaction_count

        subroutine print_reaction_counts()
          use physics, only : NCollProc
          implicit none

          integer(lkind) ie, ip
          
          write(*,'(F10.2,3F10.0)') ((/ie*de, (reaction_counts(ie,ip),ip=1,NCollProc) /),ie=1,Nreactbins)

        end subroutine print_reaction_counts

        function choose_collision_process(e)
          use physics, only : NCollProc, total_collision_frequency, collision_frequency
          use random, only : random_real
          implicit none

          real(rkind) e
          integer choose_collision_process


          real(rkind) r, cf, cfsum
          integer ip

          cfsum = 0.0
          r = random_real()
          choose_collision_process = 0

          do ip = 1, int(NCollProc)
            cf = collision_frequency(e,ip) / total_collision_frequency
            cfsum = cfsum + cf
            if (cfsum .gt. r .and. choose_collision_process .eq. 0) then
              choose_collision_process = ip
            end if
          end do
        end function choose_collision_process    


        subroutine cleanup_reactions()
          implicit none
          deallocate(reaction_counts)
        end subroutine cleanup_reactions

      end module reactions