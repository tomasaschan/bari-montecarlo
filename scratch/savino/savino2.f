        program savino2
        
        parameter(nen=300)
        real en0(1000000)
        real eedf(nen)

        emin=15
        emax=300
        ne=10000
        de=emax/float(nen)

        do ien=1,nen
                eedf(ien)=0
        end do

        do ie=1,ne
               en0(ie)=emax
        end do

        ie=1
        do while (ie.lt.ne)

                en = en0(ie)    !inizio traiettoria

                do while (en.gt.emin)

C                        print *,'evento',ie,en
                       ien=int(en/de)
                       eedf(ien)=eedf(ien)+1
                       enold = en
                       en = en * rand()
                       ne = ne+1
                       en0(ne)=enold-en-emin
                enddo

                ie = ie + 1

C                 print *,ie,ne

        enddo

        do ien=1,nen
               print *,float(ien)*de-de/2,eedf(ien)
        end do

        end