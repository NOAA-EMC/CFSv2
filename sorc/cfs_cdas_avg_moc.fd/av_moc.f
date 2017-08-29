       program av_moc

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program read 6-hourly/daily moc file and calculate the average value   !
!                                                                              !
!  Xingren Wu   (Xingren.Wu@noaa.gov)                                          !
!  February 11, 2010                                                           !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

      implicit none

      integer jm,km,nreg,nfiles
      CHARACTER*10 kjm,kkm,knreg,knfiles
      character*120 avmocfile

!
      call getenv("jm",kjm)
      read(kjm,'(I10)') jm
      write(*,*) "jm= ",jm
!
      call getenv("km",kkm)
      read(kkm,'(I10)') km
      write(*,*) "km= ",km
!
      call getenv("nreg",knreg)
      read(knreg,'(i10)') nreg
      write(*,*) "nreg= ",nreg
!
      call getenv("nfiles",knfiles)
      read(knfiles,'(i10)') nfiles
      write(*,*) "nfiles= ",nfiles
!
      call getenv("avmocfile",avmocfile)
      write(*,*) "avmocfile= ",avmocfile
!
      call av_moc_cal(jm,km,nreg,nfiles,avmocfile)

      stop
      end program av_moc

      subroutine av_moc_cal(jm,km,nreg,nfiles,avmocfile)

      implicit none

      integer jm,km,nreg,nfiles
      integer iyr,imth,iday,ihr,mfh0,mfh1,mfh2,mfh
      integer k,kreg,n
      real*8 divid
      real*8, dimension(jm,km,nreg)  :: davmoc,dvmoc
      real, dimension(jm,km,nreg)  :: avmoc,vmoc
      character*120 avmocfile,mocfile(nfiles)
!
      davmoc=0.
      divid=1.0/float(nfiles)
      print *,'nfiles,divid:',nfiles,divid

      open (71,file=avmocfile,form='unformatted')
      do n=1,nfiles
         read(*,*) mocfile(n)
         print *,n, ' mocfile:', mocfile(n)

         open (11,file=mocfile(n),form='unformatted')
         read (11) iyr,imth,iday,ihr,mfh1,mfh2
         print *,'iyr,imth,iday,ihr,mfh1,mfh2:', 
     &            iyr,imth,iday,ihr,mfh1,mfh2
         do kreg=1,nreg
         do k=1,km
            read (11) vmoc(:,k,kreg)
            print *, vmoc(197:206,k,kreg)
         enddo
         enddo
         close (11)

         if (n.EQ.1) mfh0=mfh1
         if (n.EQ.nfiles) mfh=mfh2
         dvmoc=vmoc
         davmoc=davmoc+dvmoc*divid
      enddo

      avmoc=davmoc

      write (71) iyr,imth,iday,ihr,mfh0,mfh
      do kreg=1,nreg
      do k=1,km
         write (71) avmoc(:,k,kreg)
      enddo
      enddo
      close (71)

      return
      end subroutine av_moc_cal
