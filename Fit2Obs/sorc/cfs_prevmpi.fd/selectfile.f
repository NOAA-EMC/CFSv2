!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE SELECTFILE(IUNITF,INPTYP)
      
      USE SIGIO_MODULE
      USE SIGIO_R_MODULE
      use nemsio_module, only: nemsio_gfile,nemsio_init, nemsio_open,nemsio_close
      use netcdf

      IMPLICIT NONE

      CHARACTER*20 CFILE
      INTEGER*4 IRET,JRET,IUNIT,ncid
      INTEGER*4 IUNITF, INPTYP
      TYPE(SIGIO_HEAD) :: CHEAD
      TYPE(NEMSIO_GFILE) :: GFILE

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      WRITE(CFILE,'("fort.",I2.2)') IUNITF
      inptyp=0
      print*

! check for netcdf file

      iret = nf90_open(trim(cfile),nf90_nowrite,ncid)
      if (iret == 0) then
         print *,' ===> GFS FCST/ANAL INPUT IS NETCDF'
         inptyp=3 ; return
      endif 

! check for sigio file

      CALL SIGIO_RROPEN(IUNITF,CFILE,IRET)
      CALL SIGIO_SRHEAD(IUNITF,CHEAD,JRET)
      IF(IRET == 0 .AND. JRET == 0) THEN
         print *,' ===> GLOBAL FCST/ANAL INPUT IS SIGIO'
         CALL SIGIO_SCLOSE(IUNITF,IRET)
         inptyp=1 ; return
      endif

! check for nemsio file (this has a bomb in it - put it last)

      CALL NEMSIO_OPEN(GFILE,trim(CFILE),'read',IRET=IRET)
      IF(IRET == 0) THEN
         CALL NEMSIO_CLOSE(GFILE,IRET=IRET)
         print *,' ===> GFS FCST/ANAL INPUT IS NEMSIO'
         inptyp=2 ; return
      ENDIF

      if(inptyp==0) call bort('selecetfile cannot id filetype')

      RETURN
      END
