!
! $Revision$
!
 subroutine grib_check(file_name, isgrib)
!$$$  subprogram documentation block
!
! subprogram:    grib_check
!   prgmmr: gayno          org: w/np2     date: 2007-nov-28
!
! abstract:  determine whether file is grib or not.
!  
! program history log:
! 2007-nov-28  gayno    - initial version
! 2011-apr-26  gayno    - replace my simple-minded logic
!                         with call to w3lib routin skgb.
! 2014-feb-07  gayno    - determine whether file is
!                         grib1 or grib2.
!
! usage: call grib_check(file_name, isgrib)
!
!   input argument list:  file_name - file name
!
!   output argument list: isgrib - '1' or '2' if grib1/2 file
!                                  '0' if not grib
!
! files:
!   input:
!      - the file to be checked, unit=11
!
!   output: none
!
! condition codes:
!   22 - bad file open
!
! remarks: none.
!          
!$$$

 implicit none

 include 'mpif.h'

 character*(*), intent(in)         :: file_name
 integer                           :: istat, iseek, mseek, lskip, lgrib, version
 integer, intent(out)              :: isgrib

 print*,"- CHECK FILE TYPE OF: ", trim(file_name)
 call baopenr (11, file_name, istat)

 if (istat /= 0) then
   print*,'- ** FATAL ERROR. BAD OPEN.  ISTAT IS ',istat
   call w3tage('COLDSTART')
   call mpi_abort(mpi_comm_world, 22, istat)
 end if
 
 iseek = 0
 mseek = 64
 call skgb2(11, iseek, mseek, lskip, lgrib, version)

 call baclose(11, istat)
 
 if (lgrib > 0) then
   isgrib = version
   if (isgrib == 1) print*,"- FILE IS GRIB1"
   if (isgrib == 2) print*,"- FILE IS GRIB2"
 else
   isgrib = 0
   print*,"- FILE IS BINARY"
 endif

 return

 end subroutine grib_check

 SUBROUTINE SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
!$$$  subprogram documentation block
!
! subprogram:   skgb2
!   prgmmr: gayno          org: w/np2     date: 2014-feb-07
!
! abstract:  determine whether file is grib or not.
!            based on w3nco library routine skgb.
!  
! program history log:
! 2014-feb-07  gayno    - initial version
!
! usage: call SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
!
!   input argument list:  lugb  - file unit number
!                         iseek - number of bits to skip
!                                 before search.
!                         mseek - max number of bytes 
!                                 to search.
!
!   output argument list:  lskip  - number of bytes to skip 
!                                   before message
!                          lgrib  - number of bytes in message.
!                                   '0' if not grib.
!                          i1     - '1' or '2' if grib1/2 file.
!                                   '0' if not grib.
!
! files:
!   input:
!     - the file to be checked
!
!   output: none
!
! condition codes: none
!
! remarks: none.
!          
!$$$
 INTEGER, INTENT( IN)     :: LUGB, ISEEK, MSEEK
 INTEGER, INTENT(OUT)     :: LSKIP, LGRIB, I1
 PARAMETER(LSEEK=128)
 CHARACTER Z(LSEEK)
 CHARACTER Z4(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 I1=0
 LGRIB=0
 KS=ISEEK
 KN=MIN(LSEEK,MSEEK)
 KZ=LSEEK
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOOP UNTIL GRIB MESSAGE IS FOUND
 DO WHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
!  READ PARTIAL SECTION
   CALL BAREAD(LUGB,KS,KN,KZ,Z)
   KM=KZ-8+1
   K=0
!  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
   DO WHILE(LGRIB.EQ.0.AND.K.LT.KM)
     CALL GBYTEC(Z,I4,(K+0)*8,4*8)
     CALL GBYTEC(Z,I1,(K+7)*8,1*8)
     IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
!  LOOK FOR '7777' AT END OF GRIB MESSAGE
       IF (I1.EQ.1) CALL GBYTEC(Z,KG,(K+4)*8,3*8)
       IF (I1.EQ.2) CALL GBYTEC(Z,KG,(K+12)*8,4*8)
       CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
       IF(K4.EQ.4) THEN
         CALL GBYTEC(Z4,I4,0,4*8)
         IF(I4.EQ.926365495) THEN
!  GRIB MESSAGE FOUND
           LSKIP=KS+K
           LGRIB=KG
         ENDIF
       ENDIF
     ENDIF
     K=K+1
   ENDDO
   KS=KS+KM
   KN=MIN(LSEEK,ISEEK+MSEEK-KS)
 ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 RETURN
 END subroutine skgb2

 subroutine gdt_to_gds(igdtnum, igdstmpl, igdtlen, kgds, ni, nj, res)
!$$$  subprogram documentation block
!
! subprogram:    gdt_to_gds
!   prgmmr: gayno          org: w/np2     date: 2014-sep-26
!
! abstract:  convert from the grib2 grid description template array
!            used by the ncep grib2 library, to the grib1 grid
!            description section array used by ncep ipolates library.
!  
! program history log:
! 2014-sep-26  gayno    - initial version
!
! usage: call gds_to_gds(igdtnum,igdstmpl,igdtlen,kgds,ni,nj,res)
!
!   input argument list:  
!     igdtnum  - grib2 grid desc template number
!     igdstmpl - grib2 grid desc template array
!     igdtlen  - grib2 grid desc template array size
!
!   output argument list: 
!     kgds     - grib1 grid description section array 
!                used by ncep ipolates library.
!     ni,nj    - i/j grid dimensions
!     res      - grid resolution in degrees
!                        
! files: none
!
! condition codes:
!   23 - unrecognized grid type
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 integer, intent(in   )  :: igdtnum, igdtlen, igdstmpl(igdtlen)
 integer, intent(  out)  :: kgds(200), ni, nj
 integer                 :: iscale, istat

 real,    intent(  out)  :: res

 kgds=0

 if (igdtnum.eq.0) then        ! lat/lon grid

   iscale=igdstmpl(10)*igdstmpl(11)
   if (iscale == 0) iscale = 1e6
   kgds(1)=0                   ! oct 6
   kgds(2)=igdstmpl(8)         ! octs 7-8, Ni
   ni = kgds(2)
   kgds(3)=igdstmpl(9)         ! octs 9-10, Nj
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

   kgds(6)=0                   ! oct 17, resolution and component flags
   if (igdstmpl(1)==2 ) kgds(6)=64
   if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(15))/float(iscale)*1000.)  ! octs 18-20, Lat of last grid point
   kgds(8)=nint(float(igdstmpl(16))/float(iscale)*1000.)  ! octs 21-23, Lon of last grid point
   kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.)  ! octs 24-25, di
   kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.) ! octs 26-27, dj

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(19),7)) kgds(11) = 128
   if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

   kgds(12)=0      ! octs 29-32, reserved
   kgds(19)=0      ! oct 4, # vert coordinate parameters
   kgds(20)=255    ! oct 5, used for thinned grids, set to 255

   res = float(kgds(9)) / 1000.0 

 elseif (igdtnum.eq.40) then       !  Gaussian Lat/Lon grid

   iscale=igdstmpl(10)*igdstmpl(11)
   if (iscale==0) iscale=1e6
   kgds(1)=4                   ! oct 6
   kgds(2)=igdstmpl(8)         ! octs 7-8, Ni
   ni = kgds(2)
   kgds(3)=igdstmpl(9)         ! octs 9-10, Nj
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

   kgds(6)=0                   ! oct 17, resolution and component flags
   if (igdstmpl(1)==2 ) kgds(6)=64
   if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(15))/float(iscale)*1000.) ! octs 18-20, Lat of last grid point
   kgds(8)=nint(float(igdstmpl(16))/float(iscale)*1000.) ! octs 21-23, Lon of last grid point
   kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.) ! octs 24-25, Di
   kgds(10)=igdstmpl(18)                                 ! octs 26-27, Number of parallels

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(19),7)) kgds(11) = 128
   if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

   kgds(12)=0      ! octs 29-32, reserved
   kgds(19)=0      ! oct 4, # vert coordinate parameters
   kgds(20)=255    ! oct 5, used for thinned grids, set to 255

   res = float(kgds(9)) / 1000.0

 elseif (igdtnum.eq.20) then       ! Polar Stereographic Grid

   iscale=1e6
   kgds(1)=5                      ! oct 6, data representation type, polar
   kgds(2)=igdstmpl(8)            ! octs 7-8, nx 
   ni = kgds(2)
   kgds(3)=igdstmpl(9)            ! octs 8-10, ny
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(10))/float(iscale)*1000.)  ! octs 11-13, lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(11))/float(iscale)*1000.)  ! octs 14-16, lon of 1st grid point

   kgds(6)=0                      ! oct 17, resolution and component flags
   if (igdstmpl(1) >= 2 .or. igdstmpl(1) <= 5) kgds(6)=64
   if (igdstmpl(1) == 7) kgds(6)=64
   if ( btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(12),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(14))/float(iscale)*1000.)  ! octs 18-20, lon of orientation
   kgds(8)=nint(float(igdstmpl(15))/float(iscale)*1000.)  ! octs 21-23, dx
   kgds(9)=nint(float(igdstmpl(16))/float(iscale)*1000.)  ! octs 24-26, dy

   kgds(10)=0                ! oct 27, projection center flag
   if (btest(igdstmpl(17),1)) kgds(10) = 128

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(18),7)) kgds(11) = 128
   if (btest(igdstmpl(18),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(18),5)) kgds(11) = kgds(11) +  32

   kgds(19)=0    ! oct 4, # vert coordinate parameters
   kgds(20)=255  ! oct 5, used for thinned grids, set to 255

   res = 0.5 * float(kgds(8)+kgds(9)) / 1000. 
   res = res / 111.0

 elseif (igdtnum.eq.1) then    ! Rotated Lat/Lon grid

   if (btest(igdstmpl(19),2)) then  ! e-stagger, bit 6 of scan mode is '1'

     iscale=igdstmpl(10)*igdstmpl(11)
     if (iscale == 0) iscale = 1e6
     kgds(1)=203                    ! oct 6, "E" grid
     kgds(2)=igdstmpl(8)            ! octs 7-8, Ni
     ni = kgds(2)
     kgds(3)=igdstmpl(9)            ! octs 9-10, Nj
     nj = kgds(3)
     kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
     kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

     kgds(6)=0                      ! oct 17, resolution and component flags
     if (igdstmpl(1)==2 ) kgds(6)=64
     if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
     if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

     kgds(7)=nint(float(igdstmpl(20))/float(iscale)*1000.)+90000  ! octs 18-20, Lat of cent of rotation
     kgds(8)=nint(float(igdstmpl(21))/float(iscale)*1000.)        ! octs 21-23, Lon of cent of rotation
     kgds(9)=nint(float(igdstmpl(17))/float(iscale)*500.)         ! octs 24-25, Di
                                                                  ! Note!! grib 2 convention twice grib 1
     kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.)       ! octs 26-27, Dj

     kgds(11) = 0                   ! oct 28, scan mode
     if (btest(igdstmpl(19),7)) kgds(11) = 128
     if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
     if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

     kgds(12)=0    ! octs 29-32, reserved
     kgds(19)=0    ! oct 4, # vert coordinate parameters
     kgds(20)=255  ! oct 5, used for thinned grids, set to 255

     res = sqrt( (float(kgds(9)) / 1000.0)**2   +    &
                 (float(kgds(10)) / 1000.0)**2  )

   else   ! b-stagger

     iscale=igdstmpl(10)*igdstmpl(11)
     if (iscale == 0) iscale = 1e6
     kgds(1)=205                    ! oct 6,     rotated lat/lon for Non-E Stagger grid
     kgds(2)=igdstmpl(8)            ! octs 7-8,  Ni
     ni = kgds(2)
     kgds(3)=igdstmpl(9)            ! octs 9-10, Nj
     nj = kgds(3)
     kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
     kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

     kgds(6)=0                      ! oct 17, resolution and component flags
     if (igdstmpl(1)==2 ) kgds(6)=64
     if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) )  kgds(6)=kgds(6)+128
     if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

     kgds(7)=nint(float(igdstmpl(20))/float(iscale)*1000.)+90000 ! octs 18-20, Lat of cent of rotation
     kgds(8)=nint(float(igdstmpl(21))/float(iscale)*1000.)       ! octs 21-23, Lon of cent of rotation
     kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.)       ! octs 24-25, Di
     kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.)      ! octs 26-27, Dj

     kgds(11) = 0                   ! oct 28, scan mode
     if (btest(igdstmpl(19),7)) kgds(11) = 128
     if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
     if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

     kgds(12)=nint(float(igdstmpl(15))/float(iscale)*1000.) ! octs 29-31, Lat of last grid point
     kgds(13)=nint(float(igdstmpl(16))/float(iscale)*1000.) ! octs 32-34, Lon of last grid point

     kgds(19)=0    ! oct 4, # vert coordinate parameters
     kgds(20)=255  ! oct 5, used for thinned grids, set to 255

     res = ((float(kgds(9)) / 1000.0) + (float(kgds(10)) / 1000.0)) * 0.5

   endif

 else

   print*,'- ** FATAL ERROR CONVERTING TO GRIB2 GDT'
   print*,'- ** UNRECOGNIZED GRID TYPE'
   call w3tage('COLDSTART')
   call mpi_abort(mpi_comm_world, 23, istat)

 endif
 
 end subroutine gdt_to_gds

 subroutine grib2_null(gfld)
!$$$  subprogram documentation block
!
! subprogram:    grib2_null
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  nullify the grib2 gribfield pointers.
!  
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call grib2_null with a gribfield data structure
!
!   input argument list:  
!     gfld - a gribfield data structure
!
!   output argument list: 
!     gfld - a gribfield data structure
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use grib_mod

 implicit none

 type(gribfield), intent(inout)           :: gfld

 nullify(gfld%idsect)
 nullify(gfld%local)
 nullify(gfld%list_opt)
 nullify(gfld%igdtmpl)
 nullify(gfld%ipdtmpl)
 nullify(gfld%coord_list)
 nullify(gfld%idrtmpl)
 nullify(gfld%bmap)
 nullify(gfld%fld)

 end subroutine grib2_null
 
 subroutine grib2_free(gfld)
!$$$  subprogram documentation block
!
! subprogram:    grib2_free
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  deallocate the grib2 gribfield pointers.
!  
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call grib2_free with a gribfield data structure
!
!   input argument list:  
!     gfld - a gribfield data structure
!
!   output argument list: 
!     gfld - a gribfield data structure
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use grib_mod

 implicit none

 type(gribfield), intent(inout)    :: gfld

 if (associated(gfld%idsect)) deallocate(gfld%idsect)
 if (associated(gfld%local)) deallocate(gfld%local)
 if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
 if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
 if (associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
 if (associated(gfld%coord_list)) deallocate(gfld%coord_list)
 if (associated(gfld%idrtmpl)) deallocate(gfld%idrtmpl)
 if (associated(gfld%bmap)) deallocate(gfld%bmap)
 if (associated(gfld%fld)) deallocate(gfld%fld)

 end subroutine grib2_free
