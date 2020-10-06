!  PROGRAM read_convdiag
!  read the conventional data from gsi and put into different array for
!  input to prepbufr
!  PRGMMR:   X. Su       ORG: NP20                 Date: 2007-11-15
!
! ABSTRACT:  read the conventional data from gsi and put into different
! array
!            for input to prepbufr.
!
      subroutine read_convdiag(filein,cwork,dwork,ndata,knt)

      use netcdf 

      real(4),allocatable,dimension(:,:)    :: rdiag
      character(8),allocatable,dimension(:) :: cdiag

      character(8) sid; real(8) rid; equivalence(sid,rid)

      parameter(maxn=3500000)
      character(8) ,dimension(maxn)      :: cwork
      integer(4)   ,dimension(maxn)      :: itype
      real(4)      ,dimension(maxn,0:20) :: dwork

      character(len=3)  :: dtype,type
      character(len=50) :: filein,filenn

      integer nchar,nreal,ii,mype,ntype,idate,knt(300,0:10)
      integer dimid

      logical first

      data lunin/11/
      data lunit/20/

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      nn=1

!  loop over datagypes to process

      do i=1,4
      if(i==1) dtype='ps' 
      if(i==2) dtype='q'  
      if(i==3) dtype='t'  
      if(i==4) dtype='uv' 

      filenn=trim(filein)//'_'//trim(dtype); print*,filenn

!  open the file and find the dimension of the arrays

      call check( nf90_open(filenn,NF90_NOWRITE,ncid), "opening PL file")
      call check( nf90_inq_dimid(ncid,"nobs",dimid), "getting nobs dimid")
      call check( nf_inq_dimlen (ncid,dimid,nobs ),  "getting nobs dim")

!  check on the array bounds

      if(nn+nobs.gt.maxn) print*,'nn=',nn+nobs           
      if(nn+nobs.gt.maxn) call bort('maxn exceeded')
      print*,'nn=',nn+nobs,nobs

      !!nn=nn+nobs; cycle 

      jtype=0
      if(dtype == 'ps') jtype=1
      if(dtype == 'q') jtype=2
      if(dtype == 't') jtype=3
      if(dtype == 'uv') jtype=4
      if(dtype == 'spd') jtype=5
      !if(dtype == gps') jtype=6

      if(jtype==0) print*,dtype
      if(jtype==0) call bort('dtype')

!  read a dtype (jtype)  segment from netcdf into dwork

         call get_c(ncid,"Station_ID",nobs,cwork(nn))               ! station id

         call get_i(ncid,"Observation_Type    ",nobs,itype)         ! oi report type
         call get_r(ncid,"Latitude            ",nobs,dwork(nn,2))   ! observation latitude(degrees)
         call get_r(ncid,"Longitude           ",nobs,dwork(nn,3))   ! observation longitude{degrees)
         call get_r(ncid,"Station_Elevation   ",nobs,dwork(nn,4))   ! station elevation(meters)
         call get_r(ncid,"Pressure            ",nobs,dwork(nn,5))   ! observation pressure (hpa)
         call get_r(ncid,"Height              ",nobs,dwork(nn,6))   ! observation height (meters)
         call get_r(ncid,"Time                ",nobs,dwork(nn,7))   ! obs time (hours relat.  to anal. time)
         call get_r(ncid,"Prep_QC_Mark        ",nobs,dwork(nn,8))   ! input prepbufr qc or event mark
         call get_r(ncid,"Prep_Use_Flag       ",nobs,dwork(nn,9))   ! read_prepbufr data usage flag
         call get_r(ncid,"Analysis_Use_Flag   ",nobs,dwork(nn,10))  ! data usage flag((1=use, -1=not used)0
         call get_r(ncid,"Nonlinear_QC_Rel_Wgt",nobs,dwork(nn,11))  ! variational qc weight
         call get_r(ncid,"Errinv_Adjust       ",nobs,dwork(nn,12))  ! analsis adjusted observation error

         if( trim(dtype) == 'uv') then
            call get_r(ncid,"u_Observation                    ",nobs,dwork(nn,15))           ! u observation
            call get_r(ncid,"u_Obs_Minus_Forecast_adjusted    ",nobs,dwork(nn,16))           ! u background or analysis
            call get_r(ncid,"v_Observation                    ",nobs,dwork(nn,17))           ! v observation
            call get_r(ncid,"v_Obs_Minus_Forecast_adjusted    ",nobs,dwork(nn,18))           ! v background or analysis
            !!dwork(nn,19)=rdiag(23,i)             ! 10m wind reduction factor
         else if( trim(dtype) == 'q') then
            call get_r(ncid,"Observation                      ",nobs,dwork(nn,15))           ! observation
            call get_r(ncid,"Obs_Minus_Forecast_adjusted      ",nobs,dwork(nn,16))           ! background or analysis
            call get_r(ncid,"Forecast_Saturation_Spec_Hum     ",nobs,dwork(nn,17))           ! guess saturation specific humidity
         else
            call get_r(ncid,"Observation                      ",nobs,dwork(nn,15))           ! observation
            call get_r(ncid,"Obs_Minus_Forecast_adjusted      ",nobs,dwork(nn,16))           ! background or analysis
         endif

         do n=1,nobs        
         dwork(nn+n-1,0)=jtype
         dwork(nn+n-1,1)=itype(n)
         it=itype(n)
         if(it>=240.and.it<=260) cycle ! skip satwinds
         knt(it,jtype)=knt(it,jtype)+1
         knt(it,0)=knt(it,0)+1

         if( dwork(nn+n-1,0)==0) write(6,'(12(f10.2))') dwork(n,0:11)
         if( dwork(nn+n-1,1)==0) write(6,'(12(f10.2))') dwork(n,0:11)
         if( dwork(nn+n-1,0)==0) call bort('jtype=0') 
         if( dwork(nn+n-1,1)==0) call bort('itype=0') 
         enddo

         nn=nn+nobs

         enddo ! end of loop over vars (ps,q,t,uv)

         ndata=nn-1

         do n=1,ndata
         dwork(n,16)=dwork(n,15)-dwork(n,16)
         if(jtype==4)dwork(n,18)=dwork(n,17)-dwork(n,18)
         !write(6,'(a8,2x,13(f10.2))') cwork(n),dwork(n,0:12) 
         if(nint(dwork(n,1))==0) print*,n,dwork(n,1)
         enddo

         print*; print*,'data counts by report type'
         do i=1,300
         if(knt(i,0).gt.0) print'(i3,8(2x,i8))',i,(knt(i,j),j=0,5)
         enddo; print*

         return 
         end
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine get_i(ncid,varname,dim,arr)

         use netcdf
        
         character(*) varname
         integer      dim,varid
         integer      arr(dim)
         call check( nf90_inq_varid(ncid,varname,varid), "getting "//varname//" varid")
         call check( nf90_get_var(ncid,varid,arr),"reading "//varname)
         !print*,varname,arr(1:10)

      end subroutine get_i
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine get_r(ncid,varname,dim,arr)

         use netcdf
         integer :: start, count
         character(*) varname
         integer      dim,varid
         real         arr(dim)
         call check( nf90_inq_varid(ncid,varname,varid), "getting "//varname//" varid")
         call check( nf90_get_var(ncid,varid,arr),"reading "//varname)
         !print*,varname,arr(1:10)

      end subroutine get_r
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine get_c(ncid,varname,dim,arr)

         use netcdf
         integer :: start(4), count(4)
         character(*)  varname
         integer       dim,varid
         character(*)  arr(dim)
         call check( nf90_inq_varid(ncid,varname,varid), "getting "//varname//" varid")
         call check( nf90_get_var(ncid,varid,arr),"reading "//varname)
         !print*,varname,arr(1:10)

      end subroutine get_c
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------
      subroutine check(status, loc)

         use netcdf

         integer, intent(in) :: status
         character(len=*), intent(in) :: loc

         if(status /= NF90_NOERR) then
            write (*,*) "Error at ", loc
            write (*,*) NF90_STRERROR(status)
         end if

      end subroutine check
!---------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------

