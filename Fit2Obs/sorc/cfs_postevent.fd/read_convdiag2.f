!  PROGRAM read_convdiag 
!  read the conventional data from gsi and put into different array for input to prepbufr
!  PRGMMR:   X. Su       ORG: NP20                 Date: 2007-11-15
!  
! ABSTRACT:  read the conventional data from gsi and put into different array 
!            for input to prepbufr.

      subroutine read_convdiag(filein,cwork,dwork,ndata,knt)

      real(4),allocatable,dimension(:,:)    :: rdiag
      character(8),allocatable,dimension(:) :: cdiag

      character(8) sid; real(8) rid; equivalence(sid,rid)
      
      parameter(maxn=1500000)
      character(8),dimension(maxn) :: cwork
      real(4),dimension(maxn,0:20) :: dwork

      character(len=3)  :: dtype,type 
      character(len=50) :: filein
   
      integer nchar,nreal,ii,mype,ntype,idate,knt(300,0:10)

      logical first

      data lunin/11/
      data lunit/20/

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      nn=0        

      close(lunin)
      call datebf(lunit,mear,mmon,mday,mour,jdate);jdate=i4dy(jdate)
      open(lunin,file=filein,form='unformatted',convert="big_endian")
      first=.true. ; goto 3
2     if(first) then
         close(lunin)
         open(lunin,file=filein,form='unformatted',convert="little_endian")
         first=.false.
         goto 3
      else
         call bort('bad diag file or wrong date')
      endif

3     rewind(lunin); read(lunin,err=2) idate; print *, idate,jdate      
      if(idate/=jdate) goto 2
      loopd: do; read(lunin,end=100,err=2) dtype,nchar,nreal,ii
      !print*,dtype,nchar,nreal,ii

      jtype=0
      if(dtype == ' ps') jtype=1
      if(dtype == '  q') jtype=2
      if(dtype == '  t') jtype=3
      if(dtype == ' uv') jtype=4
      if(dtype == 'spd') jtype=5
      !if(dtype == 'gps') jtype=6

      allocate(cdiag(ii),rdiag(nreal,ii))
      read(lunin,IOSTAT=iflag) cdiag,rdiag
      if(jtype/=0) then
         loopi: do i=1,ii
         itype=rdiag(1,i); if(itype>=240.and.itype<=260) cycle ! skip satwinds
         nn=nn+1; if(nn.gt.maxn) cycle                       ! don't overflow arrays
         cwork(nn)=cdiag(i)                   ! station id
         dwork(nn,0)=jtype                    ! observation type (ps,q,t,uv,etc) 
         dwork(nn,1)=itype                    ! oi report type 
         dwork(nn,2)=rdiag(3,i)               ! observation latitude(degrees) 
         dwork(nn,3)=rdiag(4,i)               ! observation longitude(degrees)
         dwork(nn,4)=rdiag(5,i)               ! station elevation(meters) 
         dwork(nn,5)=rdiag(6,i)               ! observation pressure (hpa) 
         dwork(nn,6)=rdiag(7,i)               ! observation height (meters) 
         dwork(nn,7)=rdiag(8,i)               ! obs time (hours relat. to anal. time) 
         dwork(nn,8)=rdiag(9,i)               ! input prepbufr qc or event mark 
         dwork(nn,9)=rdiag(11,i)              ! read_prepbufr data usage flag 
         dwork(nn,10)=rdiag(12,i)             ! data usage flag((1=use, -1=not used) 
         dwork(nn,11)=rdiag(13,i)*.25         ! variational qc weight 
         dwork(nn,12)=1.0/rdiag(16,i)             ! analsis adjusted observation error 
         if( trim(dtype) == ' uv') then
            dwork(nn,15)=rdiag(17,i)             ! u observation
            dwork(nn,16)=rdiag(17,i)-rdiag(18,i) ! u background or analysis
            dwork(nn,17)=rdiag(20,i)             ! v observation
            dwork(nn,18)=rdiag(20,i)-rdiag(21,i) ! v background or analysis
            dwork(nn,19)=rdiag(23,i)             ! 10m wind reduction factor 
         else if( trim(dtype) == '  q') then
            dwork(nn,15)=rdiag(17,i)             ! observation   
            dwork(nn,16)=rdiag(17,i)-rdiag(18,i) ! background or analysis   
            dwork(nn,17)=rdiag(20,i)             ! guess saturation specific humidity 
         else 
            dwork(nn,15)=rdiag(17,i)             ! observation   
            dwork(nn,16)=rdiag(17,i)-rdiag(18,i) ! background or analysis   
         endif
         knt(itype,jtype)=knt(itype,jtype)+1; knt(itype,0)=knt(itype,0)+1

!        if(cwork(nn)=='GSV0GBRA') then
!        if(cwork(nn)=='0S26229Q') then
         if(itype==2210000) then
            xob=rdiag(3,i);yob=rdiag(4,i);prs=rdiag(6,i)
            obs=dwork(nn,15);bak=dwork(nn,16)
            elv=dwork(nn, 4);zob=dwork(nn, 6);tim=dwork(nn,7)
            uob=dwork(nn,15);vob=dwork(nn,17)
            !!write(6,99)cwork(nn),itype,jtype,prs,xob,yob,zob,elv,tim
99          format(a8,2x,2i4,8(2x,g12.4))
            !print*,'-------------------------------------------------------------'
            !print'("a8:",a8)',cwork(nn)
            !sid=cwork(nn); print*,"r8:",rid
            !print'("z16: ",z16,11(1x,f8.2))',cwork(nn),(dwork(nn,j),j=0,8),uob,vob
            print'(a8,11(1x,f8.2))',cwork(nn),(dwork(nn,j),j=0,8),uob,vob
         endif

         enddo loopi ! end data store loop 
      else
         !print*,dtype,rdiag(1,1)
      endif
      deallocate(cdiag,rdiag)
      enddo loopd ! ending read data do loop

100   ndata=nn

      if(ndata.gt.maxn) then
         print*,'ndata=',ndata
         call bort('too many diag records')
      endif

      close(lunin)
      return

      print*,'data counts by report type'
      do i=1,300
      if(knt(i,0).gt.0) print'(i3,8(2x,i8))',i,(knt(i,j),j=0,5)
      enddo

      return
      end 
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine opendian(iunit,file)
      character*(*) file
      character*15  convert,nativey,nativen,be,le
      character*1   byte                    
      integer ibyte,jbyte,iswap
      equivalence(byte,ibyte)
      data le/'little_endian'/
      data be/'big_endian   '/
!----------------------------------------------------------------------
!----------------------------------------------------------------------
! what is native?
      ibyte=1; convert='none'
      if(ichar(byte)==0) then 
         nativey=be;nativen=le
      else
         nativey=le;nativen=be
      endif
      print*,'running on ',nativey,' platform'

! try to open the file natively, if yes, report, rewind, return

      open(iunit,file=file,form='unformatted')
      read(iunit,iostat=iostat) 
      if(iostat==0)then ! file is opened as native
         print*,nativey,trim(file)
         rewind(iunit)
         return
      endif

! try to open the file non-natively, if yes, report, rewind, return

      close(iunit)
      open(iunit,file=file,form='unformatted',convert=nativen) 
      read(iunit,iostat=iostat) ibyte
      if(iostat==0)then ! file is opened as non-native
         print*,nativen,trim(file)
         rewind(iunit)
         return
      endif

      print*,'unable to open ',trim(file)
      return
      end
