!  PROGRAM read_convdiag 
!  read the conventional data from gsi and put into different array for input to prepbufr
!  PRGMMR:   X. Su       ORG: NP20                 Date: 2007-11-15
!  
! ABSTRACT:  read the conventional data from gsi and put into different array 
!            for input to prepbufr.

      subroutine read_convdiag(filein,cwork,dwork,ndata,knt)

      real(4),allocatable,dimension(:,:)    :: rdiag
      character(8),allocatable,dimension(:) :: cdiag
      
      parameter(maxn=2000000)
      character(8),dimension(maxn) :: cwork
      real(4),dimension(maxn,0:20) :: dwork

      character(len=3)  :: dtype,type 
      character(len=50) :: filein
   
      integer nchar,nreal,ii,mype,ntype,idate,knt(300,0:10)

      data lunin/11/

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

      nn=0        

      open(lunin,file=filein,form='unformatted')
      rewind(lunin); read(lunin) idate; print *, 'idate=',idate
      loopd: do; read(lunin,end=100) dtype,nchar,nreal,ii

      jtype=0
      if(dtype == ' ps') jtype=1
      if(dtype == '  q') jtype=2
      if(dtype == '  t') jtype=3
      if(dtype == ' uv') jtype=4
      if(dtype == 'spd') jtype=5

      allocate(cdiag(ii),rdiag(nreal,ii))
      read(lunin,IOSTAT=iflag) cdiag,rdiag
      if(jtype/=0) then
         loopi: do i=1,ii; nn=nn+1; if(nn.gt.maxn) cycle
         cwork(nn)=cdiag(i)                   ! station id
         itype=rdiag(1,i)
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
         if(cwork(nn)=='0S26229Q') then
            xob=rdiag(3,i);yob=rdiag(4,i);prs=rdiag(6,i)
            obs=dwork(nn,15);bak=dwork(nn,16)
            elv=dwork(nn, 4);zob=dwork(nn, 6);tim=dwork(nn,7)
            uob=dwork(nn,15);vob=dwork(nn,17)
!           write(6,99)cwork(nn),itype,jtype,prs,xob,yob,zob,elv,tim
99          format(a8,2x,2i4,8(2x,g12.4))
            print'(a8,11(1x,f8.2))',cwork(nn),(dwork(nn,j),j=0,8),uob,vob
         endif

         enddo loopi ! end data store loop 
      else
!        print*,dtype,rdiag(1,1)
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
