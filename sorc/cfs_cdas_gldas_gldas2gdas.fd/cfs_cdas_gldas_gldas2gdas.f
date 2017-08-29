! % gldas2gdaspara gdas1.t00z.sfcanl gdas1.t00z.sfcanl.gldas LIS.E382.2007050200.NOAHgbin
program gldas2sfc
  use sfcio_module
  implicit none
  integer narg,iargc
  integer(sfcio_intkind),parameter:: lusfc=11,luggg=51,luctl=52
  integer(sfcio_intkind):: irets
  character(255) inputsfc,outputsfc,inputgldas
  integer ninputsfc,noutputsfc,ninputgldas
  integer iret,nsfc,n,idrt,i,j,k,jj
  real,allocatable:: tsea(:,:),canopy(:,:),snwdph(:,:),sheleg(:,:)
  real,allocatable:: smc(:,:,:),stc(:,:,:),slc(:,:,:)
  real :: sldph(4) = (/100.,300.,600.,1000./)
  type(sfcio_head):: head
  type(sfcio_data):: data
  narg=iargc()
  if(narg.ne.3) then
     print*, "gldas2sfc_gdas382 gdas1.t00z.sfcanl gdas1.t00z.sfcanl.gldas LIS.E380.2007050200.NOAHgbin"
     if(narg.ne.0) call errmsg('gldas2sfc: 3 arguments required')
     call eusage
     call errexit(1)
  endif

! OUTPUT MERGED SFCANL AND GLDAS

  call getarg(narg-1,outputsfc)
  noutputsfc=len_trim(outputsfc)
  open(luggg,file=outputsfc(1:noutputsfc),action='write',form='unformatted',iostat=iret)
  if(iret.ne.0) then
     call errmsg('gldas2sfc: error opening file '//outputsfc(1:noutputsfc))
     call errexit(2)
  endif

! INPUT GLDAS

  call getarg(narg,inputgldas)
  ninputgldas=len_trim(inputgldas)
  open(luctl,file=inputgldas(1:ninputgldas),form='unformatted',iostat=iret)
  if(iret.ne.0) then
     call errmsg('gldas2sfc: error opening file '//inputgldas(1:ninputgldas))
     call errexit(2)
  endif

! INPUT SFCANL

    call getarg(narg-2,inputsfc)
    ninputsfc=len_trim(inputsfc)
    call sfcio_srohdc(lusfc,inputsfc(1:ninputsfc),head,data,irets)
    if(irets.ne.0) then
print *,'irets=',irets
       call errmsg('gldas2sfc: error opening file '//inputsfc(1:ninputsfc))
       call errexit(2)
    endif

     allocate(tsea(head%lonb,head%latb)) 
     allocate(canopy(head%lonb,head%latb)) 
     allocate(snwdph(head%lonb,head%latb)) 
     allocate(sheleg(head%lonb,head%latb)) 
     allocate(smc(head%lonb,head%latb,head%lsoil)) 
     allocate(stc(head%lonb,head%latb,head%lsoil)) 
     allocate(slc(head%lonb,head%latb,head%lsoil)) 
     call fixio_r_gldas(tsea,canopy,snwdph,sheleg,&
       smc,stc,slc,luctl,head%lonb,head%latb,head%lsoil)

     do j=1,head%latb
        jj = head%latb - j + 1
     do i=1,head%lonb
       if(tsea(i,jj).gt.0 .and. data%smc(i,j,1).lt.1.0)then
     !   data%tsea(i,j)=tsea(i,jj)
     !   data%canopy(i,j)=canopy(i,jj)
     !   data%snwdph(i,j)=snwdph(i,jj)
     !   data%sheleg(i,j)=sheleg(i,jj)
         do k=1,head%lsoil
         data%smc(i,j,k)=smc(i,jj,k)
         data%stc(i,j,k)=stc(i,jj,k)
         data%slc(i,j,k)=slc(i,jj,k)
         enddo
       endif
      enddo
      enddo
    call sfcio_swhead(luggg,head,iret)
    call sfcio_swdata(luggg,head,data,iret)
    call sfcio_axdata(data,irets)
contains
subroutine eusage
  implicit none
  call errmsg('Usage: gldas2sfc inputsfc outputsfc inputgldas')
end subroutine

  subroutine fixio_r_gldas(tsea,canopy,snwdph,sheleg,&
         smc,stc,slc,nread,idim,jdim,lsoil)

      implicit none
      integer  idim,jdim,lsoil,i,j,k,m,nread

      real tsea(idim,jdim),     canopy(idim,jdim),&
           snwdph(idim,jdim),   sheleg(idim,jdim),&
           smc(idim,jdim,lsoil),&
           stc(idim,jdim,lsoil),&
           slc(idim,jdim,lsoil)
      real tmp(idim,jdim)


      read(nread) tsea
      print *, 'read tsea'

      READ(nread) sheleg 
      print *, 'read sheleg '

      do k=1, lsoil
      read(nread) tmp 
       do i=1, idim
       do j=1, jdim
         smc(i,j,k)=tmp(i,j)
       enddo
       enddo
      enddo
      print *, 'read smc(',lsoil,' layers)'

      do k=1, lsoil
      read(nread) tmp
       do i=1, idim
       do j=1, jdim
         stc(i,j,k)=tmp(i,j)
       enddo
       enddo
      enddo
      print *, 'read stc(',lsoil,' layers)'

      read(nread) snwdph
      print *, 'read snwpdh '

      do k=1, lsoil
      read(nread) tmp
       do i=1, idim
       do j=1, jdim
         slc(i,j,k)=tmp(i,j)
       enddo
       enddo
      enddo
      print *, 'read slc(',lsoil,' layers)'

      read(nread) canopy
      print *, 'read canopy '

      return 
end subroutine
end program
