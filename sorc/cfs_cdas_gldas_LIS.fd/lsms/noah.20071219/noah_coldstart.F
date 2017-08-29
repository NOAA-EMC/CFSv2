!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
! !ROUTINE: noah_coldstart.F90
!
! !DESCRIPTION:
!  
! Routine for noah initialization from cold start
! 
! !INTERFACE:
subroutine noah_coldstart()
! !USES:
   use lisdrv_module, only: lis, grid, glbgindex
   use noah_varder
!3.1   use time_module
   use time_manager
   use spmdMod, only: iam
!EOP
   implicit none
   real, allocatable :: valtemp(:,:)
   real, allocatable :: valtempS(:,:,:)
   real, allocatable :: jtemp(:,:) 	!jesse 20040426 for outputing IC
   real, allocatable :: jtemp4(:,:,:)   !jesse 20040426 for outputing IC
   integer :: t,l,n,c,r,rr,cc
!BOC
   if ( lis%o%startcode == 2 ) then
      print*,'MSG: noah_coldstart 2 -- cold-starting noah', &
             '...using ics from card file',' (', iam, ')'
      
      print*,'DBG: noah_coldstart -- nch',lis%d%nch, &
           ' (', iam, ')'
      do t=1,lis%d%nch
         noah(t)%t1=noahdrv%noah_it
!J         noah(t)%t1=280.0         
         noah(t)%cmc=0.0004 
         noah(t)%snowh=0.0
         noah(t)%sneqv=0.0
         noah(t)%ch=0.0150022404
         noah(t)%cm=0.0205970779
         do l=1,4
            noah(t)%stc(l)=noahdrv%noah_it
         enddo
         noah(t)%smc(1)=0.3252287
         noah(t)%smc(2)=0.3194746
         noah(t)%smc(3)=0.3172167
         noah(t)%smc(4)=0.3078052
         noah(t)%sh2o(1)=0.1660042
         noah(t)%sh2o(2)=0.2828006
         noah(t)%sh2o(3)=0.3172163
         noah(t)%sh2o(4)=0.3078025
      enddo  
      lis%t%yr=lis%t%syr
      lis%t%mo=lis%t%smo 
      lis%t%da=lis%t%sda
      lis%t%hr=lis%t%shr
      lis%t%mn=lis%t%smn
      lis%t%ss=lis%t%sss

      call date2time(lis%t%time,lis%t%doy,lis%t%gmt,lis%t%yr,&
                     lis%t%mo,lis%t%da,lis%t%hr,lis%t%mn,lis%t%ss) 
      write(*,*)'MSG: noah_coldstart -- Using lis.crd start time ',&
                lis%t%time, ' (', iam, ')'
      write(79,*)'MSG: noah_coldstart -- Using lis.crd start time ',&
                 lis%t%time, ' (', iam, ')'
   elseif ( lis%o%startcode == 3 ) then
      allocate(valtemp(lis%d%gnc,lis%d%gnr))
      allocate(valtempS(4,lis%d%gnc,lis%d%gnr))
      allocate(jtemp(lis%d%gnc,lis%d%gnr))
      allocate(jtemp4(4,lis%d%gnc,lis%d%gnr))
      print*,'MSG: noah_coldstart 3 -- IC read', &
             '...using ics from files',' (', iam, ')'
      
      print*,'DBG: noah_coldstart -- nch',lis%d%nch, &
           ' (', iam, ')'
      print*,'MSG: array sizes :',lis%d%gnc,lis%d%gnr

      rr=0

!     Skin Temperature
      jtemp = -999.	!jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_T1',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%t1=valtemp(c,r)
         jtemp(cc,rr) = valtemp(c,r) 
      enddo
      cc=0
      enddo
      rr=0
      print*,'T1 read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="t1.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     Canopy Moisture Content
      jtemp = -999.     !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_CMC',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%cmc=valtemp(c,r)
         jtemp(cc,rr) = valtemp(c,r)
      enddo
      cc=0
      enddo
      rr=0
      print*,'CMC read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="cmc.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     Snow Depth
      jtemp = -999.     !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_SNOWH',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%snowh=valtemp(c,r)
         jtemp(cc,rr) = valtemp(c,r)
      enddo
      cc=0
      enddo
      rr=0
      print*,'SNOWH read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="snowh.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     SNEQV
      jtemp = -999.     !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_SNEQV',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%sneqv=valtemp(c,r)/1000.
         jtemp(cc,rr) = valtemp(c,r)
      enddo
      cc=0
      enddo
      rr=0
      print*,'SNEQV read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="sneqv.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     CH
      jtemp = -999.     !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_CH',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%ch=valtemp(c,r)
         jtemp(cc,rr) = valtemp(c,r)
      enddo
      cc=0
      enddo
      rr=0
      print*,'CH read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="ch.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     CM
      jtemp = -999.     !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_CM',form='unformatted')
      read(40)valtemp
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%cm=valtemp(c,r)
         jtemp(cc,rr) = valtemp(c,r)
      enddo
      cc=0
      enddo
      rr=0
      print*,'CM read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="cm.bin",form='unformatted')
        write(32) jtemp
        close(32)
      endif

!     SMC 4 layer
      jtemp4 = -999.	!jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_SMC',form='unformatted')
      do l = 1, 4
      read(40)valtempS(l,:,:)
      end do
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
      do l=1,4
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%smc(l)=valtempS(l,c,r)
         jtemp4(l,cc,rr) = valtempS(l,c,r)
      enddo
      enddo
      cc=0
      enddo
      rr=0
      print*,'SMC read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="smc.bin",form='unformatted')
        write(32) jtemp4(1,:,:)
        write(32) jtemp4(2,:,:)
        write(32) jtemp4(3,:,:)
        write(32) jtemp4(4,:,:)
        close(32)
      endif

!     SH2O 4 layer 
      jtemp4 = -999.    !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_SH2O',form='unformatted')
      do l = 1, 4
      read(40)valtempS(l,:,:)
      end do
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
      do l=1,4
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%sh2o(l)=valtempS(l,c,r)
         jtemp4(l,cc,rr) = valtempS(l,c,r)
      enddo
      enddo
      cc=0
      enddo
      rr=0
      print*,'SH2O read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="sh2o.bin",form='unformatted')
        write(32) jtemp4(1,:,:)
        write(32) jtemp4(2,:,:)
        write(32) jtemp4(3,:,:)
        write(32) jtemp4(4,:,:)
        close(32)
      endif

!     STC 4 layer
      jtemp4 = -999.    !jesse 20040429
      open(40,file='./ICS/GDAS2.INIT_STC',form='unformatted')
      do l = 1, 4
      read(40)valtempS(l,:,:)
      end do
      close(40)
      do r=lis%d%gnr,1,-1
      rr=rr+1
      do c=lis%d%gnc,1,-1
      cc=c
      do l=1,4
         if(glbgindex(cc,rr).ne.-1)noah(glbgindex(cc,rr))%stc(l)=valtempS(l,c,r)
         jtemp4(l,cc,rr) = valtempS(l,c,r)
      enddo
      enddo
      cc=0
      enddo
      rr=0
      print*,'STC read of ICS done for # ',lis%d%nch
      if(lis%o%wparam.eq.1) then
        open(32,file="stc.bin",form='unformatted')
        write(32) jtemp4(1,:,:)
        write(32) jtemp4(2,:,:)
        write(32) jtemp4(3,:,:)
        write(32) jtemp4(4,:,:)
        close(32)
      endif

      lis%t%yr=lis%t%syr
      lis%t%mo=lis%t%smo 
      lis%t%da=lis%t%sda
      lis%t%hr=lis%t%shr
      lis%t%mn=lis%t%smn
      lis%t%ss=lis%t%sss

      call date2time(lis%t%time,lis%t%doy,lis%t%gmt,lis%t%yr,&
                     lis%t%mo,lis%t%da,lis%t%hr,lis%t%mn,lis%t%ss) 
      write(*,*)'MSG: noah_coldstart -- ICS for start time ',&
                lis%t%time, ' (', iam, ')'
      write(79,*)'MSG: noah_coldstart -- Using ICS for start time ',&
                 lis%t%time, ' (', iam, ')'
      deallocate(valTEMP)
      deallocate(valTEMPS)
      deallocate(jtemp)
      deallocate(jtemp4)
   endif
!J20041118 TEST COLDSTART
      do t=1,lis%d%nch
!         noah(t)%t1=noahdrv%noah_it
!         noah(t)%cmc=0.0004
!         noah(t)%snowh=0.0
!         noah(t)%sneqv=0.0
!         noah(t)%ch=0.0150022404
!         noah(t)%cm=0.0205970779
!         do l=1,4
!            noah(t)%stc(l)=noahdrv%noah_it
!         enddo
!         noah(t)%smc(1)=0.3252287
!         noah(t)%smc(2)=0.3194746
!         noah(t)%smc(3)=0.3172167
!         noah(t)%smc(4)=0.3078052
!         noah(t)%sh2o(1)=0.1660042
!         noah(t)%sh2o(2)=0.2828006
!         noah(t)%sh2o(3)=0.3172163
!         noah(t)%sh2o(4)=0.3078025
      enddo
!J20041118
!EOC
end subroutine noah_coldstart
