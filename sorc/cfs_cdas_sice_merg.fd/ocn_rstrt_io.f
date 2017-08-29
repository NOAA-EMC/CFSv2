!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads and writes ocn restart files with "nc" format.           !
!  It contains: read_ocnvar2, write_ocnvar2                                       !
!                                                                              !
!     Xingren Wu   (Xingren.Wu@noaa.gov)                                       !
!     2007-11-05                                                               !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

module ocn_rstrt_mode

      implicit none

contains

      subroutine read_ocnvar2(im,jm,km1,km2,lm,data1,data2,sname1,sname2,file_in,nv)

!
!     (im,jm,km1,lm) and (im,jm,km2,lm) represent (x,y,z,t) for data,
!     sname is the shot name for data that is stored in the file (.e.g. temp),
!     file_in is the file name and nv is the # of variables (1 or 2).
!
      INCLUDE "netcdf.inc"

      integer im,jm,km1,km2,lm,nv
      integer id1,id2,status,ncid

      real*8, dimension(im,jm,km1,lm) :: data1
      real*8, dimension(im,jm,km2,lm) :: data2
      character(len=120) :: file_in,sname1,sname2

      status = nf_open(file_in(1:len_trim(file_in)), NF_NOWRITE, ncid)
      status = nf_inq_varid(ncid, sname1(1:len_trim(sname1)), id1)
      if (nv == 2) status = nf_inq_varid(ncid, sname2(1:len_trim(sname2)), id2)
      status = nf_get_var_double(ncid,id1,data1)
      if (nv == 2) status = nf_get_var_double(ncid,id2,data2)

      status=nf_close(ncid)

      return
      end subroutine read_ocnvar2

      subroutine write_ocnvar2(im,jm,km1,km2,lm,data1,data2,sname1,sname2,file_out,title,nv,nz)

!
!     (im,jm,km1,lm) and (im,jm,km2,lm) represent (x,y,z,t) for data,
!     sname is the shot name for data that will be saved in the file (.e.g. temp),
!     file_out is the file name, nv is the # of variables (1 or 2),
!     and nz is the # of z axis (e.g. 2, one for km=1 level and the other for km=40).

      INCLUDE "netcdf.inc"

      integer i,im,jm,km1,km2,lm,nv,nz
      integer id1,id2,status,ncid
      integer :: xaxis_1_dim, yaxis_1_dim,zaxis_1_dim,zaxis_2_dim,Time_dim
      integer :: id_xaxis_1,id_yaxis_1,id_zaxis_1,id_zaxis_2,id_Time
      integer, dimension(4) :: vardim_1,vardim_2,startvar,countvar_1,countvar_2
      real    xaxis_1(im),yaxis_1(jm),zaxis_1(km1),zaxis_2(km2)
      real*8  Time(lm)
      real*8, dimension(im,jm,km1,lm) :: data1
      real*8, dimension(im,jm,km2,lm) :: data2
      character(len=120) :: file_out,sname1,sname2,title

      id_xaxis_1=1
      xaxis_1_dim=1
      id_yaxis_1=2
      yaxis_1_dim=2
      id_zaxis_1=3
      zaxis_1_dim=3

      if (nz == 2) then
         id_zaxis_2=4
         zaxis_2_dim=4
         id_Time=5
         Time_dim=5
      else
         id_Time=4
         Time_dim=4
      endif

      id1=id_Time+1
      id2=id1+1

      do i=1,im
         xaxis_1(i)=real(i)
      enddo

      do i=1,jm
         yaxis_1(i)=real(i)
      enddo

      do i=1,km1
         zaxis_1(i)=real(i)
      enddo

      do i=1,km2
         zaxis_2(i)=real(i)
      enddo

      do i=1,lm
         Time(i)=real(i)
      enddo


      status = nf_create(file_out(1:len_trim(file_out)),nf_write,ncid)
      status = nf_def_dim(ncid,'xaxis_1', im , xaxis_1_dim)
      status = nf_def_dim(ncid,'yaxis_1', jm , yaxis_1_dim)
      status = nf_def_dim(ncid,'zaxis_1', km1, zaxis_1_dim)
      if (nz == 2) status = nf_def_dim(ncid,'zaxis_2', km2, zaxis_2_dim)
      status = nf_def_dim(ncid,'Time', nf_unlimited, Time_dim)

      status = nf_def_var(ncid,'xaxis_1', nf_float, 1,xaxis_1_dim,id_xaxis_1)
      status = nf_def_var(ncid,'yaxis_1', nf_float, 1,yaxis_1_dim,id_yaxis_1)
      status = nf_def_var(ncid,'zaxis_1', nf_float, 1,zaxis_1_dim,id_zaxis_1)
      if (nz == 2) status = nf_def_var(ncid,'zaxis_2', nf_float, 1,zaxis_2_dim,id_zaxis_2)
      status = nf_def_var(ncid,'Time',    nf_double,1,Time_dim,      id_Time)

      vardim_1= (/xaxis_1_dim,yaxis_1_dim,zaxis_1_dim,Time_dim/)
      status = nf_def_var(ncid,sname1(1:len_trim(sname1)),nf_double,4,vardim_1,id1)

      if (nz == 2) then
         vardim_2= (/xaxis_1_dim,yaxis_1_dim,zaxis_2_dim,Time_dim/)
      else
         vardim_2= (/xaxis_1_dim,yaxis_1_dim,zaxis_1_dim,Time_dim/)
      endif
      status = nf_def_var(ncid,sname2(1:len_trim(sname2)),nf_double,4,vardim_2,id2)

      status = nf_put_att_text(ncid, nf_global, 'filename',len_trim(title),title)

      status = nf_put_att_text(ncid,id_xaxis_1,'long_name',7,'xaxis_1')
      status = nf_put_att_text(ncid,id_xaxis_1,'units',4,'none')
      status = nf_put_att_text(ncid,id_xaxis_1,'cartesian_axis',1,'X')

      status = nf_put_att_text(ncid,id_yaxis_1,'long_name',7,'yaxis_1')
      status = nf_put_att_text(ncid,id_yaxis_1,'units',4,'none')
      status = nf_put_att_text(ncid,id_yaxis_1,'cartesian_axis',1,'Y')

      status = nf_put_att_text(ncid,id_zaxis_1,'long_name',7,'zaxis_1')
      status = nf_put_att_text(ncid,id_zaxis_1,'units',4,'none')
      status = nf_put_att_text(ncid,id_zaxis_1,'cartesian_axis',1,'Z')

      if (nz == 2) then
         status = nf_put_att_text(ncid,id_zaxis_2,'long_name',7,'zaxis_2')
         status = nf_put_att_text(ncid,id_zaxis_2,'units',4,'none')
         status = nf_put_att_text(ncid,id_zaxis_2,'cartesian_axis',1,'Z')
      endif

      status = nf_put_att_text(ncid,id_Time,'long_name',4, 'Time')
      status = nf_put_att_text(ncid,id_Time,'units',10,'time level')
      status = nf_put_att_text(ncid,id_Time,'cartesian_axis',1,'T')

      status = nf_put_att_text(ncid,id1,'long_name',len_trim(sname1),sname1(1:len_trim(sname1)))
      status = nf_put_att_text(ncid,id1,'units',4, 'none')

      if (nv == 2) then
         status = nf_put_att_text(ncid,id2,'long_name',len_trim(sname2),sname2(1:len_trim(sname2)))
         status = nf_put_att_text(ncid,id2,'units',4,'none')
      endif

      status = nf_enddef(ncid)

      status = nf_put_vara_real(ncid,id_xaxis_1,1,im,xaxis_1)
      status = nf_put_vara_real(ncid,id_yaxis_1,1,jm,yaxis_1)
      status = nf_put_vara_real(ncid,id_zaxis_1,1,km1,zaxis_1)
      if (nz == 2) status = nf_put_vara_real(ncid,id_zaxis_2,1,km2,zaxis_2)
      status = nf_put_vara_double(ncid,id_Time,1,lm,Time)

      startvar = (/1,1,1,1/)
      countvar_1 = (/im,jm,km1,lm/)
      if (nv == 2) countvar_2 = (/im,jm,km2,lm/)
      status = nf_put_vara_double(ncid,id1,startvar,countvar_1,data1)
      if (nv == 2) status = nf_put_vara_double(ncid,id2,startvar,countvar_2,data2)

      status=nf_close(ncid)

      return
      end subroutine write_ocnvar2

      END module ocn_rstrt_mode
