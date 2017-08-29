      program overdate
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: OVERDATE     REPLACE DATE ON A GRIB FILE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-01-01
C
C ABSTRACT: THIS PROGRAM READS AN ENTIRE GRIB FILE FROM UNIT 11
C   AND WRITES IT BACK OUT TO UNIT 51, REPLACING THE INTERNAL
C   DATE FROM A DATE READ IN FROM UNIT 5 IN YYYYMMDDHH FORMAT.
C
C PROGRAM HISTORY LOG:
C   1998-01-01  IREDELL
C   1999-05-24  Gilbert     - added calls to BAOPEN.
C
C INPUT FILES:
C   UNIT    5    10-DIGIT DATE IN YYYYMMDDHH FORMAT
C   UNIT   11    INPUT GRIB FILE = "fort.11"
C
C OUTPUT FILES:
C   UNIT   51    OUTPUT GRIB FILE = "fort.51"
C
C SUBPROGRAMS CALLED:
C   SKGB     - Find next grib record
C   BAREAD   - Read GRIB record
C   WRYTE    - Write GRIB record
C
C REMARKS:
C   ANY NON-GRIB INFORMATION IN THE INPUT GRIB FILE WILL BE LOST.
C   AN OUTPUT LINE WILL BE WRITTEN FOR EACH GRIB MESSAGE COPIED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      parameter(msk1=32000,msk2=4000,mgrib=16000000)
      character cgrib(mgrib)
      call baopenr(11,"fort.11",iret1)
      call baopenw(51,"fort.51",iret2)
      read *,idate   ! input in yyyymmddhh form
      i4=idate/1000000
      im=(idate-i4*1000000)/10000
      id=(idate-i4*1000000-im*10000)/100
      ih=(idate-i4*1000000-im*10000-id*100)
      ic=(i4-1)/100+1
      iy=(i4-(ic-1)*100)
      n=0
      iseek=0
      call skgb(11,iseek,msk1,lskip,lgrib)
      dowhile(lgrib.gt.0.and.lgrib.le.mgrib)
        call baread(11,lskip,lgrib,ngrib,cgrib)
        if(ngrib.ne.lgrib) call exit(2)
        n=n+1
        ic0=mova2i(cgrib(8+25))
        iy0=mova2i(cgrib(8+13))
        im0=mova2i(cgrib(8+14))
        id0=mova2i(cgrib(8+15))
        ih0=mova2i(cgrib(8+16))
        cgrib(8+25)=char(ic)
        cgrib(8+13)=char(iy)
        cgrib(8+14)=char(im)
        cgrib(8+15)=char(id)
        cgrib(8+16)=char(ih)
        call wryte(51,lgrib,cgrib)
        print '("msg",i6,4x,"len",i8,4x,"was",5i4.2,4x,"now",5i4.2)',
     &   n,lgrib,ic0,iy0,im0,id0,ih0,ic,iy,im,id,ih
        iseek=lskip+lgrib
        call skgb(11,iseek,msk2,lskip,lgrib)
      enddo
      end
