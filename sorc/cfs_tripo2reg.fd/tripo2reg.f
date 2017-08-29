       program tripo2reg_mom4ice

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program read mom4ice daily ocean and ice data                          !
!  (yyyymmdd.ocean_day.nc & yyyymmdd.ice_day.nc) and interpolate the data      !
!  on to regular grid (e.g. 2x1).                                              !
!                                                                              !
!  Xingren Wu   (Xingren.Wu@noaa.gov)                                          !
!     April 20, 2007                                                           !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

       use tripo_intp_mod, only: tripo

       implicit none

       integer idbug,iyr,imth,iday,ihr,im,jm,km,imo,jmo,imos,jmtp,jmtpo
       integer mfh,mfhout,nreg,mkmoc,mfcstcpl,igenocnp
       real*4  dlat,dlon,flats,flatn,flonw,flone,tripolat
       CHARACTER*1  kdbug,kmkmoc,kmfcstcpl
       CHARACTER*120 icefile,ocnfile,outfile,mocfile
       CHARACTER*10 kyr,kmth,kday,khr
       CHARACTER*10 kfh,kfhout
       CHARACTER*10 kim,kjm,kkm,kjmtp,kimos,knreg
       CHARACTER*10 kimo,kjmo,kjmtpo
       CHARACTER*10 klats,klatn,klonw,klone,ktripolat,kigenocnp

       call start()
       call getenv("idbug",kdbug)
       read(kdbug,'(i1)') idbug
       write(*,*) "idbug= ",idbug
!
       call getenv("mkmoc",kmkmoc)
       read(kmkmoc,'(i1)') mkmoc
       write(*,*) "mkmoc= ",mkmoc
!
       call getenv("mfcstcpl",kmfcstcpl)
       read(kmfcstcpl,'(i1)') mfcstcpl
       write(*,*) "mfcstcpl= ",mfcstcpl
!
       call getenv("IGEN_OCNP",kigenocnp)
       read(kigenocnp,'(i3)') igenocnp
       write(*,*) "IGEN_OCNP= ",igenocnp
!
       call getenv("icefile",icefile)
       write(*,*) "icefile= ",icefile
!
       call getenv("ocnfile",ocnfile)
       write(*,*) "ocnfile= ",ocnfile
!
       call getenv("outfile",outfile)
       write(*,*) "outfile= ",outfile
!
       call getenv("mocfile",mocfile)
       write(*,*) "mocfile= ",mocfile
!
       call getenv("year",kyr)
       read(kyr,'(I4)') iyr
       write(*,*) "year= ",iyr
!
       call getenv("month",kmth)
       read(kmth,'(I2)') imth
       write(*,*) "month= ",imth
!
       call getenv("day",kday)
       read(kday,'(I2)') iday
       write(*,*) "day= ",iday
!
       call getenv("hour",khr)
       read(khr,'(I2)') ihr
       write(*,*) "hour= ",ihr
!
       call getenv("fh",kfh)
       read(kfh,'(I10)') mfh
       write(*,*) "fh= ",mfh
!
       call getenv("hh_inc_ocn",kfhout)
       read(kfhout,'(I10)') mfhout
       write(*,*) "hh_inc_ocn= ",mfhout
!
       call getenv("im",kim)
       read(kim,'(I10)') im
       write(*,*) "im= ",im
!
       call getenv("jm",kjm)
       read(kjm,'(I10)') jm
       write(*,*) "jm= ",jm
!
       call getenv("imo",kimo)
       read(kimo,'(I10)') imo
       write(*,*) "imo= ",imo
!
       call getenv("jmo",kjmo)
       read(kjmo,'(I10)') jmo
       write(*,*) "jmo= ",jmo
!
       call getenv("jmtp",kjmtp)
       read(kjmtp,'(I10)') jmtp
       write(*,*) "jmtp= ",jmtp
!
       call getenv("jmtpo",kjmtpo)
       read(kjmtpo,'(I10)') jmtpo
       write(*,*) "jmtpo= ",jmtpo
!
       call getenv("imos",kimos)
       read(kimos,'(I10)') imos
       write(*,*) "imos= ",imos
!
       call getenv("km",kkm)
       read(kkm,'(I10)') km
       write(*,*) "km= ",km
!
       call getenv("flats",klats)
       read(klats,'(f8.1)') flats
       write(*,*) "flats= ",flats
!
       call getenv("flatn",klatn)
       read(klatn,'(f8.1)') flatn
       write(*,*) "flatn= ",flatn
!
       call getenv("flonw",klonw)
       read(klonw,'(f8.1)') flonw
       write(*,*) "flonw= ",flonw
!
       call getenv("flone",klone)
       read(klone,'(f8.1)') flone
       write(*,*) "flone= ",flone
!
       call getenv("nreg",knreg)
       read(knreg,'(i10)') nreg
       write(*,*) "nreg= ",nreg
!
       call getenv("tripolat",ktripolat)
       read(ktripolat,'(f8.1)') tripolat
       write(*,*) "tripolat= ",tripolat
!
       dlat = (flatn-flats) / (jmo-1)
!
       dlon = (flone-flonw) / (imo-1)
!
       call tripo(idbug,im,jm,km,icefile,ocnfile,iyr,imth,iday,ihr, &
       mfh,mfhout,flonw,flone,dlon,flatn,flats,dlat,jmtp,imos, &
       imo,jmo,jmtpo,mkmoc,nreg,tripolat,outfile,mocfile,mfcstcpl,igenocnp)
!
       call summary()
       call errexit(0)
       stop
       end

