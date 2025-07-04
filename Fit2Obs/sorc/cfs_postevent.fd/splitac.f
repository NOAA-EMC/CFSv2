      program splitac

      character(80) hdstr,obstr,qmstr,xystr
      character(8)  subset
      real(8)       typ,hdr(10),obs(10,255),qms(10,255),xyt(10,255)

      data lunin /20/
      data lunot /50/

      hdstr = 'SID XOB YOB ELV DHR TYP                        '
      obstr = 'POB QOB TOB ZOB UOB VOB CAT                    '
      qmstr = 'PQM QQM TQM ZQM WQM                            '
      xystr = 'XDR YDR HRDR                                   '

      call openbf(lunin,'IN ',lunin)
      call openbf(lunot,'OUT',lunin)

      do while(ireadmg(lunin,subset,idate)==0)
      do while(ireadsb(lunin)==0)

      call ufbint(lunin,hdr,10,1,iret,hdstr)   
      ityp=mod(nint(hdr(6)),100)

      ! do the mass part
      call ufbint(lunin,obs,10,255,nlev,'POB QOB TOB ZOB NUL NUL CAT') ! read the observation elements
      call ufbint(lunin,qms,10,255,iret,'PQM QQM TQM ZQM NUL        ') ! read the current quality marks
      call ufbint(lunin,xyt,10,255,iret,'XDR YDR HRDR               ') ! read balloon drift coordinates
      do n=1,nlev
      hdr(2)=xyt(1,n) ! reflect location in profile 
      hdr(3)=xyt(2,n)
      hdr(5)=xyt(3,n)
      hdr(6)=100+ityp
      call openmb(lunot,subset,idate)
      call ufbint(lunot,hdr,10,1,iret,hdstr)   
      call ufbint(lunot,obs(1,n),10,1,iret,obstr) 
      call ufbint(lunot,qms(1,n),10,1,iret,qmstr) 
      call writsb(lunot)
      enddo

      ! do the wind part
      call ufbint(lunin,obs,10,255,nlev,'POB NUL NUL ZOB UOB VOB CAT') ! read the observation elements
      call ufbint(lunin,qms,10,255,iret,'PQM NUL NUL ZQM WQM        ') ! read the current quality marks
      call ufbint(lunin,xyt,10,255,iret,'XDR YDR HRDR               ') ! read balloon drift coordinates
      do n=1,nlev
      hdr(2)=xyt(1,n) ! reflect location in profile 
      hdr(3)=xyt(2,n)
      hdr(5)=xyt(3,n)
      hdr(6)=200+ityp
      call openmb(lunot,subset,idate)
      call ufbint(lunot,hdr,10,1,iret,hdstr)   
      call ufbint(lunot,obs(1,n),10,1,iret,obstr) 
      call ufbint(lunot,qms(1,n),10,1,iret,qmstr) 
      call writsb(lunot)
      enddo

      enddo
      enddo

      call closbf(lunot)

      stop
      end
