      program rembfr

      character*8 subset,remset

      read(5,'(a)') remset

      call openbf(20,'IN',20)
      call openbf(50,'OUT',20)
      call openbf(51,'OUT',20)

1     do while(ireadmg(20,subset,idate).eq.0)
      if(subset.ne.remset) then    
         call copymg(20,50)
      else
         call copymg(20,51)
      endif
      enddo

      stop
      end
