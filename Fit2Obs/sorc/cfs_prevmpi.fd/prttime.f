c-----------------------------------------------------------------------------------
c-----------------------------------------------------------------------------------
      subroutine prttime(title)
      character(*) title
      data t0/0.00/
      data t1/0.00/
      save t0,t1
      if(t0==0.00) t0=secnds(t0)
      print1,title,secnds(t0)-t1,secnds(t0)
      t1=secnds(t0)
1     format('time>>>',a8,3(2x,f8.2))
      return
      end

