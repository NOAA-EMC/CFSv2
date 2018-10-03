SHELL=  /bin/sh
ISIZE = 4
RSIZE = 8
COMP=   ifort -nofree
LIBS=   $(W3EMC_LIBd) $(W3NCO_LIBd) $(BACIO_LIB4)  
LDFLAGS=  
#FFLAGS= -O2 -xHost -i$(ISIZE) -real-size 32
FFLAGS= -O2 -g -traceback -check bounds -check format -xHost -fpe0 -i$(ISIZE) -r$(RSIZE)    
FFLAGS= -O2 -g -traceback                                          -i$(ISIZE) -r$(RSIZE)    

gettrk:      gettrk_main.f gettrk_modules.o
	@echo " "
	@echo "  Compiling the tracking program....."
	$(COMP) $(FFLAGS) $(LDFLAGS) gettrk_modules.o gettrk_main.f $(LIBS) -o gettrk
	@echo " "
	rm -f *.o *.mod

gettrk_modules.o:    gettrk_modules.f
	@echo " "
	@echo "  Compiling the modules....."
	$(COMP) $(FFLAGS) -c gettrk_modules.f -o gettrk_modules.o
	@echo " "

CMD=	gettrk

clean:
	-rm -f  *.o  *.mod

install:
	mv $(CMD) ../../exec/$(CMD)  

