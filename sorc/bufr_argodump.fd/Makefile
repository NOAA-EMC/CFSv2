# ###########################################################################
# makefile (bufr_argodump) -  23 May 2018 
# 
#                use to convert argo type 031005 to tesac format 031002
# ----
#
# Source files & objects
#
SRCS	= bufr_argodump.f  
OBJS	= bufr_argodump.o


FC =            ifort
LIBS=           -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_v10.2.3_4_64
#CHKOUT = 	-ftrapuv  -check all  -fp-stack-check  -fstack-protector
DEBUG = 	-g -traceback
FFLAGS =        -O2 $(DEBUG) $(CHKOUT) -free
LDFLAGS=

CMD = 		bufr_argodump

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(CMD) $(SRCS) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)    

install:
	mv $(CMD) ../../exec/

# -- end of Makefile

