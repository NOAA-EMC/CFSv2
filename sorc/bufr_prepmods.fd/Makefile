# makefile (bufr_prepmods) -  4 Feb 2013
# 
#  4 Feb 2013 JWhiting cp'd fr IBM/AIX ver of 22 Mar 2012 (dated 11 Nov 2003)
#              config's for use on WCOSS linux (ifort cmplr, new lib specs)
#                rm'd profiler refs
#                parameterized cmpl debug options (-g, -traceback))
#                set optimization to default (-O2)
#                rm'd unneeded compiler options (list, source, etc)
#                set to use default variable sizes (-i4, -r4)
#                 added developmental cmpl options (CHKOUT)
# ----
#
# Source files & objects
SRCS	= bufr_prepmods.f clonlat.f mnemonics.f obname.f raddate.f ssmipn.f tideg.f
OBJS	= $(SRCS:.f=.o) 


DEBUG = 	-g -traceback
FFLAGS =        -O2 $(DEBUG) $(CHKOUT)
LDFLAGS=

CMD = 		bufr_prepmods


all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f bufr_prepmods

install:
	mv $(CMD) ../../exec/

# -- end of Makefile

