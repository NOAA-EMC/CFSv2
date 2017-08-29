SHELL=/bin/sh
#######################################
# Makefile for swapPentad
# Use:
# make - build the executable
# make clean - start with a clean slate
#######################################
# Define the name of the executable
TARGET = cfs_cdas_swapPentad
# CPP, Compiler, and Linker Options 
OPTS     =  ${FFLAGSM}
W3LIBDIR = 
LIBS     = 

OBJS = swapPentad.o 

.f.o:
	$(FC) -c $<

$(TARGET):	$(OBJS)
	$(FC) -o $(TARGET) $(OBJS) $(OPTS) $(LIBS)

clean:
	/bin/rm -f $(TARGET) *.lst *.o *.mod
