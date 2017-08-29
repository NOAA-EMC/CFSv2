SHELL=/bin/sh
#######################################
# Makefile for precipLIS
# Use:
# make - build the executable
# make clean - start with a clean slate
#######################################
# Define the name of the executable
TARGET = cfs_cdas_precipLIS
# CPP, Compiler, and Linker Options 
OPTS     =${FFLAGSM}
LIBS     =${LIBSM}

SOURCE = precipLIS.f90

OBJS = precipLIS.o 

$(TARGET):	$(OBJS)
	$(FC) -o $(TARGET) $(OBJS) $(OPTS) $(LIBS)

$(OBJS):	$(SOURCE)
	$(FC) -c $(SOURCE)

clean:
	/bin/rm -f $(TARGET) *.lst *.o *.mod
