#######################################
# Makefile for pentad2gldas
# Use:
# make - build the executable
# make clean - start with a clean slate
#######################################
# Define the name of the executable
TARGET = cfs_cdas_pentad2gldas
# CPP, Compiler, and Linker Options 
OPTS     =${FFLAGSM}
LIBS     =${ESSL} ${LIBSM}

OBJS = pentad2gldas.o \
	  num_parthds.o \
          circle.o \
          getgdas.o \
          getpentad.o \
          input.o \
          outgrib.o \
          output.o 

.f.o:
	$(FC) -c $<

$(TARGET):	$(OBJS)
	$(FC) -o $(TARGET) $(OBJS) $(OPTS) $(LIBS) -openmp

clean:
	/bin/rm -f $(TARGET) *.lst *.o *.mod
