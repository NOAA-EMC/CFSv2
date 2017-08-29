# Template for WCOSS
#typical use with netCDF:
# mkmf -t template.ibm -c"-Duse_libMPI -Duse_netCDF" path_names /usr/local/include

FC = ftn    
LD = ftn    
CC = cc

FFLAGS= -real-size 64 -integer-size 32 -O0 -align dcommons -convert big_endian -traceback $(INCL)
CFLAGS = -O3 $(INCL)
LDFLAGS = $(LIBS)

##LIBS=-L/usrx/local/NetCDF/3.6.3/lib -lnetcdf
##INCL=-I/usrx/local/NetCDF/3.6.3/include

LIBS=$(NETCDF_LDFLAGS)
INCL=$(NETCDF_INCLUDE)

