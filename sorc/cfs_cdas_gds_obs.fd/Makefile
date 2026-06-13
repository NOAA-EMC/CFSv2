#!/bin/bash
#
short_name=cmbDysPrf4
long_name=cfs_cdas_cmbDysPrf4
short_sname=cmbDysPrfs4
long_sname=cfs_cdas_cmbDysPrfs4
short_ename=mkEvNc4r
long_ename=cfs_cdas_mkEvNc4r

export LIBS=$(W3EMC_LIB4) $(W3NCO_LIB4) $(BUFR_LIB4) $(BACIO_LIB4) 
export NCDF=$(NETCDF_LDFLAGS) 
export INC=$(NETCDF_INCLUDE) 

export FFLAGS=-O3 -free -check bounds -convert big_endian -g -traceback

all: $(long_name) $(long_sname) $(long_ename)
$(short_name).o: $(short_name).f
	$(FC) $(FFLAGS) $(INC) -c $(short_name).f

OBJ = $(short_name).o

$(long_name): $(OBJ)
	$(FC) -o $(long_name) $(OBJ) $(NCDF) $(LIBS)

$(short_sname).o: $(short_sname).f
	$(FC) $(FFLAGS) $(INC) -c $(short_sname).f

OBJS = $(short_sname).o

$(long_sname): $(OBJS)
	$(FC) -o $(long_sname) $(OBJS) $(NCDF) $(LIBS)

$(short_ename).o: $(short_ename).f
	$(FC) $(FFLAGS) $(INC) -c $(short_ename).f

OBJE = $(short_ename).o

$(long_ename): $(OBJE)
	$(FC) -o $(long_ename) $(OBJE) $(NCDF) $(LIBS)

clean:
	rm *.o
