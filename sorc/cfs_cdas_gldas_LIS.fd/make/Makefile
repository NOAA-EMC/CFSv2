# Set up special characters

null  :=
space := $(null) $(null)

# Check for directory in which to put executable
ifeq ($(MODEL_EXEDIR),$(null))
MODEL_EXEDIR :=.
endif

# Check for name of executable
ifeq ($(EXENAME),$(null))
EXENAME := ../cfs_cdas_gldas_LIS
endif

# Check if SPMD is defined in "misc.h"
# Ensure that it is defined and not just "undef SPMD" set in file
ifeq ($(SPMD),$(null))
  SPMDSET := $(shell /bin/grep SPMD misc.h)
  ifneq (,$(findstring define,$(SPMDSET)))
    SPMD := TRUE
  else
    SPMD := FALSE
  endif
endif

# Determine platform
UNAMES := $(shell uname -s)
UMACHINE := $(shell uname -m)

ifeq ($(UNAMES),AIX)

LIB_ESMF   := ../../../../lib
MOD_ESMF   := $(LIB_ESMF)/incmod/esmf_3_1_0rp2
LIB_NETCDF := $(LIB_ESMF)
INC_NETCDF := $(LIB_ESMF)/incmod/netcdf-3.5.0
#LIB_NETCDF := /usrx/local/netcdf
#INC_NETCDF := /usrx/local/netcdf/include
LIB_MPI    := /usr/lpp/ppe.poe/lib
INC_MPI    := /usr/lpp/ppe.poe/include

endif

ifeq ($(UNAMES),Linux)

### Zeus/Gaea
#LIB_ESMF   :=/scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib
#LIB_NETCDF := /scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib/libnetcdf.a
#INC_NETCDF := /scratch2/portfolios/NCEPDEV/global/save/Shrinivas.Moorthi/para/lib/netcdf

### Eddy/Tide
#LIB_ESMF   := /usrx/local/esmf-3.1.0rp2
#LIB_NETCDF := /usrx/local/NetCDF/3.6.3/lib/libnetcdf.a
#INC_NETCDF := /usrx/local/NetCDF/3.6.3/include

#MOD_ESMF   := $(LIB_ESMF)/include
#LIB_MPI    := 
#INC_MPI    :=

endif

# Load dependency search path.
dirs := . $(shell cat Filepath)
# Set cpp search path, include netcdf
cpp_dirs := $(dirs) $(INC_NETCDF) $(INC_MPI)
cpp_path := $(foreach dir,$(cpp_dirs),-I$(dir)) # format for command line

# Expand any tildes in directory names. Change spaces to colons.
VPATH    := $(foreach dir,$(cpp_dirs),$(wildcard $(dir)))
VPATH    := $(subst $(space),:,$(VPATH))

#------------------------------------------------------------------------
# Primary target: build the model
#------------------------------------------------------------------------
all: $(MODEL_EXEDIR)/$(EXENAME)

# Get list of files and determine objects and dependency files
FIND_FILES = $(wildcard $(dir)/*.F $(dir)/*.f $(dir)/*.F90 $(dir)/*.c)
FILES      = $(foreach dir, $(dirs),$(FIND_FILES))
SOURCES   := $(sort $(notdir $(FILES)))
DEPS      := $(addsuffix .d, $(basename $(SOURCES)))
OBJS      := $(addsuffix .o, $(basename $(SOURCES)))
DOCS      := $(addsuffix .tex, $(basename $(SOURCES)))

$(MODEL_EXEDIR)/$(EXENAME): $(OBJS)
	 $(FC) -o $@ $(OBJS) $(FOPTS) $(LDFLAGS)
debug: $(OBJS)
        echo "FFLAGS: $(FFLAGS)"
        echo "LDFLAGS: $(LDFLAGS)"
        echo "OBJS: $(OBJS)"

#***********************************************************************
#********** Architecture-specific flags and rules***********************
#***********************************************************************
# AIX
#----------------------------------------------------------------

ifeq ($(UNAMES),AIX)

ESMF_ARCH = rs6000_64
#FC           = mpxlf90_r
FC           = mpxlf90
CPP          = /lib/cpp
CC           = /usr/lpp/ppe.poe/bin/mpcc_r
#CC           = mpcc
MPI_PATH     = /usr/lpp/ppe.poe

# Library directories
#LIB_DIR     = /nwprod/lib/
LIB_DIR      = ../../../../lib
LIB_DIR2      = ../lib/
HDFLIBDIR    = $(LIB_DIR)
GFIOLIBDIR   = $(LIB_DIR)
INCMOD1 =$(LIB_DIR)/incmod/sigio_4
INCMOD2 =$(LIB_DIR)/incmod/sfcio_4
CPPFLAGS     = -P
PSASINC      =
CFLAGS       = $(cpp_path) -c -w -O -q64 -qcpluscmt
#FFLAGS       = $(cpp_path) -I$(MOD_ESMF)/$(ESMF_ARCH) -I$(MPI_PATH)/include -I $(INCMOD1) -I $(INCMOD2) -O -qstrict -qtune=auto -qarch=auto -qmaxmem=-1 -NS2000 -qnosave -c -q64 -qsource -qxref=full -qattr=full
FFLAGS       = $(cpp_path) -I$(MOD_ESMF) -I$(MPI_PATH)/include -I $(INCMOD1) -I $(INCMOD2) -O -qstrict -qtune=auto -qarch=auto -qmaxmem=-1 -NS2000 -qnosave -c -q64 -qsource -qxref=full -qattr=full
#FOPTS        = $(LIB_DIR)libw3_4.a
#LDFLAGS      = -bmap:map -bloadmap:lm -lmass -L$(LIB_ESMF)/$(ESMF_ARCH) -lesmf -L$(LIB_DIR) -lbacio_4
#LDFLAGS      = -bmap:map -bloadmap:lm -lmass -L$(LIB_ESMF)/$(ESMF_ARCH) -lesmf -L$(LIB_DIR) -lbacio_4 -lip_4 -lw3_4 -lsp_4 -qsmp=noauto -lessl
LDFLAGS      = -bmap:map -bloadmap:lm -lmass -L$(LIB_ESMF) -lesmf_3_1_0rp2 -L$(LIB_DIR) -lbacio_4 -lip_4 -lsp_4 qsmp=nonauto -lessl
ifeq ($(SPMD),TRUE)
#  FFLAGS  -= -mp
#  FFLAGS  += -macro_expand
#  FFLAGS  += -I$(INC_MPI) -macro_expand

#  LDFLAGS += -L$(LIB_MPI) -lmpi
else
  FFLAGS  += -DHIDE_MPI
endif
.F.o:
	$(FC) $(FFLAGS) $<
.F90.o:
	$(FC) $(FFLAGS) $<
.f.o:
	$(FC) $(FFLAGS) $<
.f90.o:
.c.o:
	$(CC) $(cpp_path) $(CFLAGS) $<

endif

#----------------------------------------------------------------
# ZEUS/GAEA/Eddy/Tide
#----------------------------------------------------------------

ifeq ($(UNAMES),Linux)

ESMF_ARCH = rs6000_64

CPP          =
MPI_PATH     = 

# Library directories

### Eddy/Tide
#LIB_DIR      = /nwprod/lib
INCMOD1	=$SIGIO_INC4 
INCMOD2	=$SFCIO_INC4 

CPPFLAGS     = -P
PSASINC      =
CFLAGS       = -DIRIX64 -c -O
FFLAGS       = -free -c -O0 -g -traceback -check all -fpe0 -ftrapuv -CB -convert big_endian
FFLAGS       = -free -c -O0 -g -traceback                               -convert big_endian
LDFLAGS      = $(BACIO_LIB4) $(IP_LIB4) $(W3EMC_LIB4) $(W3NCO_LIB4) $(SP_LIB4) -qopenmp 
ifeq ($(SPMD),TRUE)
#  FFLAGS  -= -mp
#  #  FFLAGS  += -macro_expand
#  FFLAGS  += -I$(INC_MPI) -macro_expand
#
#  LDFLAGS += -L$(LIB_MPI) -lmpi
else
  FFLAGS  += -DHIDE_MPI
endif
.SUFFIXES: .f .f90 .F .F90 .c .o
.F.o:
	$(FC) $(FFLAGS) $<
.F90.o:
	$(FC) $(FFLAGS) $<
.f.o:
	$(FC) $(FFLAGS) $<
.f90.o:
	$(FC) $(FFLAGS) $<
.c.o:
	$(CC) $(cpp_path) $(CFLAGS) $<

endif


RM := rm

clean:
	$(RM) -f *.o *.mod *.stb  $(MODEL_EXEDIR)/$(EXENAME)

realclean:
	$(RM) -f *.o *.d *.mod *.stb  $(MODEL_EXEDIR)/$(EXENAME)
#------------------------------------------------------------------------
#!!!!!!!!!!!!!!!!DO NOT EDIT BELOW THIS LINE.!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#------------------------------------------------------------------------
# These rules cause a dependency file to be generated for each source
# file.  It is assumed that the tool "makdep" (provided with this
# distribution in clm2/tools/makdep) has been built and is available in
# the user's $PATH.  Files contained in the clm2 distribution are the
# only files which are considered in generating each dependency.  The
# following filters are applied to exclude any files which are not in
# the distribution (e.g. system header files like stdio.h).
#
#  1) Remove full paths from dependencies. This means gnumake will not break
#     if new versions of files are created in the directory hierarchy
#     specified by VPATH.
#
#  2) Because of 1) above, remove any file dependencies for files not in the
#     clm2 source distribution.
#
# Finally, add the dependency file as a target of the dependency rules.  This
# is done so that the dependency file will automatically be regenerated
# when necessary.
#
#     i.e. change rule
#       make.o : make.c make.h
#       to:
#       make.o make.d : make.c make.h
#------------------------------------------------------------------------
DEPGEN := ./MAKDEP/makdep -s F
%.d : %.c
	@echo "Building dependency file $@"
	@$(DEPGEN) -f $(cpp_path) $< > $@
%.d : %.f
	@echo "Building dependency file $@"
	@$(DEPGEN) -f $(cpp_path) $< > $@
%.d : %.F90
	@echo "Building dependency file $@"
	@$(DEPGEN) -f $(cpp_path) $<  > $@
%.d : %.F
	@echo "Building dependency file $@"
	@$(DEPGEN) -f $(cpp_path) $< > $@
#
# if goal is clean or realclean then don't include .d files
# without this is a hack, missing dependency files will be created
# and then deleted as part of the cleaning process
#
INCLUDE_DEPS=TRUE
ifeq ($(MAKECMDGOALS), realclean)
 INCLUDE_DEPS=FALSE
endif
ifeq ($(MAKECMDGOALS), clean)
 INCLUDE_DEPS=FALSE
endif

ifeq ($(INCLUDE_DEPS), TRUE)
-include $(DEPS)
endif
