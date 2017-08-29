#-----------------------------------------------------------------------
# This Makefile is for building dependency generator "makdep" on a CRAY, 
# SGI or SUN.  The executable will be put in $(HOME)/bin.  If this directory
# does not exist or is not in $PATH, problems will occur when the CCM is built.
#------------------------------------------------------------------------

EXEDIR  := .
EXENAME := makdep

#------------------------------------------------------------------------
# Linux
#------------------------------------------------------------------------

CFLAGS := -O

#------------------------------------------------------------------------
# Default rules
#------------------------------------------------------------------------

#CC = /usr/vacpp/bin/cc_r
CC = icc
$(EXEDIR)/$(EXENAME):	main.o
	$(CC) -o $@ main.o 

RM := rm

clean:
	$(RM) -f *.o $(EXEDIR)/$(EXENAME)
