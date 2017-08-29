SHELL	=/bin/sh
SRCS	=$(NAME).f
OBJS	=$(NAME).o
FC	=$(FCMP)
FFLAGS  =$(FFLAGSM)
LDFLAGS	=   
CMD	=$(NAME)

$(CMD):	$(SRCS) $(OBJS)
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRCs) $(OBJS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS)
clobber:        clean
	-rm -f $(CMD)
