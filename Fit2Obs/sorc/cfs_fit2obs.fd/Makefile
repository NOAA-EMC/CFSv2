SHELL	=	/bin/sh
SRCS	=	$(NAME).f
OBJS	=	$(NAME).o
FC	=	$(FCMP)
CMD	=	$(NAME)
FFLAGS  =	$(FLAGS) 
LDFLAGS	=   

$(CMD):	$(SRCS) 
	$(FC) $(FFLAGS) $(LDFLAGS) $(SRCS) $(LIBS) -o $(CMD)
clean:
	-rm -f $(OBJS)
clobber:        clean
	-rm -f $(CMD)
