SHELL=/bin/sh

LIB=${NCDF}
INCL=${INC}

CFLAGS=${CFLAGSM}

PROG=cfs_

all: $(PROG)sstavg
$(PROG)sstavg.o: $(PROG)sstavg.c
	$(CC) $(CFLAGS) $(INCL) -c $(PROG)sstavg.c

$(PROG)sstavg: $(PROG)sstavg.o
	$(CC) -o $(PROG)sstavg $(PROG)sstavg.o $(LIB)

clean:
	rm *.o

