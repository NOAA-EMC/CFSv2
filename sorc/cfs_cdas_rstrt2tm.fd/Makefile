SHELL=/bin/sh
######################################################################
SRC=rstrt2tm.c
OBJ=$(SRC:.c=.o)
CFLAG=${CFLAGSM}
LDFLAG=${LDFLAGSM}
TARGET=cfs_cdas_rstrt2tm

$(TARGET): $(OBJ)
	$(CC) $(LDFLAG) -o $(TARGET) $(OBJ) $(NCDF)

.c.o:
	$(CC) $(CFLAG) $(INC) -c $<

clean:
	rm -rf *.o
