SHELL=/bin/sh

SRC=mppnccombine.c
OBJ=$(SRC:.c=.o)

LIBNCDF=${NCDF}
INCL=${INC}
TARGET=cfs_mppnccombine

.c.o:
	$(CC) $(CFLAG) $(INC) -c $<

$(TARGET): $(OBJ)
	$(CC) $(LDFLAG) -o $(TARGET) $(OBJ) $(NCDF) -lm

clean:
	rm *.o
