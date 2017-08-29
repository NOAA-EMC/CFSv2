#!/bin/sh

SUF = cfs_cdas_godas_

all: $(SUF)mk1DySst4i $(SUF)mk1DySss4i $(SUF)mkDlyTclm
TmUtil.o : TmUtil.c
	$(CC) $(CFLAGS) -c TmUtil.c

mk1DySst4i.o : mk1DySst4i.c
	$(CC) $(CFLAGS) $(INC) -c mk1DySst4i.c

OBJT = TmUtil.o mk1DySst4i.o

$(SUF)mk1DySst4i : $(OBJT)
	$(CC) -o $(SUF)mk1DySst4i $(OBJT) $(NCDF)

mk1DySss4i.o : mk1DySss4i.c
	$(CC) $(CFLAGS) $(INC) -c mk1DySss4i.c

OBJS = TmUtil.o mk1DySss4i.o

$(SUF)mk1DySss4i : $(OBJS)
	$(CC) -o $(SUF)mk1DySss4i $(OBJS) $(NCDF)

mkDlyTclm.o : mkDlyTclm.c
	$(CC) $(CFLAGS) $(INC) -c mkDlyTclm.c

OBJR = TmUtil.o mkDlyTclm.o

$(SUF)mkDlyTclm : $(OBJR)
	$(CC) -o $(SUF)mkDlyTclm $(OBJS) $(NCDF)

clean:
	rm -f *.o

