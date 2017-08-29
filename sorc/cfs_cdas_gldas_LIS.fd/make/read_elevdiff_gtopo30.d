read_elevdiff_gtopo30.o read_elevdiff_gtopo30.d : read_elevdiff_gtopo30.F
read_elevdiff_gtopo30.o : misc.h
read_elevdiff_gtopo30.o : lisdrv_module.o
read_elevdiff_gtopo30.o : lis_openfileMod.o
read_elevdiff_gtopo30.o : spmdMod.o
