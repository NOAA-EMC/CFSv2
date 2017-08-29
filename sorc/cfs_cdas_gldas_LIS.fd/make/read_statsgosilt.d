read_statsgosilt.o read_statsgosilt.d : read_statsgosilt.F
read_statsgosilt.o : misc.h
read_statsgosilt.o : lisdrv_module.o
read_statsgosilt.o : lis_openfileMod.o
read_statsgosilt.o : lis_indices_module.o
