read_statsgosand.o read_statsgosand.d : read_statsgosand.F
read_statsgosand.o : misc.h
read_statsgosand.o : lisdrv_module.o
read_statsgosand.o : lis_openfileMod.o
read_statsgosand.o : lis_indices_module.o
