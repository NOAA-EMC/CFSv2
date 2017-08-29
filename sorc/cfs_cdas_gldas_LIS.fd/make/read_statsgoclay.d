read_statsgoclay.o read_statsgoclay.d : read_statsgoclay.F
read_statsgoclay.o : misc.h
read_statsgoclay.o : lisdrv_module.o
read_statsgoclay.o : lis_openfileMod.o
read_statsgoclay.o : lis_indices_module.o
