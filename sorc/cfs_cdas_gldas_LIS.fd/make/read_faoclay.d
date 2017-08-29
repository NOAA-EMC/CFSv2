read_faoclay.o read_faoclay.d : read_faoclay.F
read_faoclay.o : misc.h
read_faoclay.o : lisdrv_module.o
read_faoclay.o : lis_openfileMod.o
read_faoclay.o : lis_indices_module.o
