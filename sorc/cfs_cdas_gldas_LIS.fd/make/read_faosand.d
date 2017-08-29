read_faosand.o read_faosand.d : read_faosand.F
read_faosand.o : misc.h
read_faosand.o : lisdrv_module.o
read_faosand.o : lis_openfileMod.o
read_faosand.o : lis_indices_module.o
