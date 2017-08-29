read_faosilt.o read_faosilt.d : read_faosilt.F
read_faosilt.o : misc.h
read_faosilt.o : lisdrv_module.o
read_faosilt.o : lis_openfileMod.o
read_faosilt.o : lis_indices_module.o
