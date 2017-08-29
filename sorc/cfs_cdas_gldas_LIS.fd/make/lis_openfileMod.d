lis_openfileMod.o lis_openfileMod.d : lis_openfileMod.F
lis_openfileMod.o : lisdrv_module.o
lis_openfileMod.o : lis_indices_module.o
lis_openfileMod.o : opendap_module.o
lis_openfileMod.o : agrmetopendap_module.o
